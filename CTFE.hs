{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module CTFE where

import AST
import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Map
import Data.Monoid
import Data.Scientific
import Parser (parseExpr)

data CollectEnv = CollectEnv
    { _collectFuncs :: Map Ident FFuncDecl
    , _collectPragmaMsgs :: [FExpr]
    }
makeLenses ''CollectEnv

data DRVal
    = DNum Scientific
    | DBool Bool
    | DString ByteString

data DVal
    = Func FFuncDecl
    | RValue DRVal
    | LValue (IORef DRVal)
    | Void

data Scope = Scope
    { _scopeCTParams :: Map Ident (IORef DRVal)
    , _scopeRTParams :: Map Ident (IORef DRVal)
    }
makeLenses ''Scope

type Interpret = ReaderT Scope (StateT (Map Ident FFuncDecl) IO)

showDRVal :: DRVal -> String
showDRVal (DNum v) = show v
showDRVal (DBool b) = show b
showDRVal (DString s) = show s

showDVal :: MonadIO m => DVal -> m String
showDVal (RValue v) = pure (showDRVal v)
showDVal (LValue ref) = liftIO (readIORef ref) <&> showDRVal
showDVal (Func f) = pure (show f)
showDVal Void = pure "void"

interpret :: FModule -> IO ()
interpret (Module _ decls) =
    do
        let CollectEnv funcs pragmaMsgs =
                mapM_ collectDecl decls `execState` CollectEnv mempty []
        _ <- mapM_ (\expr -> interpretExpr expr >>= showDVal >>= liftIO . putStrLn) (reverse pragmaMsgs)
            & (`runReaderT` Scope mempty mempty)
            & (`execStateT` funcs)
        pure ()

collectDecl :: FDecl -> State CollectEnv ()
collectDecl (DeclFunc func)      = collectFuncs . at (funcIdent func) ?= func
collectDecl (DeclPragmaMsg expr) = collectPragmaMsgs %= (expr:)

interpretStmt :: FStmt -> ExceptT DVal Interpret ()
interpretStmt (StmtExpr val) = interpretExpr val & void & lift
interpretStmt (StmtIf cond t mf) = do
    RValue (DBool b) <- interpretExpr cond & lift
    if b then interpretStmt t else traverse_ interpretStmt mf
interpretStmt (StmtRet val) = lift (interpretExpr val) >>= throwError
interpretStmt (StmtBlock stmts) = interpretStmts stmts

interpretStmts :: [FStmt] -> ExceptT DVal Interpret ()
interpretStmts [] = pure ()
interpretStmts [x] = interpretStmt x
interpretStmts (x:xs) = interpretStmt x >> interpretStmts xs

-- Monad for "fail"
match :: Monad m => String -> (a -> b -> m c) -> [a] -> [b] -> m [c]
match sndName f =
    go
    where
        go [] [] = pure []
        go (x:xs) (y:ys) = (:) <$> f x y <*> go xs ys
        go [] (_:_) = fail ("Too many " ++ sndName)
        go (_:_) [] = fail ("Too few " ++ sndName)

getDRVal :: MonadIO m => String -> DVal -> m DRVal
getDRVal msg =
    \case
    LValue ref -> liftIO (readIORef ref)
    RValue val -> pure val
    x -> showDVal x >>= fail . (msg ++)

newParamFromArg :: MonadIO m => Param -> DVal -> m (Ident, IORef DRVal)
newParamFromArg (Param _type name) val =
    getDRVal "Invalid parameter: " val >>= liftIO . newIORef <&> (,) name

withScope ::
    (MonadReader Scope f, MonadIO f) =>
    ([Param], [DVal]) -> ([Param], [DVal]) ->
    f a ->
    f a
withScope (ctParams, ctArgs) (rtParams, rtArgs) act =
    do
        newCTScope <- m "compile" ctParams ctArgs
        newRTScope <- m "run"     rtParams rtArgs
        local (\(Scope ct rt) -> Scope (newCTScope <> ct) (newRTScope <> rt)) act
    where
        m prefix ps as =
            match (prefix ++ "-time argument list") newParamFromArg ps as
            <&> fromList

num2 ::
    Monad f =>
    String -> (Scientific -> Scientific -> Scientific) ->
    DRVal -> DRVal -> f DRVal
num2 _ f (DNum x) (DNum y) = DNum (f x y) & pure
num2 msg _ _ _ = fail ("Cannot " ++ msg ++ " non-numbers)")

str2 ::
    Monad f =>
    String -> (ByteString -> ByteString -> ByteString) ->
    DRVal -> DRVal -> f DRVal
str2 _ f (DString x) (DString y) = DString (f x y) & pure
str2 msg _ _ _ = fail ("Cannot " ++ msg ++ " non-strings)")

funcOp :: Monad f => InfixOp -> DRVal -> DRVal -> f DRVal
funcOp InfixAdd = num2 "add" (+)
funcOp InfixSub = num2 "subtract" (-)
funcOp InfixMul = num2 "multiply" (*)
funcOp InfixConcat = str2 "concat" (<>)

interpretInfix :: InfixOp -> DVal -> DVal -> Interpret DVal
interpretInfix iop l r =
    funcOp iop <$> getDRVal msg l <*> getDRVal msg r
    & join
    <&> RValue
    where
        msg = "Cannot " ++ show iop ++ " from"

interpretExpr :: FExpr -> Interpret DVal
interpretExpr (FExpr e) =
    case e of
    ExprVar var ->
        (<|>)
        <$> view (scopeRTParams . at var)
        <*> view (scopeCTParams . at var)
        >>= \case
        Nothing ->
            use (at var)
            >>= \case
            Nothing -> do
                ct <- view (scopeCTParams . at var)
                fail ("Undefined variable: " ++ show var ++ " not in " ++ show (void ct))
            Just func -> pure (Func func)
        Just (ref :: IORef DRVal) -> pure (LValue ref)
    ExprFuncall func ctArgsE rtArgsE ->
        interpretExpr func
        >>= \case
        Func f -> do
            ctArgs <- mapM interpretExpr ctArgsE
            rtArgs <- mapM interpretExpr rtArgsE
            res <-
                interpretStmts (funcBody f)
                & withScope (funcCTParams f, ctArgs) (funcRTParams f, rtArgs)
                & runExceptT
            case res of
                Left val -> pure val
                Right () -> fail "Missing 'return' statement"
        x -> showDVal x >>= fail . ("Calling a non-function: " ++)
    ExprLiteralNum x -> RValue (DNum x) & pure
    ExprLiteralBool x -> RValue (DBool x) & pure
    ExprLiteralStr x -> RValue (DString x) & pure
    ExprAssign var other ->
        interpretExpr var
        >>= \case
        LValue ref ->
            do
                rval <-
                    interpretExpr other
                    >>= getDRVal "Cannot assign from "
                writeIORef ref rval & liftIO
                LValue ref & pure
        _ -> fail "Assignment target is not an lvalue"
    ExprGetAttr _var _member -> error "unimplemented: getAttr"
    ExprParens var -> interpretExpr var
    ExprInfix l iop r ->
        join $ interpretInfix iop <$> interpretExpr l <*> interpretExpr r
    ExprMixin strExpr ->
        interpretExpr strExpr >>= getDRVal "Cannot mixin "
        >>= \case
        DString str ->
            case parseOnly parseExpr str of
            Left err -> fail ("mixin parse error: " ++ show err ++ " source: " ++ show str)
            Right expr -> interpretExpr expr
        x -> fail ("Cannot mixin " ++ showDRVal x)

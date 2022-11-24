{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables, FlexibleContexts, DerivingVia, DeriveGeneric, StandaloneDeriving #-}
module Language.D.Semantic
    ( semantic
    ) where

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
import Data.Scientific
import Generic.Data (Generic, Generically(..))
import Language.D.AST
import Language.D.Collect (collect, CollectEnv(..))
import Language.D.Parser (parseExpr)

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
    deriving stock (Generic)
    deriving Semigroup via Generically Scope

makeLenses ''Scope

type Semantic = ReaderT Scope (StateT (Map Ident FFuncDecl) IO)

showDRVal :: DRVal -> String
showDRVal (DNum v) = show v
showDRVal (DBool b) = show b
showDRVal (DString s) = show s

showDVal :: MonadIO m => DVal -> m String
showDVal (RValue v) = pure (showDRVal v)
showDVal (LValue ref) = liftIO (readIORef ref) <&> showDRVal
showDVal (Func f) = pure (show f)
showDVal Void = pure "void"

semantic :: FModule -> IO ()
semantic module_ =
    do
        let CollectEnv funcs pragmaMsgs = collect module_
        _ <- mapM_ (\expr -> semanticExpr expr >>= showDVal >>= liftIO . putStrLn) (reverse pragmaMsgs)
            & (`runReaderT` Scope mempty mempty)
            & (`execStateT` funcs)
        pure ()

semanticStmt :: FStmt -> ExceptT DVal Semantic ()
semanticStmt (StmtExpr val) = semanticExpr val & void & lift
semanticStmt (StmtIf cond t mf) = do
    RValue (DBool b) <- semanticExpr cond & lift
    if b then semanticStmt t else traverse_ semanticStmt mf
semanticStmt (StmtRet val) = lift (semanticExpr val) >>= throwError
semanticStmt (StmtBlock stmts) = semanticStmts stmts

semanticStmts :: [FStmt] -> ExceptT DVal Semantic ()
semanticStmts [] = pure ()
semanticStmts [x] = semanticStmt x
semanticStmts (x:xs) = semanticStmt x >> semanticStmts xs

-- | like zipWith but use fail if lengths mismatch
zipWithF :: MonadFail m => String -> (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithF sndName f =
    go
    where
        go [] [] = pure []
        go (x:xs) (y:ys) = (:) <$> f x y <*> go xs ys
        go [] (_:_) = fail ("Too many " ++ sndName)
        go (_:_) [] = fail ("Too few " ++ sndName)

getDRVal :: (MonadFail m, MonadIO m) => String -> DVal -> m DRVal
getDRVal msg =
    \case
    LValue ref -> liftIO (readIORef ref)
    RValue val -> pure val
    x -> showDVal x >>= fail . (msg ++)

newParamFromArg :: (MonadIO m, MonadFail m) => Param -> DVal -> m (Ident, IORef DRVal)
newParamFromArg (Param _type name) val =
    getDRVal "Invalid parameter: " val >>= liftIO . newIORef <&> (,) name

withScope ::
    (MonadReader Scope m, MonadIO m, MonadFail m) =>
    ([Param], [DVal]) -> ([Param], [DVal]) ->
    m a ->
    m a
withScope (ctParams, ctArgs) (rtParams, rtArgs) act =
    do
        newScope <-
            Scope
            <$> m "compile" ctParams ctArgs
            <*> m "run"     rtParams rtArgs
        local (newScope <>) act
    where
        m prefix ps as =
            zipWithF (prefix ++ "-time argument list") newParamFromArg ps as
            <&> fromList

num2 ::
    MonadFail f =>
    String -> (Scientific -> Scientific -> Scientific) ->
    DRVal -> DRVal -> f DRVal
num2 _ f (DNum x) (DNum y) = DNum (f x y) & pure
num2 msg _ _ _ = fail ("Cannot " ++ msg ++ " non-numbers)")

str2 ::
    MonadFail f =>
    String -> (ByteString -> ByteString -> ByteString) ->
    DRVal -> DRVal -> f DRVal
str2 _ f (DString x) (DString y) = DString (f x y) & pure
str2 msg _ _ _ = fail ("Cannot " ++ msg ++ " non-strings)")

funcOp :: MonadFail f => InfixOp -> DRVal -> DRVal -> f DRVal
funcOp InfixAdd = num2 "add" (+)
funcOp InfixSub = num2 "subtract" (-)
funcOp InfixMul = num2 "multiply" (*)
funcOp InfixConcat = str2 "concat" (<>)

semanticInfix :: InfixOp -> DVal -> DVal -> Semantic DVal
semanticInfix iop l r =
    funcOp iop <$> getDRVal msg l <*> getDRVal msg r
    & join
    <&> RValue
    where
        msg = "Cannot " ++ show iop ++ " from"

semanticExpr :: FExpr -> Semantic DVal
semanticExpr (FExpr e) =
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
        semanticExpr func
        >>= \case
        Func f -> do
            ctArgs <- mapM semanticExpr ctArgsE
            rtArgs <- mapM semanticExpr rtArgsE
            res <-
                semanticStmts (funcBody f)
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
        semanticExpr var
        >>= \case
        LValue ref ->
            do
                rval <-
                    semanticExpr other
                    >>= getDRVal "Cannot assign from "
                writeIORef ref rval & liftIO
                LValue ref & pure
        _ -> fail "Assignment target is not an lvalue"
    ExprGetAttr _var _member -> error "unimplemented: getAttr"
    ExprParens var -> semanticExpr var
    ExprInfix l iop r ->
        join $ semanticInfix iop <$> semanticExpr l <*> semanticExpr r
    ExprMixin strExpr ->
        semanticExpr strExpr >>= getDRVal "Cannot mixin "
        >>= \case
        DString str ->
            case parseOnly parseExpr str of
            Left err -> fail ("mixin parse error: " ++ show err ++ " source: " ++ show str)
            Right expr -> semanticExpr expr
        x -> fail ("Cannot mixin " ++ showDRVal x)

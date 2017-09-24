{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables #-}
module CTFE where

import AST
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
import Parser (parseExpr)

data CollectEnv = CollectEnv
    { _collectFuncs :: Map Ident FuncDecl
    , _collectPragmaMsgs :: [Expr]
    }
makeLenses ''CollectEnv

data DRVal
    = DNum Scientific
    | DBool Bool
    | DString ByteString

data DVal
    = Func FuncDecl
    | RValue DRVal
    | LValue (IORef DRVal)
    | Void

data Scope = Scope
    { _scopeParams :: Map Ident (IORef DRVal)
    }
makeLenses ''Scope

type Interpret = ReaderT Scope (StateT (Map Ident FuncDecl) IO)

showDRVal :: DRVal -> String
showDRVal (DNum v) = show v
showDRVal (DBool b) = show b
showDRVal (DString s) = show s

showDVal :: DVal -> Interpret String
showDVal (RValue v) = pure (showDRVal v)
showDVal (LValue ref) = liftIO (readIORef ref) <&> showDRVal
showDVal (Func f) = pure (show f)
showDVal Void = pure "void"

interpret :: Module -> IO ()
interpret (Module _ decls) =
    do
        let CollectEnv funcs pragmaMsgs =
                mapM_ collectDecl decls `execState` CollectEnv mempty []
        _ <- mapM_ (\expr -> interpretExpr expr >>= showDVal >>= liftIO . putStrLn) (reverse pragmaMsgs)
            & (`runReaderT` Scope mempty)
            & (`execStateT` funcs)
        pure ()

collectDecl :: Decl -> State CollectEnv ()
collectDecl (DeclFunc func)      = collectFuncs . at (funcIdent func) ?= func
collectDecl (DeclPragmaMsg expr) = collectPragmaMsgs %= (expr:)

interpretStmt :: Stmt -> ExceptT DVal Interpret ()
interpretStmt (StmtExpr val) = interpretExpr val & void & lift
interpretStmt (StmtIf cond t mf) = do
    RValue (DBool b) <- interpretExpr cond & lift
    if b then interpretStmt t else traverse_ interpretStmt mf
interpretStmt (StmtRet val) = lift (interpretExpr val) >>= throwError
interpretStmt (StmtBlock stmts) = interpretStmts stmts

interpretStmts :: [Stmt] -> ExceptT DVal Interpret ()
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

getDRVal :: String -> DVal -> Interpret DRVal
getDRVal msg =
    \case
    LValue ref -> liftIO (readIORef ref)
    RValue val -> pure val
    x -> showDVal x >>= fail . (msg ++)

newScope :: [Param] -> [DVal] -> Interpret Scope
newScope params args =
    match "arguments" pair params args
    <&> fromList
    >>= (\new -> ask <&> scopeParams %~ (new `mappend`))
    where
        pair (Param _type name) val =
            getDRVal "Invalid parameter: " val >>= liftIO . newIORef <&> (,) name

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
funcOp InfixConcat = str2 "concat" mappend

interpretInfix :: InfixOp -> DVal -> DVal -> Interpret DVal
interpretInfix iop l r =
    funcOp iop <$> getDRVal msg l <*> getDRVal msg r
    & join
    <&> RValue
    where
        msg = "Cannot " ++ show iop ++ " from"

interpretExpr :: Expr -> Interpret DVal
interpretExpr (ExprVar var) =
    view (scopeParams . at var)
    >>= \case
    Nothing ->
        use (at var)
        >>= \case
        Nothing -> fail ("Undefined variable: " ++ show var)
        Just func -> pure (Func func)
    Just (ref :: IORef DRVal) -> pure (LValue ref)
interpretExpr (ExprFuncall func args) =
    interpretExpr func
    >>= \case
    Func f -> do
        argsE <- mapM interpretExpr args
        scope <- newScope (funcParams f) argsE
        res <- interpretStmts (funcBody f)
            & local (const scope)
            & runExceptT
        case res of
            Left val -> pure val
            Right () -> fail "Missing 'return' statement"
    x -> showDVal x >>= fail . ("Calling a non-function: " ++)
interpretExpr (ExprLiteralNum x) = RValue (DNum x) & pure
interpretExpr (ExprLiteralBool x) = RValue (DBool x) & pure
interpretExpr (ExprLiteralStr x) = RValue (DString x) & pure
interpretExpr (ExprAssign var other) =
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
interpretExpr (ExprGetAttr _var _member) = error "unimplemented: getAttr"
interpretExpr (ExprParens var) = interpretExpr var
interpretExpr (ExprInfix l iop r) =
    join $ interpretInfix iop <$> interpretExpr l <*> interpretExpr r
interpretExpr (ExprMixin strExpr) =
    interpretExpr strExpr >>= getDRVal "Cannot mixin "
    >>= \case
    DString str ->
        case parseOnly parseExpr str of
        Left err -> fail ("mixin parse error: " ++ show err ++ " source: " ++ show str)
        Right expr -> interpretExpr expr
    x -> fail ("Cannot mixin " ++ showDRVal x)

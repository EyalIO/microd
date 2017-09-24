{-# OPTIONS -Wall #-}
module AST where

import           Data.Scientific (Scientific)

data Type = TInt | TVoid
    deriving Show

type Ident = String

data Param = Param
    { paramType :: Type
    , paramName :: Ident
    } deriving Show

data InfixOp
    = InfixAdd
    | InfixSub
    | InfixMul
    | InfixConcat
    deriving Show

data Expr
    = ExprLiteralNum Scientific
    | ExprVar Ident
    | ExprGetAttr Expr Ident
    | ExprParens Expr
    | ExprAssign Expr Expr
    | ExprFuncall Expr [Expr]
    | ExprInfix Expr InfixOp Expr
    deriving Show

data Stmt
    = StmtRet Expr
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtExpr Expr
    | StmtBlock [Stmt]
    --  | StmtDecl Decl
    deriving Show

data FuncDecl = FuncDecl
    { funcRetType :: Type
    , funcIdent :: Ident
    , funcParams :: [Param]
    , funcBody :: [Stmt]
    } deriving Show

data Decl
    = DeclFunc FuncDecl
    deriving Show

data Module = Module
    { astModuleName :: String
    , astDecls :: [Decl]
    } deriving Show

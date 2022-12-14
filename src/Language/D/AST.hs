module Language.D.AST
    ( Type(..)
    , Ident
    , Param(..)
    , InfixOp(..)
    , Expr(..)
    , Stmt(..)
    , FuncDecl(..)
    , Decl(..)
    , Module(..)
    , FExpr(..)
    , FDecl
    , FFuncDecl
    , FModule
    , FStmt
    ) where

import Data.Scientific (Scientific)
import Data.Text (Text)

data Type = TInt | TVoid | TBool | TString
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

data Expr expr
    = ExprLiteralNum (Either Integer Double)
    | ExprLiteralBool Bool
    | ExprLiteralStr Text
    | ExprMixin expr
    | ExprVar Ident
    | ExprGetAttr expr Ident
    | ExprParens expr
    | ExprAssign expr expr
    | ExprFuncall expr [expr] [expr]
    | ExprInfix expr InfixOp expr
    deriving Show

data Stmt expr
    = StmtRet expr
    | StmtIf expr (Stmt expr) (Maybe (Stmt expr))
    | StmtExpr expr
    | StmtBlock [Stmt expr]
    | StmtMixin expr
    --  | StmtDecl Decl
    deriving Show

data FuncDecl expr = FuncDecl
    { funcRetType :: Type
    , funcIdent :: Ident
    , funcCTParams :: [Param]
    , funcRTParams :: [Param]
    , funcBody :: [Stmt expr]
    } deriving Show

data Decl expr
    = DeclFunc (FuncDecl expr)
    | DeclPragmaMsg expr
    -- todo: | DeclMixin expr
    deriving Show

data Module expr = Module
    { astModuleName :: String
    , astDecls :: [Decl expr]
    } deriving Show

-- Direct fix-point for Expr
newtype FExpr = FExpr { _fExpr :: Expr FExpr } deriving Show
type FStmt = Stmt FExpr
type FDecl = Decl FExpr
type FFuncDecl = FuncDecl FExpr
type FModule = Module FExpr

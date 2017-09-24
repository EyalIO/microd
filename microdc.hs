{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (sepBy)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Scientific (Scientific)
import           Data.Char (isAlphaNum)

data Type = TInt | TVoid
    deriving Show

guardChar :: String -> (Char -> Bool) -> Parser ()
guardChar msg p = do
    mc <- peekChar
    case mc of
        Just c | not (p c) -> fail ("Expecting " ++ msg)
        _ -> pure ()

keyword :: ByteString -> Parser ()
keyword s = string s *> guardChar "new word" (not . isAlphaNum) *> noise

oper :: ByteString -> Parser ()
oper s = string s *> guardChar "end of operator" (`notElem` ("+*-~/=%^&|"::String))

parseType :: Parser Type
parseType =
    (TInt <$ keyword "int")
    <|> (TVoid <$ keyword "void")

inRange :: Ord a => a -> a -> a -> Bool
inRange low hi x = low <= x && x <= hi

type Ident = String

parseIdent :: Parser Ident
parseIdent =
    (:)
    <$> satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_")
    <*> many1 (satisfy isAlpha_ascii)

data Param = Param
    { paramType :: Type
    , paramName :: Ident
    } deriving Show

noise :: Parser ()
noise = skipSpace

parseParam :: Parser Param
parseParam = Param <$> (parseType <* noise) <*> parseIdent

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

parseExpr :: Parser Expr
parseExpr = parseAssign

parseAssign :: Parser Expr
parseAssign = parseRAssocOp "=" ExprAssign parseAddSub

parseAddSub :: Parser Expr
parseAddSub =
    parseLAssocInfix
    (InfixAdd <$ oper "+" <|> InfixSub <$ oper "-" <|> InfixConcat <$ oper "~")
    parseMul

parseMul :: Parser Expr
parseMul = parseLAssocInfix (InfixMul <$ oper "*") parseTerminal

parseLAssocInfix :: Parser InfixOp -> Parser Expr -> Parser Expr
parseLAssocInfix infixOp higherPrecParser = do
    l <- higherPrecParser <* noise
    go l
    where
        go l = do
            op <- infixOp
            r <- higherPrecParser
            go (ExprInfix l op r)
            <|> pure l

parseRAssocOp :: ByteString -> (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
parseRAssocOp opStr cons higherPrecParser = do
    expr <- higherPrecParser <* noise
    more <- many (oper opStr *> noise *> higherPrecParser)
    pure $ case more of
        [] -> expr
        (x:xs) -> cons expr (go x xs)
    where
        go x [] = x
        go x (y:ys) = cons x (go y ys)

openParen :: Parser ()
openParen = char '(' *> noise

closeParen :: Parser ()
closeParen = noise <* char ')'

semicolon :: Parser ()
semicolon = noise <* char ';'

sepBy :: Parser a -> Parser x -> Parser [a]
p `sepBy` sep = p `P.sepBy` (noise *> sep <* noise)

parseTerminal:: Parser Expr
parseTerminal =
    do
        term <-
            ExprLiteralNum <$> scientific
            <|> ExprParens <$> (openParen *> parseExpr <* closeParen)
            <|> ExprVar <$> parseIdent
        postfix term
    where
        postfix e = do
            noise
            (ExprFuncall e
                <$> (openParen *>
                        (parseExpr `sepBy` (char ','))
                        <* closeParen)
                >>= postfix)
                <|> (ExprGetAttr e <$> (char '.' *> parseIdent) >>= postfix)
                <|> pure e

data Stmt
    = StmtRet Expr
    | StmtIf Expr Stmt (Maybe Stmt)
    | StmtExpr Expr
    | StmtBlock [Stmt]
    --  | StmtDecl Decl
    deriving Show

parseBlock :: Parser [Stmt]
parseBlock =
    char '{' *> noise *>
    many parseStmt <* noise <*
    char '}'

parseStmt :: Parser Stmt
parseStmt =
    (StmtRet <$> (keyword "return" *> parseExpr <* semicolon))
    <|> (StmtIf
            <$> (keyword "if" *> openParen *> parseExpr <* closeParen)
            <*> (noise *> parseStmt)
            <*> (optional (noise *> keyword "else" *> parseStmt)))
    <|> (StmtBlock <$> parseBlock)

data FuncDecl = FuncDecl
    { funcRetType :: Type
    , funcIdent :: Ident
    , funcParams :: [Param]
    , funcBody :: [Stmt]
    } deriving Show

parseFuncDecl :: Parser FuncDecl
parseFuncDecl =
    FuncDecl
    <$> parseType <* noise
    <*> parseIdent <* noise
    <*> (openParen *> (parseParam `sepBy` char ',') <* closeParen) <* noise
    <*> parseBlock

data Decl
    = ASTFuncDecl FuncDecl
    deriving Show

parseDecl :: Parser Decl
parseDecl = ASTFuncDecl <$> parseFuncDecl

data Module = Module
    { astModuleName :: String
    , astDecls :: [Decl]
    } deriving Show

test :: ByteString
test = "module Foo; int foo() { return 5; }"

parseModule :: Parser Module
parseModule =
    noise *>
    (Module
        <$> (keyword "module" *> noise *> parseIdent <* semicolon)
        <*> many (noise *> parseDecl))
    <* endOfInput

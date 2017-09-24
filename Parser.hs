{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Parser where

import           AST
import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Attoparsec.ByteString.Char8 hiding (sepBy)
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (isAlphaNum)

guardChar :: String -> (Char -> Bool) -> Parser ()
guardChar msg p = do
    mc <- peekChar
    case mc of
        Just c | not (p c) -> fail ("Expecting " ++ msg)
        _ -> pure ()

keyword :: ByteString -> Parser ()
keyword s = string s *> guardChar "new word" (not . isAlphaNum) *> noise

oper :: ByteString -> Parser ()
oper s = string s *> guardChar "end of operator" (`notElem` ("+*-~/=%^&|"::String)) *> noise

parseType :: Parser Type
parseType =
    (TInt <$ keyword "int")
    <|> (TVoid <$ keyword "void")
    <|> (TBool <$ keyword "bool")
    <|> (TString <$ keyword "string")

inRange :: Ord a => a -> a -> a -> Bool
inRange low hi x = low <= x && x <= hi

parseIdent :: Parser Ident
parseIdent =
    (:)
    <$> satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_")
    <*> many (satisfy isAlpha_ascii)
    <?> "Identifier"

noise :: Parser ()
noise = skipSpace <?> "whitespace"

parseParam :: Parser Param
parseParam =
    Param <$> (parseType <* noise) <*> parseIdent
    <?> "Parameter"

parseExpr :: Parser Expr
parseExpr = parseAssign <?> "Expression"

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
    more <- many (oper opStr *> higherPrecParser)
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

literalBool :: Parser Bool
literalBool = False <$ "false" <|> True <$ "true"

escapeSequence :: Parser Char
escapeSequence =
    char '\\' *> anyChar >>= unescape
    where
        unescape 't' = pure '\t'
        unescape 'n' = pure '\n'
        unescape 'r' = pure '\r'
        unescape x = fail ("Bad escape char: " ++ show x)

literalStr :: Parser String
literalStr =
    quoted '"' <|> quoted '\'' <|> quoted '`'
    where
        quoted q = char q *> many oneChar <* char q
        oneChar = escapeSequence <|> satisfy (`notElem` ['\\', '"'])

parseTerminal:: Parser Expr
parseTerminal =
    do
        term <-
                ExprLiteralNum <$> scientific
            <|> ExprLiteralBool <$> literalBool
            <|> ExprLiteralStr <$> literalStr
            <|> ExprParens <$> (openParen *> parseExpr <* closeParen)
            <|> ExprVar <$> parseIdent
            <?> "Terminal expression"
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

parseBlock :: Parser [Stmt]
parseBlock =
    char '{' *> noise *>
    many (parseStmt <* noise) <*
    char '}'
    <?> "Statement block"

parseStmt :: Parser Stmt
parseStmt =
    StmtRet <$> (keyword "return" *> parseExpr <* semicolon)
    <|> StmtIf
            <$> (keyword "if" *> openParen *> parseExpr <* closeParen)
            <*> (noise *> parseStmt)
            <*> (optional (noise *> keyword "else" *> parseStmt))
    <|> StmtBlock <$> parseBlock
    <|> StmtExpr <$> parseExpr <* semicolon
    <?> "Statement"

parseFuncDecl :: Parser FuncDecl
parseFuncDecl =
    FuncDecl
    <$> parseType <* noise
    <*> parseIdent <* noise
    <*> (openParen *> (parseParam `sepBy` char ',') <* closeParen) <* noise
    <*> parseBlock
    <?> "Function Declaration"

parsePragma :: ByteString -> Parser a -> Parser a
parsePragma pragma p =
    keyword "pragma" *> openParen *> string pragma *> noise *> oper "," *> p <* closeParen
    <?> "Pragma"

parseDecl :: Parser Decl
parseDecl =
    DeclFunc <$> parseFuncDecl
    <|> (DeclPragmaMsg <$> parsePragma "msg" parseExpr <* semicolon)
    <?> "Declaration"

parseModule :: Parser Module
parseModule =
    noise *>
    (Module
        <$> (keyword "module" *> noise *> parseIdent <* semicolon)
        <*> many (noise *> parseDecl))
    <* noise
    <* endOfInput
    <?> "Module"

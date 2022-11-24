{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Language.D.Parser
    ( Parser
    , parseStmts
    , parseDecl
    , parseExpr
    , parseModule
    , eof
    ) where

import           Control.Applicative
import           Control.Lens.Operators
import           Data.Char (isAlphaNum)
import           Data.Text (Text, pack)
import           Language.D.AST
import           Text.Trifecta (Parser, (<?>))
import qualified Text.Trifecta as P

keyword :: Text -> Parser ()
keyword s = P.try (P.text s *> (P.notFollowedBy P.alphaNum <?>"?")) <* noise

oper :: Text -> Parser ()
oper s = P.try (P.text s *> P.notFollowedBy (P.satisfy (`elem` ("+*-~/=%^&|"::String)))) <* noise

parseType :: Parser Type
parseType =
    (TInt <$ keyword "int")
    <|> (TVoid <$ keyword "void")
    <|> (TBool <$ keyword "bool")
    <|> (TString <$ keyword "string")

parseIdent :: Parser Ident
parseIdent =
    (:)
    <$> P.satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_")
    <*> many P.alphaNum
    <?> "Identifier"

noise :: Parser ()
noise = P.whiteSpace -- todo: OR comments...

parseParam :: Parser Param
parseParam =
    Param <$> (parseType <* noise) <*> parseIdent
    <?> "Parameter"

parseExpr :: Parser FExpr
parseExpr = parseAssign <?> "Expression"

parseAssign :: Parser FExpr
parseAssign = parseRAssocOp "=" (ExprAssign <&> fmap FExpr) parseAddSub

parseAddSub :: Parser FExpr
parseAddSub =
    parseLAssocInfix
    (InfixAdd <$ oper "+" <|> InfixSub <$ oper "-" <|> InfixConcat <$ oper "~")
    parseMul

parseMul :: Parser FExpr
parseMul = parseLAssocInfix (InfixMul <$ oper "*") parsePostfix

parseLAssocInfix :: Parser InfixOp -> Parser FExpr -> Parser FExpr
parseLAssocInfix infixOp higherPrecParser = do
    l <- higherPrecParser <* noise
    go l
    where
        go l = do
            op <- P.try infixOp
            r <- higherPrecParser
            go (ExprInfix l op r & FExpr)
            <|> pure l

parseRAssocOp :: Text -> (FExpr -> FExpr -> FExpr) -> Parser FExpr -> Parser FExpr
parseRAssocOp opStr cons higherPrecParser = do
    expr <- higherPrecParser <* noise
    more <- many (oper opStr *> higherPrecParser)
    pure $ case more of
        [] -> expr
        (x:xs) -> cons expr (go x xs)
    where
        go x [] = x
        go x (y:ys) = cons x (go y ys)

semicolon :: Parser ()
semicolon = noise <* P.char ';'

sepBy :: Parser a -> Parser x -> Parser [a]
p `sepBy` sep = p `P.sepBy` (noise *> sep <* noise)

literalBool :: Parser Bool
literalBool = False <$ keyword "false" <|> True <$ keyword "true"

escapeSequence :: Parser Char
escapeSequence =
    P.char '\\' *> P.anyChar >>= unescape
    where
        unescape 't' = pure '\t'
        unescape 'n' = pure '\n'
        unescape 'r' = pure '\r'
        unescape x = fail ("Bad escape char: " ++ show x)

literalStr :: Parser Text
literalStr =
    quoted '"' <|> quoted '\'' <|> quoted '`'
    <&> pack
    where
        quoted q = P.char q *> many oneChar <* P.char q
        oneChar = escapeSequence <|> P.satisfy (`notElem` ['\\', '"'])

parseMixin :: Parser FExpr
parseMixin = keyword "mixin" *> noise *> P.parens parseExpr

parseTerminal :: Parser FExpr
parseTerminal =
    (   ExprLiteralNum <$> P.integerOrDouble
    <|> ExprLiteralBool <$> literalBool
    <|> ExprLiteralStr <$> literalStr
    <|> ExprMixin <$> parseMixin
    <|> ExprParens <$> P.parens parseExpr
    <|> ExprVar <$> parseIdent
    <&> FExpr
    ) <* noise
    <?> "Terminal expression"

parsePostfix :: Parser FExpr
parsePostfix =
    do
        FExpr term <- parseTerminal
        postfix term
    where
        argList = P.parens (parseExpr `sepBy` P.char ',')
        postfix e = do
            let ex = FExpr e
            (ExprFuncall ex <$> (oper "!" *> (argList <|> (:[]) <$> parseTerminal)) <*> argList
             <|> ExprFuncall ex [] <$> argList
             >>= postfix)
                <|> (P.char '.' *> parseIdent <&> ExprGetAttr ex >>= postfix)
                <|> pure ex

parseBlock :: Parser [FStmt]
parseBlock =
    P.char '{' *> noise *>
    parseStmts <*
    P.char '}'
    <?> "Statement block"

parseStmts :: Parser [FStmt]
parseStmts = many (parseStmt <* noise)

parseStmt :: Parser FStmt
parseStmt =
    StmtRet <$> (keyword "return" *> parseExpr <* semicolon)
    <|> StmtIf
            <$> (keyword "if" *> P.parens parseExpr)
            <*> (noise *> parseStmt)
            <*> optional (noise *> keyword "else" *> parseStmt)
    <|> StmtBlock <$> parseBlock
    <|> StmtMixin <$> parseMixin <* semicolon
    <|> StmtExpr <$> parseExpr <* semicolon
    <?> "Statement"

parseFuncDecl :: Parser FFuncDecl
parseFuncDecl =
    mkFuncDecl
    <$> parseType <* noise
    <*> parseIdent <* noise
    <*> paramList <* noise
    <*> optional (paramList <* noise)
    <*> parseBlock
    <?> "Function Declaration"
    where
        mkFuncDecl typ ident params Nothing block =
            FuncDecl typ ident [] params block
        mkFuncDecl typ ident ctParams (Just rtParams) block =
            FuncDecl typ ident ctParams rtParams block
        paramList = P.parens (parseParam `sepBy` P.char ',')

parsePragma :: Text -> Parser a -> Parser a
parsePragma pragma p =
    keyword "pragma" *> P.parens (keyword pragma *> noise *> oper "," *> p)
    <?> "Pragma"

parseDecl :: Parser FDecl
parseDecl =
    DeclFunc <$> parseFuncDecl
    <|> (DeclPragmaMsg <$> parsePragma "msg" parseExpr <* semicolon)
    -- <|> DeclMixin <$> parseMixin
    <?> "Declaration"

parseModule :: Parser FModule
parseModule =
    noise *>
    (Module
        <$> (keyword "module" *> noise *> parseIdent <* semicolon)
        <*> many (P.try (noise *> parseDecl)))
    <* eof
    <?> "Module"

eof :: Parser ()
eof = noise <* P.eof

{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.ByteString.Char8 as BS8
import           Language.D.Parser (parseModule)
import qualified Language.D.Semantic as Semantic
import           System.Environment (getArgs)
import qualified Text.Trifecta as Parser

run :: FilePath -> IO ()
run filePath =
    Parser.parseFromFileEx parseModule filePath
    >>= \case
        Parser.Failure err -> print err
        Parser.Success module_ -> Semantic.semanticModule module_

main :: IO ()
main = do
    [filePath] <- getArgs
    run filePath

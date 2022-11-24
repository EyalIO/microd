{-# OPTIONS -Wall #-}
module Main where

import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BS8
import           Language.D.Parser (parseModule)
import qualified Language.D.Semantic as Semantic
import           System.Environment (getArgs)

run :: FilePath -> IO ()
run filePath =
    do
        moduleData <- BS8.readFile filePath
        case Parser.parseOnly parseModule moduleData of
            Left err -> print $ "Parse error: " <> err
            Right module_ -> Semantic.semanticModule module_

main :: IO ()
main = do
    [filePath] <- getArgs
    run filePath

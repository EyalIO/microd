{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified CTFE
import           Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BS8
import           Parser (parseModule)
import           System.Environment (getArgs)

run :: FilePath -> IO ()
run filePath =
    BS8.readFile filePath <&> Parser.parseOnly parseModule
    >>= \case
    Left err -> print err
    Right module_ -> CTFE.interpret module_

main :: IO ()
main = do
    [filePath] <- getArgs
    run filePath

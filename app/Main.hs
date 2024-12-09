module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import Data.List
import Data.Maybe
import Lexer
import Parser
import Tokens

main :: IO ()
main = do
  filename <- getLine
  file <- BS.readFile filename
  case runAlex file (modify (\s -> s{fileName = filename}) >> go) of
    Left err -> putStrLn err
    Right tokens -> putStrLn $ intercalate "\n" $ map show tokens
 where
  go = do
    output <- alexMonadScan
    if output == WithPosition TokenEOF (0, 0) then pure [output] else (output :) <$> go

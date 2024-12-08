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
  let lexerTokens = fromRight [] $ runAlex file (modify (\s -> s{fileName = filename}) >> go)
  let tokens = if TokenEOF `elem` lexerTokens then lexerTokens else lexerTokens ++ [TokenEOF]
  if any (isJust . fromError) tokens
    then putStrLn $ intercalate "\n" $ mapMaybe fromError tokens ++ ["Compilation halted"]
    else print (parse tokens)
 where
  go = do
    output <- alexMonadScan
    if isFinalToken output then pure [output] else (output :) <$> go
  fromError (TokenError a) = Just a
  fromError _ = Nothing
  isFinalToken TokenEOF = True
  isFinalToken (TokenError str) = "EOF" `isInfixOf` str
  isFinalToken _ = False

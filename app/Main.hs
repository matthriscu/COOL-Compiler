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
  let tokens = if WithPosition TokenEOF (0, 0) `elem` lexerTokens then lexerTokens else lexerTokens ++ [WithPosition TokenEOF (0, 0)]
  if any (isJust . fromError) tokens
    then putStrLn $ intercalate "\n" $ mapMaybe fromError tokens ++ ["Compilation halted"]
    else putStrLn $ intercalate "\n" $ map show tokens
 where
  go = do
    output <- alexMonadScan
    if isFinalToken output then pure [output] else (output :) <$> go
  fromError (WithPosition (TokenError a) _) = Just a
  fromError _ = Nothing
  isFinalToken (WithPosition TokenEOF _) = True
  isFinalToken (WithPosition (TokenError str) _) = "EOF" `isInfixOf` str
  isFinalToken _ = False

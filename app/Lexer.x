{
module Lexer where

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BS
import Tokens
}

%wrapper "monadUserState-bytestring"

tokens :-
<0>  $white+              ;
<0>  "--".*               ;
<0>  "<-"                 { tok TokenAssign }
<0>  "class"              { tok TokenClass }
<0>  "inherits"           { tok TokenInherits}
<0>  "not"                { tok TokenNot }
<0>  "if"                 { tok TokenIf }
<0>  "then"               { tok TokenThen }
<0>  "else"               { tok TokenElse }
<0>  "fi"                 { tok TokenFi }
<0>  "case"               { tok TokenCase }
<0>  "esac"               { tok TokenEsac }
<0>  "let"                { tok TokenLet }
<0>  "in"                 { tok TokenIn }
<0>  "while"              { tok TokenWhile }
<0>  "loop"               { tok TokenLoop }
<0>  "pool"               { tok TokenPool }
<0>  "true"               { tok (TokenBooleanLiteral True) }
<0>  "false"              { tok (TokenBooleanLiteral False) }
<0>  "+"                  { tok TokenPlus }
<0>  "-"                  { tok TokenMinus }
<0>  "*"                  { tok TokenStar }
<0>  "/"                  { tok TokenSlash }
<0>  "=>"                 { tok TokenCaseArrow }
<0>  "of"                 { tok TokenOf }
<0>  "<"                  { tok TokenLessThan }
<0>  "<="                 { tok TokenLessThanEqual }
<0>  "="                  { tok TokenEqual }
<0>  "isvoid"             { tok TokenIsVoid }
<0>  "new"                { tok TokenNew }
<0>  "@"                  { tok TokenAt }
<0>  "~"                  { tok TokenTilde }
<0>  ":"                  { tok TokenColon }
<0>  ";"                  { tok TokenSemicolon }
<0>  "{"                  { tok TokenLeftBrace }
<0>  "}"                  { tok TokenRightBrace }
<0>  "("                  { tok TokenLeftParen }
<0>  ")"                  { tok TokenRightParen }
<0>  ","                  { tok TokenComma }
<0>  "."                  { tok TokenDot }
<0>  [A-Z][a-zA-Z0-9_]*   { tokType }
<0>  [a-z][a-zA-Z0-9_]*   { tokIdentifier }
<0>  [0-9]+               { tokIntLiteral }
<0>  \"                   { enterString `andBegin` string }
<0> "(*"                  { enterComment `andBegin` comment }
<0> "*)"                  { \(pos, _, _, _) _ -> get >>= return . makeLexicalError pos ("Unmatched *)") }
<0>  .                    { \(pos, _, str, _) _ -> get >>= return . makeLexicalError pos ("Invalid character: " ++ [BS.head str]) }
<string> [.\n]            { processChar }
<comment> "(*"            { adjustNestLevel 1 }
<comment> "*)"            { adjustNestLevel (-1) }
<comment> [.\n]           ;

{
data AlexUserState = AlexUserState
  { fileName :: String
  , nestLevel :: Int
  , startPos :: AlexPosn
  , currentString :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState
    { fileName = ""
    , nestLevel = 0
    , startPos = AlexPn 0 0 0
    , currentString = ""
    }

alexEOF :: Alex (WithPosition Token)
alexEOF = do
  code <- alexGetStartCode
  state <- get
  return $ case code of
    0 -> WithPosition TokenEOF (0, 0)
    comment -> makeLexicalError (startPos state) "EOF in comment" state
    string -> makeLexicalError (startPos state) "EOF in string constant" state

convertPosition :: AlexPosn -> (Int, Int)
convertPosition (AlexPn _ l c) = (l, c)

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

enterString :: AlexAction (WithPosition Token)
enterString input@(pos, _, _, _) len = modify (\s -> s{startPos = pos, currentString = ""}) >> skip input len

enterComment :: AlexAction (WithPosition Token)
enterComment input@(pos, _, _, _) len = modify (\s -> s{startPos = pos, nestLevel = 1}) >> skip input len

adjustNestLevel :: Int -> AlexAction (WithPosition Token)
adjustNestLevel d input len = do
  state <- get
  put state{nestLevel = nestLevel state + d}
  when (nestLevel state + d == 0) $ alexSetStartCode 0
  skip input len

makeLexicalError :: AlexPosn -> String -> AlexUserState -> (WithPosition Token)
makeLexicalError (AlexPn _ l c) message state =
  WithPosition (TokenError $ show (fileName state) ++ ", line " ++ show l ++ ":" ++ show c ++ ", Lexical error: " ++ message) (l, c)

processChar :: AlexAction (WithPosition Token)
processChar input@(_, prev, str, _) len
  | c == '\0' = do
      state <- get
      return $ makeLexicalError (startPos state) "String contains null character" state
  | prev == '\\' = append (handleEscape c) >> skip input len
  | c == '\\' = skip input len
  | c == '\n' = do
      state <- get
      alexSetStartCode 0
      return $ makeLexicalError (startPos state) "Unterminated string constant" state
  | c == '\"' = do
      state <- get
      alexSetStartCode 0
      if length (currentString state) > 1024
        then return $ makeLexicalError (startPos state) "String constant too long" state
        else return $ WithPosition (TokenStringLiteral $ currentString state) (convertPosition (startPos state))
  | otherwise = append [c] >> skip input len
 where
  c = BS.head str
  append x = modify (\s -> s{currentString = currentString s ++ x})
  handleEscape x = case x of
    'n' -> "\n"
    't' -> "\t"
    'b' -> "\b"
    'f' -> "\f"
    _ -> [x]

tok :: Token -> AlexAction (WithPosition Token)
tok ctor (pos, _, _, _) _ = return $ WithPosition ctor (convertPosition pos)

tokIdentifier :: AlexAction (WithPosition Token)
tokIdentifier (pos, _, str, _) len = return $ WithPosition (TokenIdentifier $ BS.unpack $ BS.take len str) (convertPosition pos)

tokType :: AlexAction (WithPosition Token)
tokType (pos, _, str, _) len = return $ WithPosition (TokenType $ BS.unpack $ BS.take len str) (convertPosition pos)

tokIntLiteral :: AlexAction (WithPosition Token)
tokIntLiteral (pos, _, str, _) len = return $ WithPosition (TokenIntLiteral $ read $ BS.unpack $ BS.take len str) (convertPosition pos)
}

{
module Parser where
import Lexer
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  class           { TokenClass }
  inherits        { TokenInherits }
  not             { TokenNot }
  if              { TokenIf }
  then            { TokenThen }
  else            { TokenElse }
  fi              { TokenFi }
  case            { TokenCase }
  esac            { TokenEsac }
  let             { TokenLet }
  in              { TokenIn }
  while           { TokenWhile }
  loop            { TokenLoop }
  pool            { TokenPool }
  new             { TokenNew }
  plus            { TokenPlus }
  minus           { TokenMinus }
  star            { TokenStar }
  slash           { TokenSlash }
  lessThan        { TokenLessThan }
  lessThanEqual   { TokenLessThanEqual }
  equal           { TokenEqual }
  isVoid          { TokenIsVoid }
  assign          { TokenAssign }
  caseArrow       { TokenCaseArrow }
  of              { TokenOf }
  at              { TokenAt }
  tilde           { TokenTilde }
  colon           { TokenColon }
  semicolon       { TokenSemicolon }
  leftBrace       { TokenLeftBrace }
  rightBrace      { TokenRightBrace }
  leftParen       { TokenLeftParen }
  rightParen      { TokenRightParen }
  comma           { TokenComma }
  dot             { TokenDot }
  identifier      { TokenIdentifier $$ }
  type            { TokenType $$ }
  intLiteral      { TokenIntLiteral $$ }
  stringLiteral   { TokenStringLiteral $$ }
  booleanLiteral  { TokenBooleanLiteral $$ }
  eof             { TokenEOF }

%right assign
%nonassoc not
%left lessThan lessThanEqual equal
%left plus minus
%left star slash
%nonassoc isVoid
%nonassoc tilde
%nonassoc at
%nonassoc dot

%%

optional(p)
  : p { Just $1 }
  | { Nothing }

many(p)
  : p { [$1] }
  | p many(p) { $1 : $2 }

sepBy1(p, sep)
  : p { [$1] }
  | p sep sepBy(p, sep) { $1 : $3 }

sepBy(p, sep)
  : {- empty -} { [] }
  | p { [$1] }
  | p sep sepBy(p, sep) { $1 : $3 }

program :: { Program }
  : sepBy1(classDefinition, semicolon) eof { Program $1 }

inheritsType :: { Type }
  : inherits type { Type $2 }

atType :: { Type }
  : at type { Type $2 }

classDefinition :: { Class }
  : class type optional(inheritsType) leftBrace sepBy(feature, semicolon) rightBrace { Class (Type $2) $3 $5 }

formal :: { Formal }
  : identifier colon type { Formal (Identifier $1) (Type $3) }

assignExpr :: { Expr }
  : assign expr { $2 }

feature :: { Feature }
  : identifier colon type optional(assignExpr) { Attribute (Identifier $1) (Type $3) $4 }
  | identifier leftParen sepBy(formal, comma) rightParen colon type leftBrace expr rightBrace { Method (Identifier $1) $3 (Type $6) $8 }

letBinding :: { (Identifier, Type, Maybe Expr) }
  : identifier colon type optional(assignExpr) { ((Identifier $1), (Type $3), $4) }

caseBranch :: { (Identifier, Type, Expr) }
  : identifier colon type caseArrow expr { (Identifier $1, Type $3, $5) }

expr :: { Expr }
  : identifier assign expr { Assign (Identifier $1) $3 }
  | expr optional(atType) dot identifier leftParen sepBy(expr, comma) rightParen { ExplicitDispatch $1 $2 (Identifier $4) $6 }
  | identifier leftParen sepBy(expr, comma) rightParen { ImplicitDispatch (Identifier $1) $3 }
  | if expr then expr else expr fi { If $2 $4 $6 }
  | while expr loop expr pool { While $2 $4 }
  | leftBrace sepBy1(expr, semicolon) rightBrace { Block $2 }
  | let sepBy1(letBinding, comma) in expr { Let $2 $4 }
  | case expr of sepBy1(caseBranch, semicolon) esac { Case $2 $4 }
  | new type { New (Type $2) }
  | isVoid expr { IsVoid $2 }
  | expr plus expr { Add $1 $3 }
  | expr minus expr { Subtract $1 $3 }
  | expr star expr { Multiply $1 $3 }
  | expr slash expr { Divide $1 $3 }
  | tilde expr { Complement $2 }
  | expr lessThan expr { LessThan $1 $3 }
  | expr lessThanEqual expr { LessThanEqual $1 $3 }
  | expr equal expr { Equal $1 $3 }
  | not expr { Not $2 }
  | leftParen expr rightParen { $2 }
  | identifier { ID (Identifier $1) }
  | intLiteral { IntLiteral $1 }
  | stringLiteral { StringLiteral $1 }
  | booleanLiteral { BooleanLiteral $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

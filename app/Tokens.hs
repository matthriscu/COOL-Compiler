module Tokens where

data Token
  = TokenClass
  | TokenInherits
  | TokenNot
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFi
  | TokenCase
  | TokenEsac
  | TokenLet
  | TokenIn
  | TokenWhile
  | TokenLoop
  | TokenPool
  | TokenNew
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenSlash
  | TokenLessThan
  | TokenLessThanEqual
  | TokenEqual
  | TokenIsVoid
  | TokenAssign
  | TokenCaseArrow
  | TokenOf
  | TokenAt
  | TokenTilde
  | TokenColon
  | TokenSemicolon
  | TokenLeftBrace
  | TokenRightBrace
  | TokenLeftParen
  | TokenRightParen
  | TokenComma
  | TokenDot
  | TokenIdentifier {identifierValue :: String}
  | TokenType {typeValue :: String}
  | TokenIntLiteral {intValue :: Int}
  | TokenStringLiteral {stringValue :: String}
  | TokenBooleanLiteral {boolValue :: Bool}
  | TokenError String
  | TokenEOF
  deriving (Show, Eq)

newtype Program = Program [Class] deriving (Eq)

data Class = Class Type (Maybe Type) [Feature] deriving (Eq)

data Feature
  = Method Identifier [Formal] Type Expr
  | Attribute Identifier Type (Maybe Expr)
  deriving (Eq)

newtype Identifier = Identifier String deriving (Eq)

newtype Type = Type String deriving (Eq)

data Formal = Formal Identifier Type deriving (Eq)

data Expr
  = Assign Identifier Expr
  | ExplicitDispatch Expr (Maybe Type) Identifier [Expr]
  | ImplicitDispatch Identifier [Expr]
  | If Expr Expr Expr
  | While Expr Expr
  | Block [Expr]
  | Let [(Identifier, Type, Maybe Expr)] Expr
  | Case Expr [(Identifier, Type, Expr)]
  | New Type
  | IsVoid Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Complement Expr
  | LessThan Expr Expr
  | LessThanEqual Expr Expr
  | Equal Expr Expr
  | Not Expr
  | ID Identifier
  | IntLiteral Int
  | StringLiteral String
  | BooleanLiteral Bool
  deriving (Eq)

makeTag :: Int -> String -> String
makeTag n s = replicate (2 * n) ' ' ++ s ++ "\n"

showLetBindingIndented :: Int -> (Identifier, Type, Maybe Expr) -> String
showLetBindingIndented n (name, typ, expr) =
  makeTag n "local"
    ++ showIndented (n + 1) name
    ++ showIndented (n + 1) typ
    ++ maybe "" (showIndented (n + 1)) expr

showCaseBranchIndented :: Int -> (Identifier, Type, Expr) -> String
showCaseBranchIndented n (name, typ, expr) =
  makeTag n "case branch"
    ++ showIndented (n + 1) name
    ++ showIndented (n + 1) typ
    ++ showIndented (n + 1) expr

instance Show Program where
  show (Program classes) =
    makeTag 0 "program"
      ++ concatMap (showIndented 1) classes
      ++ "\n"

class ShowIndented a where
  showIndented :: Int -> a -> String

instance ShowIndented Class where
  showIndented n (Class name parent features) =
    makeTag n "class"
      ++ showIndented (n + 1) name
      ++ maybe "" (showIndented (n + 1)) parent
      ++ concatMap (showIndented (n + 1)) features

instance ShowIndented Type where
  showIndented n (Type name) = makeTag n name

instance ShowIndented Feature where
  showIndented n (Method name formals returnType body) =
    makeTag n "method"
      ++ showIndented (n + 1) name
      ++ concatMap (showIndented (n + 1)) formals
      ++ showIndented (n + 1) returnType
      ++ showIndented (n + 1) body
  showIndented n (Attribute name typ value) =
    makeTag n "attribute"
      ++ showIndented (n + 1) name
      ++ showIndented (n + 1) typ
      ++ maybe "" (showIndented (n + 1)) value

instance ShowIndented Identifier where
  showIndented n (Identifier name) = makeTag n name

instance ShowIndented Formal where
  showIndented n (Formal name typ) =
    makeTag n "formal"
      ++ showIndented (n + 1) name
      ++ showIndented (n + 1) typ

instance ShowIndented Expr where
  showIndented n (Assign name value) =
    makeTag n "<-"
      ++ showIndented (n + 1) name
      ++ showIndented (n + 1) value
  showIndented n (ExplicitDispatch obj typ name args) =
    makeTag n "."
      ++ showIndented (n + 1) obj
      ++ maybe "" (showIndented (n + 1)) typ
      ++ showIndented (n + 1) name
      ++ concatMap (showIndented (n + 1)) args
  showIndented n (ImplicitDispatch name args) =
    makeTag n "implicit dispatch"
      ++ showIndented (n + 1) name
      ++ concatMap (showIndented (n + 1)) args
  showIndented n (If cond then' else') =
    makeTag n "if"
      ++ showIndented (n + 1) cond
      ++ showIndented (n + 1) then'
      ++ showIndented (n + 1) else'
  showIndented n (While cond body) =
    makeTag n "while"
      ++ showIndented (n + 1) cond
      ++ showIndented (n + 1) body
  showIndented n (Block exprs) =
    makeTag n "block"
      ++ concatMap (showIndented (n + 1)) exprs
  showIndented n (Let bindings body) =
    makeTag n "let"
      ++ concatMap (showLetBindingIndented (n + 1)) bindings
      ++ showIndented (n + 1) body
  showIndented n (Case expr branches) =
    makeTag n "case"
      ++ showIndented (n + 1) expr
      ++ concatMap (showCaseBranchIndented (n + 1)) branches
  showIndented n (New typ) =
    makeTag n "new"
      ++ showIndented (n + 1) typ
  showIndented n (IsVoid expr) =
    makeTag n "isvoid"
      ++ showIndented (n + 1) expr
  showIndented n (Add lhs rhs) =
    makeTag n "+"
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Subtract lhs rhs) =
    makeTag n "-"
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Multiply lhs rhs) =
    makeTag n "*"
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Divide lhs rhs) =
    makeTag n "/"
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Complement expr) =
    makeTag n "~"
      ++ showIndented (n + 1) expr
  showIndented n (LessThan lhs rhs) =
    makeTag n "<"
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (LessThanEqual lhs rhs) =
    makeTag n "<="
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Equal lhs rhs) =
    makeTag n "="
      ++ showIndented (n + 1) lhs
      ++ showIndented (n + 1) rhs
  showIndented n (Not expr) =
    makeTag n "not"
      ++ showIndented (n + 1) expr
  showIndented n (ID name) = showIndented n name
  showIndented n (IntLiteral value) = makeTag n (show value)
  showIndented n (StringLiteral value) = makeTag n value
  showIndented n (BooleanLiteral True) = makeTag n "true"
  showIndented n (BooleanLiteral False) = makeTag n "false"

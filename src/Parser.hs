module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "if", "else", "while", "true", "false", "int", "bool"]
           , Token.reservedOpNames  = [ "=", "+", "++", "-", "*", "^", "==", "and", "or", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer


data Program  = Program [Instr]
data Instr    = Assign String Expr            -- Assigning a variable
              | While Expr Program
              | IfElse Expr Program Program
              | If Expr Program
              | Print [Printable]

data Expr     = BinOp Op Expr Expr
              | Boolean String                -- A boolean value; either "true" or "false"
              | Val Int                       -- A integer
              | Var String                    -- Using a variable

data Printable = PrintStr String | PrintExp Expr
data Op = Add | Sub | Mult | Pow  -- Integer operators
        | EQS | LTS | LTES        -- Comparison operators
        | And | Or | Not          -- Logical operators

--data Instr    = AssignB String ExprB            -- bool b = true
--              | AssignI String ExprI            -- int a = 10
--              | While ExprB Program             -- while (b) { a = a + 1}
--              | IfElse ExprB Program Program    -- if (b) {int c = 0} else {int c = 1}
--              | If ExprB Program                -- if (b) {int c = 0}
--              | Print [Printable]               -- print("This is a boolean: " ++ b)
--
--data ExprB    = BinOpB  OpB ExprB ExprB         -- true and false
--              | BinOpCB OpC ExprB ExprB         -- true == false
--              | BinOpCI OpC ExprI ExprI         -- a < 10
--              | ValB Bool                       -- false
--              | VarB String                     -- b
--
--data ExprI    = BinOpI OpI ExprI ExprI          -- 2 ^ a
--              | ValI Int                        -- 2
--              | VarI String                     -- a
--
--data Printable = PrintB ExprB | PrintI ExprI | PrintS String
--data OpB = And | Or | Not
--data OpC = EQS | LTS | LTES
--data OpI = Add | Sub | Mult | Pow
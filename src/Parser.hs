module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "if", "else", "while", "true", "false", "int", "bool", "shared", "fork", "lock", "unlock"]
           , Token.reservedOpNames  = [ "=", "+", "-", "*", "^", "==", "and", "or", "not"]
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


type Program  = [Instr]
data Instr    = Decl Scope Type String (Maybe Expr)   -- Declare a variable; Local: int i = 0; Shared: shared int i = o
              | Assign String Expr                    -- Assign a value to a variable; i = 0
              | While Expr Program
              | IfElse Expr Program Program
              | If Expr Program
              | Print Expr
              | Fork (Maybe Int) Program                -- Starts a new thread; fork {}; (Maybe Int) is the number of the thread. While parsing is Nothing, in elaboration is counted
              | Lock String                             -- Locks a lock; lock(i)
              | Unlock String                           -- Unlocks a lock; unlock(i)
              | NotOp Expr
              deriving Show
              
data Expr     = BinOp Op Expr Expr
              | Val Int                               -- A integer
              | Var String                            -- Using a variable
              deriving Show

data Op = AddS | SubS | MultS                         -- Integer operators
        | EQS | LTS | LTES                            -- Comparison operators
        | AndS | OrS                                  -- Logical operators
        deriving Show

data Type = TypeInt | TypeBool | TypeLock deriving Show
data Scope = Local | Shared deriving (Show, Eq)


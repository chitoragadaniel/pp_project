module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "if", "else", "while", "true", "false", "int", "bool", "shared", "fork", "lock", "unlock"]
           , Token.reservedOpNames  = [ "=", "+", "-", "*", "^", "==", "<", "<=", "and", "or", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Int
integer = fromInteger <$> Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer


type Program  = [Instr]
data Instr    = Decl Scope Type String (Maybe Expr)   -- Declare a variable; Local: int i = 0; Shared: shared int i = o;
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
              | Val Int                           -- A integer
              | Var String                            -- Using a variable
              deriving Show

data Op = AddS | SubS | MultS                         -- Integer operators
        | EQS | LTS | LTES                            -- Comparison operators
        | AndS | OrS                                  -- Logical operators
        deriving Show

data Type = TypeInt | TypeBool | TypeLock deriving Show
data Scope = Local | Shared deriving (Show, Eq)



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

-- Parser for a program
parseProgram :: Parser Program
parseProgram =  many parseInstr

-- Parser for a single instruction
parseInstr :: Parser Instr
parseInstr = try (Decl <$> parseScope
                       <*> parseType
                       <*> identifier
                       <*> (optionMaybe (reserved "=" *> parseExpr)))
           <|> try (Assign <$> identifier <*> (reserved "=" *> parseExpr))
           <|> try (While <$> (reserved "while" *> (parens parseExpr))
                          <*> parseProgram)
           <|> try (IfElse <$> (reserved "if" *> (parens parseExpr))
                           <*> (braces parseProgram)
                           <*> (reserved "else" *> (braces parseProgram)))
           <|> try (If <$> (reserved "if" *> (parens parseExpr))
                       <*> (braces parseProgram))
           <|> try (Print <$> (reserved "print" *> (parens parseExpr)))
           <|> try (Fork <$> (reserved "fork" *> pure Nothing) <*> (braces parseProgram))
           <|> try (Lock <$> (reserved "lock" *>  (parens identifier)))
           <|> try (Unlock <$> (reserved "lock" *> (parens identifier)))
           <|> NotOp <$> (reserved "not" *> parseExpr)

parseExpr :: Parser Expr
parseExpr = try ((\left operator right -> (BinOp operator left right)) <$> term <*> parseOp <*> parseExpr)
        <|> try (Val <$> integer)
        <|> try (reserved "true" >> return (Val 1))
        <|> try (reserved "false" >> return (Val 0))
        <|> Var <$> identifier
        where
            term = try (Val <$> integer) <|> (Var <$> identifier) --To avoid infinite recursion

-- Parser for operators
parseOp :: Parser Op
parseOp = try (reserved "+" >> pure AddS)
    <|> try (reserved "-" >> pure SubS)
    <|> try (reserved "*" >> pure MultS)
    <|> try (reserved "==" >> pure EQS)
    <|> try (reserved "<" >> pure LTS)
    <|> try (reserved "<=" >> pure LTES)
    <|> try (reserved "and" >> pure AndS)
    <|> (reserved "or" >> pure OrS)

-- Parser for type
parseType :: Parser Type
parseType = try (reserved "int" >> pure TypeInt)
         <|> try (reserved "bool" >> pure TypeBool)
         <|> (reserved "lock" >> pure TypeLock)

-- Parser for scope
parseScope :: Parser Scope
parseScope = try (reserved "shared" >> pure Shared)
          <|> pure Local
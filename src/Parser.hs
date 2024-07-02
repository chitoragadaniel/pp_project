module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "if", "else", "while", "true", "false", "int", "bool", "shared"]
           , Token.reservedOpNames  = [ "=", "+", "-", "*", "^", "==", "and", "or", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

data Program  = Program [Instr] deriving Show
data Instr    = Decl Scope Type String Expr           -- Declare a variable; Local: int i = 0; Shared: shared int i = o
              | Assign String Expr                    -- Assign a value to a variable; i = 0
              | While Expr Program
              | IfElse Expr Program Program
              | If Expr Program
              | Print Expr
              deriving Show

data Expr     = BinOp Op Expr Expr
              | Val Integer                               -- A integer
              | Var String                            -- Using a variable
              deriving Show


data Op = AddS | SubS | MultS | PowS                  -- Integer operators
        | EQS | LTS | LTES                            -- Comparison operators
        | AndS | OrS | NotS                           -- Logical operators
        deriving Show
data Type = TypeInt | TypeBool deriving Show
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
parseProgram = (Program <$> many parseInstr)

-- Parser for a single instruction
parseInstr :: Parser Instr
parseInstr = try (Decl <$> parseScope
                       <*> parseType
                       <*> identifier
                       <*> (symbol "=" *> parseExpr))
           <|> try (Assign <$> identifier <*> (symbol "=" *> parseExpr))
           <|> try (While <$> (reserved "while" *> (parens parseExpr))
                          <*> parseProgram)
           <|> try (IfElse <$> (reserved "if" *> (parens parseExpr))
                           <*> (braces parseProgram)
                           <*> (reserved "else" *> (braces parseProgram)))
           <|> try (If <$> (reserved "if" *> (parens parseExpr))
                       <*> (braces parseProgram))
           <|> Print <$> (reserved "print" *> (parens parseExpr))

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
    <|> try (reserved "^" >> pure PowS)
    <|> try (reserved "==" >> pure EQS)
    <|> try (reserved "and" >> pure AndS)
    <|> try (reserved "or" >> pure OrS)
    <|> (reserved "not" >> pure NotS)

-- Parser for type
parseType :: Parser Type
parseType = try(reserved "int" >> pure TypeInt)
         <|> (reserved "bool" >> pure TypeBool)

-- Parser for scope
parseScope :: Parser Scope
parseScope = try (reserved "shared" >> pure Shared)
          <|> pure Local
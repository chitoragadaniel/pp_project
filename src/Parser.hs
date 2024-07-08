module Parser where

-- Import Parsec library and its components
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Define the language features and rules for the lexer
languageDef =
  emptyDef { Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum <|> char '_'
           , Token.reservedNames    = [ "if", "else", "while", "true", "false", "int", "bool", "shared", "fork", "lock", "unlock"]
           , Token.reservedOpNames  = [ "=", "+", "-", "*", "==", "<", "<=", "and", "or", "not"]
           }

-- Create a lexer based on the language definition
lexer = Token.makeTokenParser languageDef

-- Define parsers for different components using the lexer
identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Int
integer = fromInteger <$> Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Define the data types for the program and instructions
type Program  = [Instr]
data Instr    = Decl Scope Type String (Maybe Expr)   -- Declare a variable; Local: int i = 0; Shared: shared int i = 0;
              | Assign String Expr                    -- Assign a value to a variable; i = 0
              | While Expr Program                    -- While loop
              | IfElse Expr Program Program           -- If-else statement
              | If Expr Program                       -- If statement
              | Print Expr                            -- Print statement
              | Fork (Maybe Int) Program              -- Starts a new thread; fork {}; (Maybe Int) is the number of the thread. While parsing is Nothing, in elaboration is counted
              | Lock String                           -- Locks a lock; lock(i)
              | Unlock String                         -- Unlocks a lock; unlock(i)
              deriving (Show, Eq)

data Expr     = BinOp Op Expr Expr                    -- Binary operation
              | NotOp Expr                            -- Not operation
              | Val Int                               -- A integer
              | BVal Bool                             -- Boolean value; (Needed for type checking)
              | Var String                            -- Using a variable
              deriving (Show, Eq)

data Op = AddS | SubS | MultS                         -- Integer operators
        | EQS | LTS | LTES                            -- Comparison operators
        | AndS | OrS                                  -- Logical operators
        deriving (Show, Eq)

data Type = TypeInt | TypeBool | TypeLock deriving (Show, Eq)
data Scope = Local | Shared deriving (Show, Eq)

-- Parser for a program (sequence of instructions)
parseProgram :: Parser Program
parseProgram = whiteSpace *> many parseInstr <* eof

-- Parser for a block of code between braces
parseBlock :: Parser Program
parseBlock = braces (whiteSpace *> many parseInstr)

-- Parser for a single instruction
parseInstr :: Parser Instr
parseInstr = try (Decl <$> parseScope
                       <*> parseType
                       <*> identifier
                       <*> (optionMaybe (reserved "=" *> parseExpr)))
           <|> try (Assign <$> identifier <*> (symbol "=" *> parseExpr))
           <|> try (While <$> (reserved "while" *> (parens parseExpr))
                          <*> parseBlock)
           <|> try (IfElse <$> (reserved "if" *> (parens parseExpr))
                           <*> parseBlock
                           <*> (reserved "else" *> parseBlock))
           <|> try (If <$> (reserved "if" *> (parens parseExpr))
                       <*> parseBlock)
           <|> try (Print <$> (reserved "print" *> (parens parseExpr)))
           <|> try (Fork <$> (reserved "fork" *> pure Nothing) <*> parseBlock)
           <|> try (Lock <$> (reserved "lock" *>  (parens identifier)))
           <|> (Unlock <$> (reserved "unlock" *> (parens identifier)))

-- Parser for an expression
parseExpr :: Parser Expr
parseExpr = parseOrExpr

-- Parser for an 'or' expression
parseOrExpr :: Parser Expr
parseOrExpr = try (binOp <$> parseAndExpr <*> parseOrOp <*> parseOrExpr)
          <|> parseAndExpr

-- Parser for an 'and' expression
parseAndExpr :: Parser Expr
parseAndExpr = try (binOp <$> parseComparisonExpr <*> parseAndOp <*> parseAndExpr)
           <|> parseComparisonExpr

-- Parser for a comparison expression
parseComparisonExpr :: Parser Expr
parseComparisonExpr = try (binOp <$> parseAddSubExpr <*> parseComparisonOp <*> parseMultExpr)
                  <|> parseAddSubExpr

-- Parser for an addition or subtraction expression
parseAddSubExpr :: Parser Expr
parseAddSubExpr = try (binOp <$> parseMultExpr <*> parseAddSubOp <*> parseAddSubExpr)
              <|> parseMultExpr

-- Parser for a multiplication expression
parseMultExpr :: Parser Expr
parseMultExpr = try (binOp <$> parseUnaryExpr <*> parseMultOp <*> parseMultExpr)
            <|> parseUnaryExpr

-- Parser for a unary expression (not operation or term)
parseUnaryExpr :: Parser Expr
parseUnaryExpr = try (NotOp <$> (reserved "not" *> parseUnaryExpr)) <|> parseTerm

-- Parser for a term (parenthesized expression, integer value, boolean value, or variable)
parseTerm :: Parser Expr
parseTerm = try (parens parseExpr)
        <|> try (Val <$> integer)
        <|> try (reserved "true" >> return (BVal True))
        <|> try (reserved "false" >> return (BVal False))
        <|> (Var <$> identifier)

-- Parsers for operators
parseAddSubOp :: Parser Op
parseAddSubOp = try (reservedOp "+" >> pure AddS)
            <|> (reservedOp "-" >> pure SubS)

parseMultOp :: Parser Op
parseMultOp = (reservedOp "*" >> pure MultS)

parseComparisonOp :: Parser Op
parseComparisonOp = try (reservedOp "==" >> pure EQS)
                <|> try (reservedOp "<=" >> pure LTES)
                <|> (reservedOp "<" >> pure LTS)

parseAndOp :: Parser Op
parseAndOp = (reservedOp "and" >> pure AndS)

parseOrOp :: Parser Op
parseOrOp = (reservedOp "or" >> pure OrS)

-- Parser for type
parseType :: Parser Type
parseType = try (reserved "int" >> pure TypeInt)
         <|> try (reserved "bool" >> pure TypeBool)
         <|> (reserved "lock" >> pure TypeLock)

-- Parser for scope
parseScope :: Parser Scope
parseScope = try (reserved "shared" >> pure Shared)
          <|> pure Local

-- Helper function that takes an expression, an operator, and another expression, and constructs a new expression.
binOp :: Expr -> Op -> Expr -> Expr
binOp left operator right = BinOp operator left right

-- Function that takes an input string and parses it to a program
runParseProgram :: String -> Program
runParseProgram input =
  case (parse parseProgram "" input) of
      Right prog -> prog
      Left err -> error $ show err
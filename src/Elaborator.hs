module Elaborator where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Parser

-- #####################################################################################################################
-- #                                                   Type Checking                                                   #
-- #####################################################################################################################

-- ContextScope types that represent different levels of scope to keep track of the scope during typechecking
data ContextScope = GlobalScope | ForkScope | ControlScope

-- Variable type: scope and type
type VarType = (Scope, Type)

-- Type environment: map variable names to their types
type TypeEnv = [(String, VarType)]

-- Function to get the type of a variable from the environment
lookupVarType :: String -> TypeEnv -> Either String Type
lookupVarType var env = case lookup var env of
    Just (s, t)  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found in scope."

-- Function to infer the type of an expression
inferExprType :: TypeEnv -> Expr -> Either String Type
inferExprType env (Val _) = Right TypeInt
inferExprType env (BVal _) = Right TypeBool
inferExprType env (Var v) = lookupVarType v env
inferExprType env (NotOp expr) =
    case (inferExprType env expr) of
        Left err -> Left err
        Right t | t == TypeBool -> Right TypeBool
                | otherwise -> Left $ "Type error cannot use unary on expr " ++ show expr
inferExprType env (BinOp op e1 e2) =
    case (op, inferExprType env e1, inferExprType env e2) of
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        (AddS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (SubS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (MultS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (EQS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                  | otherwise -> Left $ "Type error in binary operation " ++ show op
        (LTS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                  | otherwise -> Left $ "Type error in binary operation " ++ show op
        (LTES, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                   | otherwise -> Left $ "Type error in binary operation " ++ show op
        (AndS, Right TypeBool, Right TypeBool) -> Right TypeBool
        (OrS, Right TypeBool, Right TypeBool) -> Right TypeBool
        (_, _, _) -> Left $ "Type error in binary operation " ++ show op

-- Function to filter only shared variables
filterSharedVariables :: TypeEnv -> TypeEnv
filterSharedVariables [] = []
filterSharedVariables ((var, (s,t)) : rest) =
    case s of
        Shared -> (var, (s,t)) : filterSharedVariables rest
        _ -> filterSharedVariables rest

-- Function to check a whole program
checkProgram :: Program -> Either String Program
checkProgram program =
    case checkProg [] program GlobalScope of
        Left err -> Left err
        Right _ -> Right program

-- Function to type check a program
checkProg :: TypeEnv -> Program -> ContextScope -> Either String TypeEnv
checkProg env [] _ = Right env
checkProg env (instr : rest) context =
    case checkInstr env instr context of
        Left err -> Left err
        Right newEnv -> checkProg newEnv rest context

-- Function to type check a single instruction
checkInstr :: TypeEnv -> Instr -> ContextScope -> Either String TypeEnv
checkInstr env (Decl s t var maybeExpr) context =
    case (context, s, t, lookup var env, maybeExpr) of
        (ControlScope, Shared, _, _, _) -> Left $ "Cannot declare shared variable in local scope"
        (ForkScope, Shared, _, _, _) -> Left $ "Cannot declare shared variable in local scope"
        (_, Local, TypeLock, _, _) -> Left $ "Cannot declare lock with local scope"
        (_, _, _, Nothing, _) -> Left $ "Duplicate declaration of variable: " ++ var
        (_, _, _, _, Just expr) -> case inferExprType env expr of
                             Right t' | t == t' -> Right ((var, (s,t)) : env)
                                      | otherwise -> Left $ "Type error in declaration of " ++ var
                             Left err -> Left err
        (_, _, _, _, Nothing) -> Right ((var, (s, t)) : env)
checkInstr env (Assign var expr) context =
    case (lookupVarType var env, inferExprType env expr) of
        (Right t, Right t') | t == t' -> Right env
                            | otherwise -> Left $ "Type error in assignment to " ++ var
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (While expr prog) _ =
    case (inferExprType env expr, checkProg env prog ControlScope) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in if condition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (IfElse expr prog1 prog2) _ =
    case (inferExprType env expr, checkProg env prog1 ControlScope, checkProg env prog2 ControlScope) of
        (Right TypeBool, Right _, Right _) -> Right env
        (Right _, Right _, Right _) -> Left "Type error in if condition"
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
checkInstr env (If expr prog) _ =
    case (inferExprType env expr, checkProg env prog ControlScope) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in if condition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (Print expr) _ =
    case inferExprType env expr of
        Right _ -> Right env
        Left err -> Left err
checkInstr env (Fork _ prog) context =
    case (context, checkProg (filterSharedVariables env) prog ForkScope) of
        (ControlScope, _) -> Left "Cannot enter fork from outside global scope"
        (_, Left err) -> Left err
        (_, Right _) -> Right env
checkInstr env (Lock var) _ =
    case (lookupVarType var env) of
        Left err -> Left err
        Right t | t == TypeLock -> Right env
                | otherwise -> Left $ "Type error in lock instruction to " ++ var
checkInstr env (Unlock var) _=
    case (lookupVarType var env) of
        Left err -> Left err
        Right t | t == TypeLock -> Right env
                | otherwise -> Left $ "Type error in unlock instruction to " ++ var

-- #####################################################################################################################
-- #                                               Program Optimizations                                               #
-- #####################################################################################################################

-- What kind of optimizations:
-- Variable renaming
-- Changes Boolean values into Integer values (true -> 1, false -> 0)
-- Number fork instructions sequentially.

-- Type environment: map variable names to their types
type VarEnv = [(String, String)]

-- Function to get the type of a variable from the environment
lookupVarName :: String -> VarEnv -> Either String String
lookupVarName var env = case lookup var env of
    Just t  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found."

getNewVarName :: VarEnv -> String
getNewVarName env = "$" ++ show (length env)

optimizeProgram :: Program -> Program
optimizeProgram prog =
    case (optimizeProg prog [] 0) of
        Right (prog', _) -> prog'
        Left err -> error err

-- Optimizes a list of instructions
optimizeProg :: Program -> VarEnv -> Int -> Either String (Program, Int)
optimizeProg [] env fc = Right ([], fc)
optimizeProg (instr : rest) env fc =
    case (optimizeInstr instr env fc) of
        Right (instr', env', fc') ->
            case (optimizeProg rest env' fc') of
                Right (rest', fc'') -> Right (instr' : rest', fc'')
                Left err -> Left err
        Left err -> Left err

-- Optimizes a single instruction
optimizeInstr :: Instr -> VarEnv -> Int -> Either String (Instr, VarEnv, Int)
optimizeInstr (Decl scope t name maybeExpr) env fc =
    case maybeExpr of
        Just expr ->
            case (optimizeExpr env expr) of
                Right expr' -> Right (Decl scope t newName (Just expr'), env', fc)
                Left err -> Left err
        Nothing -> Right (Decl scope t newName Nothing, env', fc)
    where
        newName = (getNewVarName env)
        env' = (name, newName) : env
optimizeInstr (Assign name expr) env fc =
    case (lookupVarName name env, optimizeExpr env expr) of
        (Right newName, Right expr') -> Right (Assign newName expr', env, fc)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
optimizeInstr (While expr prog) env fc =
    case (optimizeExpr env expr, optimizeProg prog env fc) of
        (Right expr', Right (prog', fc')) -> Right (While expr' prog', env, fc')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
optimizeInstr (IfElse expr thenProg elseProg) env fc =
    case (optimizeExpr env expr, optimizeProg thenProg env fc) of
        (Right expr', Right (elseProg', fc')) ->
            case (optimizeProg thenProg env fc') of
                Right (thenProg', fc'') -> Right (IfElse expr' thenProg' elseProg', env, fc'')
                Left err -> Left err
        (Left err, _) -> Left err
        (_, Left err) -> Left err
optimizeInstr (If expr prog) env fc =
    case (optimizeExpr env expr, optimizeProg prog env fc) of
        (Right expr', Right (prog', fc')) -> Right (If expr' prog', env, fc')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
optimizeInstr (Print expr) env fc =
    case (optimizeExpr env expr) of
        Right expr' -> Right (Print expr', env, fc)
        Left err -> Left err
optimizeInstr (Fork _ prog) env fc =
    case (optimizeProg prog env (fc + 1)) of
        Right (prog', fc') -> Right (Fork (Just fc) prog', env, fc')
        Left err -> Left err
optimizeInstr (Lock name) env fc =
    case lookupVarName name env of
        Right newName -> Right (Lock newName, env, fc)
        Left err -> Left err
optimizeInstr (Unlock name) env fc =
    case lookupVarName name env of
        Right newName -> Right (Unlock newName, env, fc)
        Left err -> Left err

-- Optimizes an expression
optimizeExpr :: VarEnv -> Expr -> Either String Expr
optimizeExpr env (Val n) = Right (Val n)
optimizeExpr env (BVal True) = Right (Val 1)
optimizeExpr env (BVal False) = Right (Val 0)
optimizeExpr env (Var name) =
  case lookupVarName name env of
    Right newName -> Right (Var newName)
    Left err -> Left err
optimizeExpr env (NotOp expr) =
  case (optimizeExpr env expr) of
    Right e -> Right (NotOp e)
    Left err -> Left err
optimizeExpr env (BinOp op l r) =
  case (optimizeExpr env l, optimizeExpr env r) of
      (Right l', Right r') -> Right (BinOp op l' r')
      (Left err, _) -> Left err
      (_, Left err) -> Left err


exampleProgram = "int a = 10 bool b = true if (b) {int c = 5 while (c < 10) { c = c + 1 print(c) } fork { int d = 3 print(d) } } lock x lock(x) unlock(x)"
parsedExampleProgram = [Decl Local TypeInt "a" (Just (Val 10)),Decl Local TypeBool "b" (Just (BVal True)),If (Var "b") [Decl Local TypeInt "c" (Just (Val 5)),While (BinOp LTS (Var "c") (Val 10)) [Assign "c" (BinOp AddS (Var "c") (Val 1)),Print (Var "c")],Fork Nothing [Decl Local TypeInt "d" (Just (Val 3)),Print (Var "d")]],Decl Local TypeLock "x" Nothing,Lock "x",Unlock "x"]

-- int f1 = 0 fork { int f2 = 1 print(f1) }
-- [Decl Local TypeInt "f1" (Just (Val 0)),Fork Nothing [Decl Local TypeInt "f2" (Just (Val 1)),Print (Var "f1")]]
-- Left "Variable f1 not found."

-- lock l
-- [Decl Local TypeLock "l" Nothing]
-- Left "Cannot declare lock with local scope"

-- int a = 1 if (true) { int a = 2 print(a) } print(a)
-- [Decl Local TypeInt "a" (Just (Val 1)),If (BVal True) [Decl Local TypeInt "a" (Just (Val 2)),Print (Var "a")],Print (Var "a")]
-- Left "Duplicate declaration of variable: a"

-- fork {shared int a}
-- [Fork Nothing [Decl Shared TypeInt "a" Nothing]]
-- Left "Cannot declare shared variable in local scope"

-- if (true) {shared int a}
-- [If (BVal True) [Decl Shared TypeInt "a" Nothing]]
-- Left "Cannot declare shared variable in local scope"

-- if (true) {fork {}}
-- [If (BVal True) [Fork Nothing []]]
-- Left "Cannot enter fork from outside global scope"

-- if(true) { int a = 1 } print(a)
-- [If (BVal True) [Decl Local TypeInt "a" (Just (Val 1))],Print (Var "a")]
-- Left "Variable a not found in scope."

-- print(a) int a = 10
-- [Print (Var "a"),Decl Local TypeInt "a" (Just (Val 10))]
-- Left "Variable a not found in scope."

-- fork { print(a) } shared int a = 10
-- [Fork Nothing [Print (Var "a")],Decl Shared TypeInt "a" (Just (Val 10))]
-- Left "Variable a not found in scope."
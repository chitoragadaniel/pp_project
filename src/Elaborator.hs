module Elaborator where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Parser

-- #####################################################################################################################
-- #                                       Checking for duplicate variable names                                       #
-- #####################################################################################################################

-- Function to check for duplicate declarations
checkDuplicates :: Program -> Either String Bool
checkDuplicates instrs = checkInstrs instrs []

checkInstrs :: [Instr] -> [String] -> Either String Bool
checkInstrs [] _ = Right True
checkInstrs (Decl _ _ var _ : rest) vars
    | elem var vars = Left $ "Duplicate declaration of variable: " ++ var
    | otherwise = checkInstrs rest (var : vars)
checkInstrs (While _ whileProg : rest) vars =
    case checkInstrs whileProg vars of
        Left err -> Left err
        Right _ -> checkInstrs rest vars
checkInstrs (IfElse _ thenProg elseProg : rest) vars =
    case (checkInstrs thenProg vars, checkInstrs elseProg vars) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right _, Right _) -> checkInstrs rest vars
checkInstrs (If _ thenProg : rest) vars =
    case checkInstrs thenProg vars of
        Left err -> Left err
        Right _ -> checkInstrs rest vars
checkInstrs (Fork _ forkProg : rest) vars =
    case checkInstrs forkProg vars of
        Left err -> Left err
        Right _ -> checkInstrs rest vars
checkInstrs (_ : rest) vars = checkInstrs rest vars

-- #####################################################################################################################
-- #                                                   Type Checking                                                   #
-- #####################################################################################################################

-- Function to type check a program
checkProgram :: TypeEnv -> Program -> Either String TypeEnv
checkProgram env [] = Right env
checkProgram env (instr : rest) =
    case checkInstr env instr of
        Left err -> Left err
        Right newEnv -> checkProgram newEnv rest

-- Type environment: map variable names to their types
type TypeEnv = [(String, Type)]

-- Function to get the type of a variable from the environment
lookupVarType :: String -> TypeEnv -> Either String Type
lookupVarType var env = case lookup var env of
    Just t  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found."

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

-- Function to type check a single instruction
checkInstr :: TypeEnv -> Instr -> Either String TypeEnv
checkInstr env (Decl _ t var maybeExpr) =
    case maybeExpr of
        Just expr -> case inferExprType env expr of
                             Right t' | t == t' -> Right ((var, t) : env)
                                      | otherwise -> Left $ "Type error in declaration of " ++ var
                             Left err -> Left err
        Nothing -> Right ((var, t) : env)
checkInstr env (Assign var expr) =
    case (lookupVarType var env, inferExprType env expr) of
        (Right t, Right t') | t == t' -> Right env
                            | otherwise -> Left $ "Type error in assignment to " ++ var
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (While expr prog) =
    case (inferExprType env expr, checkProgram env prog) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in if condition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (IfElse expr prog1 prog2) =
    case (inferExprType env expr, checkProgram env prog1, checkProgram env prog2) of
        (Right TypeBool, Right _, Right _) -> Right env
        (Right _, Right _, Right _) -> Left "Type error in if condition"
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
checkInstr env (If expr prog) =
    case (inferExprType env expr, checkProgram env prog) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in if condition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (Print expr) =
    case inferExprType env expr of
        Right _ -> Right env
        Left err -> Left err
checkInstr env (Fork _ prog) =
    case (checkProgram env prog) of
        Left err -> Left err
        Right _ -> Right env
checkInstr env (Lock var) =
    case (lookupVarType var env) of
        Left err -> Left err
        Right t | t == TypeLock -> Right env
                | otherwise -> Left $ "Type error in lock instruction to " ++ var
checkInstr env (Unlock var) =
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

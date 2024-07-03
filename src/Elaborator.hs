module Elaborator where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Parser

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

--TypeChecking:

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
inferExprType env (Var v) = lookupVarType v env
inferExprType env (NotOp expr) =
    case (inferExprType env expr) of
        Left err -> Left err
        Right _ -> Right TypeBool
inferExprType env (BinOp op e1 e2) =
    case (op, inferExprType env e1, inferExprType env e2) of
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        (AddS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (SubS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (MultS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (EQS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
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
    case inferExprType env expr of
        Right TypeBool -> checkProgram env prog
        Right _ -> Left "Type error in while condition"
        Left err -> Left err
checkInstr env (IfElse expr prog1 prog2) =
    case inferExprType env expr of
        Right TypeBool -> case checkProgram env prog1 of
                            Right env' -> checkProgram env' prog2
                            Left err -> Left err
        Right _ -> Left "Type error in if condition"
        Left err -> Left err
checkInstr env (If expr prog) =
    case inferExprType env expr of
        Right TypeBool -> checkProgram env prog
        Right _ -> Left "Type error in if condition"
        Left err -> Left err
checkInstr env (Print expr) =
    case inferExprType env expr of
        Right _ -> Right env
        Left err -> Left err
checkInstr env (Fork _ prog) = checkProgram env prog

-- Function to type check a program
checkProgram :: TypeEnv -> Program -> Either String TypeEnv
checkProgram env [] = Right env
checkProgram env (instr : rest) =
    case checkInstr env instr of
        Left err -> Left err
        Right newEnv -> checkProgram newEnv rest
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
-- Inputs:
--    var: name of variable to lookup its new variable name.
--    env: TypeEnv -> List of variable names and their respective scope and type.
-- Outputs:
--    Either variable type or error message.
lookupVarType :: String -> TypeEnv -> Either String Type
lookupVarType var env = case lookup var env of
    Just (s, t)  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found in scope"                                                       -- Error case: variable not found

-- Function to infer the type of an expression.
-- Inputs:
--    env: TypeEnv -> List of variable names and their respective scope and type.
--    expression: Expr -> expression of which type is inferred.
-- Outputs:
--    Either expression type or error message.
inferExprType :: TypeEnv -> Expr -> Either String Type
inferExprType env (Val _) = Right TypeInt
inferExprType env (BVal _) = Right TypeBool
inferExprType env (Var v) = lookupVarType v env
inferExprType env (NotOp expr) =
    case (inferExprType env expr) of
        Left err -> Left err                                                                                            -- Propagate error from inner expression type inference
        Right t | t == TypeBool -> Right TypeBool
                | otherwise -> Left $ "Type error: cannot use unary NOT on expr " ++ show expr                          -- Error case: type mismatch
inferExprType env (BinOp op e1 e2) =
    case (op, inferExprType env e1, inferExprType env e2) of
        (_, Left err, _) -> Left err                                                                                    -- Propagate error from left expression type inference
        (_, _, Left err) -> Left err                                                                                    -- Propagate error from right expression type inference
        (AddS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (SubS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (MultS, Right TypeInt, Right TypeInt) -> Right TypeInt
        (EQS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                  | otherwise -> Left $ "Type error in binary operation " ++ show op                    -- Error case: type mismatch
        (LTS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                  | otherwise -> Left $ "Type error in binary operation " ++ show op                    -- Error case: type mismatch
        (LTES, Right t1, Right t2) | t1 == t2 -> Right TypeBool
                                   | otherwise -> Left $ "Type error in binary operation " ++ show op                   -- Error case: type mismatch
        (AndS, Right TypeBool, Right TypeBool) -> Right TypeBool
        (OrS, Right TypeBool, Right TypeBool) -> Right TypeBool
        (_, _, _) -> Left $ "Type error in binary operation " ++ show op                                                -- Error case: invalid types for operation

-- Function to filter only shared variables.
-- Inputs:
--    env: TypeEnv -> List of variable names and their respective scope and type.
-- Outputs:
--    updated env -> Only variables with shared scopes.
filterSharedVariables :: TypeEnv -> TypeEnv
filterSharedVariables [] = []
filterSharedVariables ((var, (s,t)) : rest) =
    case s of
        Shared -> (var, (s,t)) : filterSharedVariables rest
        _ -> filterSharedVariables rest

-- Function to check a whole program.
-- Inputs:
--    program: program that needs to be type checked.
-- Outputs:
--    program: the same unchanged program.
-- Throws:
--    error when program is invalid.
checkProgram :: Program -> Program
checkProgram program =
    case checkProg [] program GlobalScope of
        Left err -> error err                                                                                           -- Error case: type checking failed
        Right _ -> program

-- Function to type check a list of instructions.
-- Inputs:
--    env: TypeEnv -> List of variable names and their respective scope and type.
--    List of Instructions
--    context: ContextScope -> Current Scope in which type checking is done
-- Outputs:
--    Either Updated env or error message.
checkProg :: TypeEnv -> Program -> ContextScope -> Either String TypeEnv
checkProg env [] _ = Right env
checkProg env (instr : rest) context =
    case checkInstr env instr context of
        Left err -> Left err                                                                                            -- Propagate error from instruction checking
        Right newEnv -> checkProg newEnv rest context

-- Function to type check a single instruction.
-- Inputs:
--    env: TypeEnv -> List of variable names and their respective scope and type.
--    instruction: Instruction that is checked.
--    context: ContextScope -> Current Scope in which type checking is done
-- Outputs:
--    Either Updated env or error message.
checkInstr :: TypeEnv -> Instr -> ContextScope -> Either String TypeEnv
checkInstr env (Decl s t var maybeExpr) context =
    case (context, s, t, lookup var env, maybeExpr) of
        (ControlScope, Shared, _, _, _) -> Left $ "Cannot declare shared variable in local scope"                       -- Error case: shared variable in invalid scope
        (ForkScope, Shared, _, _, _) -> Left $ "Cannot declare shared variable in local scope"                          -- Error case: shared variable in invalid scope
        (_, Local, TypeLock, _, _) -> Left $ "Cannot declare lock with local scope"                                     -- Error case: invalid type
        (_, _, _, Just _, _) -> Left $ "Duplicate declaration of variable: " ++ var                                     -- Error case: duplicate declaration
        (_, _, _, _, Just expr) -> case inferExprType env expr of
                             Right t' | t == t' -> Right ((var, (s,t)) : env)
                                      | otherwise -> Left $ "Type error in declaration of " ++ var                      -- Error case: type mismatch
                             Left err -> Left err                                                                       -- Propagate error from expression type inference
        (_, _, _, _, Nothing) -> Right ((var, (s, t)) : env)
checkInstr env (Assign var expr) context =
    case (lookupVarType var env, inferExprType env expr) of
        (Right t, Right t') | t == t' -> Right env
                            | otherwise -> Left $ "Type error in assignment to " ++ var                                 -- Error case: type mismatch
        (Left err, _) -> Left err                                                                                       -- Propagate error from variable lookup
        (_, Left err) -> Left err                                                                                       -- Propagate error from expression type inference
checkInstr env (While expr prog) _ =
    case (inferExprType env expr, checkProg env prog ControlScope) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in While condition"                                                      -- Error case: condition is not boolean
        (Left err, _) -> Left err                                                                                       -- Propagate error from condition type inference
        (_, Left err) -> Left err                                                                                       -- Propagate error from body type checking
checkInstr env (IfElse expr prog1 prog2) _ =
    case (inferExprType env expr, checkProg env prog1 ControlScope, checkProg env prog2 ControlScope) of
        (Right TypeBool, Right _, Right _) -> Right env
        (Right _, Right _, Right _) -> Left "Type error in if else condition"                                           -- Error case: condition is not boolean
        (Left err, _, _) -> Left err                                                                                    -- Propagate error from condition type inference
        (_, Left err, _) -> Left err                                                                                    -- Propagate error from then branch type checking
        (_, _, Left err) -> Left err                                                                                    -- Propagate error from else branch type checking
checkInstr env (If expr prog) _ =
    case (inferExprType env expr, checkProg env prog ControlScope) of
        (Right TypeBool, Right _) -> Right env
        (Right _, Right _) -> Left "Type error in if condition"                                                         -- Error case: condition is not boolean
        (Left err, _) -> Left err                                                                                       -- Propagate error from condition type inference
        (_, Left err) -> Left err                                                                                       -- Propagate error from body type checking
checkInstr env (Print expr) _ =
    case inferExprType env expr of
        Right _ -> Right env
        Left err -> Left err                                                                                            -- Propagate error from expression type inference
checkInstr env (Fork _ prog) context =
    case (context, checkProg (filterSharedVariables env) prog ForkScope) of
        (ControlScope, _) -> Left "Cannot enter fork from outside global scope"                                         -- Error case: invalid scope
        (_, Left err) -> Left err                                                                                       -- Propagate error from body type checking
        (_, Right _) -> Right env
checkInstr env (Lock var) _ =
    case (lookupVarType var env) of
        Left err -> Left err                                                                                            -- Propagate error from variable lookup
        Right t | t == TypeLock -> Right env
                | otherwise -> Left $ "Type error in lock instruction to " ++ var                                       -- Error case: type mismatch
checkInstr env (Unlock var) _=
    case (lookupVarType var env) of
        Left err -> Left err                                                                                            -- Propagate error from variable lookup
        Right t | t == TypeLock -> Right env
                | otherwise -> Left $ "Type error in unlock instruction to " ++ var                                     -- Error case: type mismatch

-- #####################################################################################################################
-- #                                                Program Elaboration                                                #
-- #####################################################################################################################

-- What kind of elaborating are done for programs:
-- Variable renaming                                                    -> Done in "elaborateProgram".
-- Changes Boolean values into Integer values (true -> 1, false -> 0)   -> Done in "elaborateProgram".
-- Number fork instructions sequentially.                               -> Done in "removeBValFromProgram".

-- Variable environment: map variable names to their new types during elaboration.
type VarEnv = [(String, String)]

-- Function to get the elaborated name of a variable from the environment
-- Inputs:
--    var: name of variable to lookup its new variable name.
--    env: Variable environnement that stores variable names and their respective elaborated variable name.
-- Outputs:
--    elaborated variable name or error message.
lookupVarName :: String -> VarEnv -> Either String String
lookupVarName var env = case lookup var env of
    Just t  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found."                                                               -- Error case: variable not found

-- Elaborates a Program or throws an error.
-- Inputs:
--    Program that needs to be elaborated.
-- Outputs:
--    Elaborated Program.
-- Throws:
--    Error when program is invalid.
elaborateProgram :: Program -> Program
elaborateProgram prog =
    case (elaborateProg prog [] [] 1 0) of
        Right (prog', _, _) -> prog'                                                                                    -- Successful elaboration: return optimized program
        Left err -> error err                                                                                           -- Error case: elaboration failed, throw an error

-- Elaborates a list of instructions within a program.
-- Inputs:
--   prog: Instructions to be elaborated.
--   outerEnv: List with variable names and their new names from the outer scope.
--   innerEnv: List with variable names and their new names from the inner scope.
--   fc: Fork counter to keep track of the number of forks in a program.
--   vc: Variable counter to keep track of the number of declared variables in a program.
-- Outputs:
--   Either Error message or:
--     Elaborated instructions.
--     Updated fork counter.
--     Updated variable counter.
elaborateProg :: Program -> VarEnv -> VarEnv -> Int -> Int -> Either String (Program, Int, Int)
elaborateProg [] outerEnv innerEnv fc vc = Right ([], fc, vc)                                                           -- Base case: empty program, return empty result
elaborateProg (instr : rest) outerEnv innerEnv fc vc =
    case (elaborateInstr instr outerEnv innerEnv fc vc) of
        Right (instr', innerEnv', fc', vc') ->
            case (elaborateProg rest outerEnv innerEnv' fc' vc') of
                Right (rest', fc'', vc'') -> Right (instr' : rest', fc'', vc'')                                         -- Recursively process remaining instructions
                Left err -> Left err                                                                                    -- Propagate error from inner elaboration
        Left err -> Left err                                                                                            -- Propagate error from current instruction

-- Elaborates a single instruction within a program.
-- Inputs:
--   instr: Instruction to be elaborated
--   outerEnv: List with variable names and their new names from the outer scope.
--   innerEnv: List with variable names and their new names from the inner scope.
--   fc: Fork counter to keep track of the number of forks in a program.
--   vc: Variable counter to keep track of the number of declared variables in a program.
-- Outputs:
--   Either Error message or:
--     Elaborated instruction.
--     Updated list with variables names and their new names from the inner scope.
--     Updated fork counter.
--     Updated variable counter.
elaborateInstr :: Instr -> VarEnv -> VarEnv -> Int -> Int -> Either String (Instr, VarEnv, Int, Int)
elaborateInstr (Decl scope t name maybeExpr) outerEnv innerEnv fc vc =
    case (lookupVarName name innerEnv, maybeExpr) of
        (Right _, _) -> Left $ "Duplicate declaration of var: " ++ name                                                 -- Error case: duplicate declaration of variable in nested scope
        (_, Just expr) ->
            case (elaborateExpr outerEnv innerEnv expr) of
                Right expr' -> Right (Decl scope t newName (Just expr'), innerEnv', fc, vc')
                Left err -> Left err                                                                                    -- Propagate error from expression elaboration
        (_, Nothing) -> Right (Decl scope t newName Nothing, innerEnv', fc, vc')
    where
        newName = "$" ++ show (vc)
        innerEnv' = (name, newName) : innerEnv
        vc' = vc + 1
elaborateInstr (Assign name expr) outerEnv innerEnv fc vc =
    case (lookupVarName name innerEnv) of
        Left _ ->
            case (lookupVarName name outerEnv, elaborateExpr outerEnv innerEnv expr) of
                (Right newName, Right expr') -> Right (Assign newName expr', innerEnv, fc, vc)
                (Left err, _) -> Left err                                                                               -- Propagate error from outer environment lookup
                (_, Left err) -> Left err                                                                               -- Propagate error from expression elaboration
        Right newName ->
            case (elaborateExpr outerEnv innerEnv expr) of
                Right expr' -> Right (Assign newName expr', innerEnv, fc, vc)
                Left err -> Left err                                                                                    -- Propagate error from expression elaboration
elaborateInstr (While expr prog) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg prog (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (prog', fc', vc')) -> Right (While expr' prog', innerEnv, fc', vc')
        (Left err, _) -> Left err                                                                                       -- Propagate error from expression elaboration
        (_, Left err) -> Left err                                                                                       -- Propagate error from program elaboration
elaborateInstr (IfElse expr thenProg elseProg) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg thenProg (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (thenProg', fc', vc')) ->
            case (elaborateProg elseProg (outerEnv ++ innerEnv) [] fc' vc') of
                Right (elseProg', fc'', vc'') -> Right (IfElse expr' thenProg' elseProg', innerEnv, fc'', vc'')
                Left err -> Left err                                                                                    -- Propagate error from program elaboration
        (Left err, _) -> Left err                                                                                       -- Propagate error from expression elaboration
        (_, Left err) -> Left err                                                                                       -- Propagate error from program elaboration
elaborateInstr (If expr prog) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg prog (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (prog', fc', vc')) -> Right (If expr' prog', innerEnv, fc', vc')
        (Left err, _) -> Left err                                                                                       -- Propagate error from expression elaboration
        (_, Left err) -> Left err                                                                                       -- Propagate error from program elaboration

elaborateInstr (Print expr) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr) of
        Right expr' -> Right (Print expr', innerEnv, fc, vc)
        Left err -> Left err                                                                                            -- Propagate error from expression elaboration
elaborateInstr (Fork _ prog) outerEnv innerEnv fc vc =
    case (elaborateProg prog (outerEnv ++ innerEnv) [] (fc + 1) vc) of
        Right (prog', fc', vc') -> Right (Fork (Just fc) prog', innerEnv, fc', vc')
        Left err -> Left err                                                                                            -- Propagate error from program elaboration
elaborateInstr (Lock name) outerEnv innerEnv fc vc =
    case lookupVarName name innerEnv of
        Left _ ->
            case lookupVarName name outerEnv of
                Right newName -> Right (Lock newName, innerEnv, fc, vc)
                Left err -> Left err                                                                                    -- Propagate error from outer environment lookup
        Right newName -> Right (Lock newName, innerEnv, fc, vc)
elaborateInstr (Unlock name) outerEnv innerEnv fc vc =
    case lookupVarName name innerEnv of
        Left _ ->
            case lookupVarName name outerEnv of
                Right newName -> Right (Unlock newName, innerEnv, fc, vc)
                Left err -> Left err                                                                                    -- Propagate error from outer environment lookup
        Right newName -> Right (Unlock newName, innerEnv, fc, vc)

-- Elaborates an expression.
-- Inputs:
--   outerEnv: List with variable names and their new names from the outer scope.
--   innerEnv: List with variable names and their new names from the inner scope.
--   expr: Expression to be elaborated.
-- Outputs:
--   Either elaborated expression or error message.
elaborateExpr :: VarEnv -> VarEnv -> Expr -> Either String Expr
elaborateExpr _ _ (Val n) = Right (Val n)
elaborateExpr _ _ (BVal b) = Right (BVal b)
elaborateExpr outerEnv innerEnv (Var name) =
  case lookupVarName name innerEnv of
    Left _ ->
      case lookupVarName name outerEnv of
          Right newName -> Right (Var newName)
          Left err -> Left err                                                                                          -- Propagate error from outer environment lookup
    Right newName -> Right (Var newName)
elaborateExpr outerEnv innerEnv (NotOp expr) =
  case (elaborateExpr outerEnv innerEnv expr) of
    Right e -> Right (NotOp e)
    Left err -> Left err                                                                                                -- Propagate error from inner expression elaboration
elaborateExpr outerEnv innerEnv (BinOp op l r) =
  case (elaborateExpr outerEnv innerEnv l, elaborateExpr outerEnv innerEnv r) of
      (Right l', Right r') -> Right (BinOp op l' r')
      (Left err, _) -> Left err                                                                                         -- Propagate error from left expression elaboration
      (_, Left err) -> Left err                                                                                         -- Propagate error from right expression elaboration

-- Function to remove BVal from program and replace with either Val 0 (false) or Val 1 (true).
-- Inputs:
--   Program from which BVal needs to be removed.
-- Outputs:
--   Program without BVal.

removeBValFromProgram :: Program -> Program
removeBValFromProgram [] = []
removeBValFromProgram (instr : rest) = (removeBValFromInstr instr) : (removeBValFromProgram rest)

-- Function to remove BVal from an instruction and replace with either Val 0 (false) or Val 1 (true).
-- Inputs:
--   Instruction from which BVal needs to be removed.
-- Outputs:
--   Instruction without BVal.
removeBValFromInstr :: Instr -> Instr
removeBValFromInstr (Decl scope t name maybeExpr) =
    case maybeExpr of
        Nothing -> Decl scope t name Nothing
        Just expr -> Decl scope t name (Just (removeBValFromExpr expr))
removeBValFromInstr (Assign name expr) = Assign name (removeBValFromExpr expr)
removeBValFromInstr (While expr prog) = While (removeBValFromExpr expr) (removeBValFromProgram prog)
removeBValFromInstr (IfElse expr thenProg elseProg) = IfElse (removeBValFromExpr expr) (removeBValFromProgram thenProg) (removeBValFromProgram elseProg)
removeBValFromInstr (If expr prog) = If (removeBValFromExpr expr) (removeBValFromProgram prog)
removeBValFromInstr (Print expr) = Print (removeBValFromExpr expr)
removeBValFromInstr (Fork maybeInt prog) = Fork maybeInt (removeBValFromProgram prog)
removeBValFromInstr instr = instr

-- Function to remove BVal from an expression and replace with either Val 0 (false) or Val 1 (true).
-- Inputs:
--   Expression from which BVal needs to be removed.
-- Outputs:
--   Expression without BVal.
removeBValFromExpr :: Expr -> Expr
removeBValFromExpr (Val n) = Val n
removeBValFromExpr (Var n) = Var n
removeBValFromExpr (BVal True) = Val 1
removeBValFromExpr (BVal False) = Val 0
removeBValFromExpr (NotOp e) = NotOp (removeBValFromExpr e)
removeBValFromExpr (BinOp op l r) = BinOp op (removeBValFromExpr l) (removeBValFromExpr r)
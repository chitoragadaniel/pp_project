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
    Nothing -> Left $ "Variable " ++ var ++ " not found in scope"

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
checkProgram :: Program -> Program
checkProgram program =
    case checkProg [] program GlobalScope of
        Left err -> error err
        Right _ -> program

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
        (_, _, _, Just _, _) -> Left $ "Duplicate declaration of variable: " ++ var
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
        (Right _, Right _) -> Left "Type error in While condition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
checkInstr env (IfElse expr prog1 prog2) _ =
    case (inferExprType env expr, checkProg env prog1 ControlScope, checkProg env prog2 ControlScope) of
        (Right TypeBool, Right _, Right _) -> Right env
        (Right _, Right _, Right _) -> Left "Type error in if else condition"
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
-- #                                                Program Elaboration                                                #
-- #####################################################################################################################

-- What kind of optimizations:
-- Variable renaming
-- Changes Boolean values into Integer values (true -> 1, false -> 0)
-- Number fork instructions sequentially.

-- Type environment: map variable names to their new types
type VarEnv = [(String, String)]

-- Function to get the type of a variable from the environment
lookupVarName :: String -> VarEnv -> Either String String
lookupVarName var env = case lookup var env of
    Just t  -> Right t
    Nothing -> Left $ "Variable " ++ var ++ " not found."

-- Function to generate a new variable name
getNewVarName :: VarEnv -> String
getNewVarName env = "$" ++ show (length env)

-- Optimizes a Program or throws an error
elaborateProgram :: Program -> Program
elaborateProgram prog =
    case (elaborateProg prog [] [] 1 0) of
        Right (prog', _, _) -> prog'
        Left err -> error err

-- Optimizes a list of instructions
-- Inputs:
  -- instructions to be elaborated
  -- List with variables names and their new names from the outer scope:  - outerEnv
  -- List with variables names and their new names from the inner scope:  - innerEnv
  -- Counter for forks:                                                   - fc
  -- Counter for variables:                                               - vc
-- Outputs:
  -- Either Error message or:
    -- Optimized instructions
    -- Updated fork counter
    -- Updated variable counter

elaborateProg :: Program -> VarEnv -> VarEnv-> Int -> Int -> Either String (Program, Int, Int)
elaborateProg [] outerEnv innerEnv fc vc = Right ([], fc, vc)
elaborateProg (instr : rest) outerEnv innerEnv fc vc =
    case (elaborateInstr instr outerEnv innerEnv fc vc) of
        Right (instr', innerEnv', fc', vc') ->
            case (elaborateProg rest outerEnv innerEnv' fc' vc') of
                Right (rest', fc'', vc'') -> Right (instr' : rest', fc'', vc'')
                Left err -> Left err
        Left err -> Left err

-- Optimizes a single instruction
-- Inputs:
  -- instructions to be elaborated
  -- List with variables names and their new names from the outer scope:  - outerEnv
  -- List with variables names and their new names from the inner scope:  - innerEnv
  -- Counter for forks:                                                   - fc
  -- Counter for variables:                                               - vc
-- Outputs:
  -- Either Error message or:
    -- Optimized instructions
    -- Updated list with variables names and their new names from the inner scope
    -- Updated fork counter
    -- Updated variable counter
elaborateInstr :: Instr -> VarEnv -> VarEnv-> Int -> Int -> Either String (Instr, VarEnv, Int, Int)
elaborateInstr (Decl scope t name maybeExpr) outerEnv innerEnv fc vc =
    case maybeExpr of
        Just expr ->
            case (elaborateExpr outerEnv innerEnv expr) of
                Right expr' -> Right (Decl scope t newName (Just expr'), innerEnv', fc, vc')
                Left err -> Left err
        Nothing -> Right (Decl scope t ("$" ++ show (vc)) Nothing, innerEnv', fc, vc')
    where
        newName = "$" ++ show (vc)
        innerEnv' = (name, newName) : innerEnv
        vc' = vc + 1
elaborateInstr (Assign name expr) outerEnv innerEnv fc vc =
    case (lookupVarName name innerEnv) of
        Left _ ->
            case (lookupVarName name outerEnv, elaborateExpr outerEnv innerEnv expr) of
                (Right newName, Right expr') -> Right (Assign newName expr', innerEnv, fc, vc)
                (Left err, _) -> Left err
                (_, Left err) -> Left err
        Right newName ->
            case (elaborateExpr outerEnv innerEnv expr) of
                Right expr' -> Right (Assign newName expr', innerEnv, fc, vc)
                Left err -> Left err
elaborateInstr (While expr prog) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg prog (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (prog', fc', vc')) -> Right (While expr' prog', innerEnv, fc', vc')
        (Left err, _) -> Left err
        (_, Left err) -> Left err
elaborateInstr (IfElse expr thenProg elseProg) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg thenProg (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (thenProg', fc', vc')) ->
            case (elaborateProg elseProg (outerEnv ++ innerEnv) [] fc' vc') of
                Right (elseProg', fc'', vc'') -> Right (IfElse expr' thenProg' elseProg', innerEnv, fc'', vc'')
                Left err -> Left err
        (Left err, _) -> Left err
        (_, Left err) -> Left err
elaborateInstr (If expr prog) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr, elaborateProg prog (outerEnv ++ innerEnv) [] fc vc) of
        (Right expr', Right (prog', fc', vc')) -> Right (If expr' prog', innerEnv, fc', vc')
        (Left err, _) -> Left err
        (_, Left err) -> Left err

elaborateInstr (Print expr) outerEnv innerEnv fc vc =
    case (elaborateExpr outerEnv innerEnv expr) of
        Right expr' -> Right (Print expr', innerEnv, fc, vc)
        Left err -> Left err
elaborateInstr (Fork _ prog) outerEnv innerEnv fc vc =
    case (elaborateProg prog (outerEnv ++ innerEnv) [] (fc + 1) vc) of
        Right (prog', fc', vc') -> Right (Fork (Just fc) prog', innerEnv, fc', vc')
        Left err -> Left err
elaborateInstr (Lock name) outerEnv innerEnv fc vc =
    case lookupVarName name innerEnv of
        Left _ ->
            case lookupVarName name outerEnv of
                Right newName -> Right (Lock newName, innerEnv, fc, vc)
                Left err -> Left err
        Right newName -> Right (Lock newName, innerEnv, fc, vc)
elaborateInstr (Unlock name) outerEnv innerEnv fc vc =
    case lookupVarName name innerEnv of
        Left _ ->
            case lookupVarName name outerEnv of
                Right newName -> Right (Unlock newName, innerEnv, fc, vc)
                Left err -> Left err
        Right newName -> Right (Unlock newName, innerEnv, fc, vc)

-- Optimizes an expression
-- Inputs:
  -- List with variables names and their new names from the outer scope:  - outerEnv
  -- List with variables names and their new names from the inner scope:  - innerEnv
  -- Expression to be elaborated
-- Outputs:
  -- Either elaborated expression or error message
elaborateExpr :: VarEnv -> VarEnv -> Expr -> Either String Expr
elaborateExpr _ _ (Val n) = Right (Val n)
elaborateExpr _ _ (BVal b) = Right (BVal b)
elaborateExpr outerEnv innerEnv (Var name) =
  case lookupVarName name innerEnv of
    Left _ ->
      case lookupVarName name outerEnv of
          Right newName -> Right (Var newName)
          Left err -> Left err
    Right newName -> Right (Var newName)
elaborateExpr outerEnv innerEnv (NotOp expr) =
  case (elaborateExpr outerEnv innerEnv expr) of
    Right e -> Right (NotOp e)
    Left err -> Left err
elaborateExpr outerEnv innerEnv (BinOp op l r) =
  case (elaborateExpr outerEnv innerEnv l, elaborateExpr outerEnv innerEnv r) of
      (Right l', Right r') -> Right (BinOp op l' r')
      (Left err, _) -> Left err
      (_, Left err) -> Left err

removeBValFromProgram :: Program -> Program
removeBValFromProgram [] = []
removeBValFromProgram (instr : rest) = (removeBValFromInstr instr) : (removeBValFromProgram rest)

removeBValFromInstr :: Instr -> Instr
removeBValFromInstr (Decl scope t name maybeExpr) =
    case maybeExpr of
        Nothing -> Decl scope t name Nothing
        Just expr -> Decl scope t name (Just (removeBValFromExpr expr))
removeBValFromInstr (Assign name expr) = Assign name (removeBValFromExpr expr)
removeBValFromInstr (While expr prog) = While (removeBValFromExpr expr) prog
removeBValFromInstr (IfElse expr thenProg elseProg) = IfElse (removeBValFromExpr expr) thenProg elseProg
removeBValFromInstr (If expr prog) = If (removeBValFromExpr expr) prog
removeBValFromInstr (Print expr) = Print (removeBValFromExpr expr)
removeBValFromInstr instr = instr

removeBValFromExpr :: Expr -> Expr
removeBValFromExpr (Val n) = Val n
removeBValFromExpr (Var n) = Var n
removeBValFromExpr (BVal True) = Val 1
removeBValFromExpr (BVal False) = Val 0
removeBValFromExpr (NotOp e) = NotOp (removeBValFromExpr e)
removeBValFromExpr (BinOp op l r) = BinOp op (removeBValFromExpr l) (removeBValFromExpr r)
module Elaborator where

-- Function to check for duplicate declarations
checkDuplicates :: Program -> Either String Program
checkDuplicates (Program instrs) = checkInstrs (Program instrs) instrs []

checkInstrs :: Program -> [Instr] -> [String] -> Either String Program
checkInstrs prog [] _ = Right prog
checkInstrs prog (Decl _ _ var _ : rest) vars
      | elem var vars = Left $ "Duplicate declaration of variable: " ++ var
      | otherwise = checkInstrs prog rest (var : vars)
checkInstrs _ (_ : rest) vars = checkInstrs rest vars

--renameVariables :: Program -> Program
--renameVariables = (Program instrs) = Program (renameInstrs instrs [])
renameInstrs [Instr] -> [String] -> Integer -> Either String [Instr]
renameInstrs [] _ _ = Right []
renameInstrs (Decl scope varType var expr : rest) vars counter
  | isJust index = Right (Decl scope ("$" ++ index) var expr)  : (renameInstrs rest vars counter)
  | otherwise = Right (Decl scope ("$" ++ counter) var expr) : (renameInstrs rest (vars ++ var) (counter + 1))
  where index = elemIndex var vars
renameInstrs (Assign var expr : rest) vars counter
  | isJust index = Right (Assign ("$" ++ index) (renameExpr expr vars)) : (renameInstrs rest vars counter)
  | otherwise = : Left $ "Use of variable before declaration: " ++ var
  where index = elemIndex var vars
renameInstrs (While expr prog: rest) vars counter =
  (While (renameExpr expr vars) prog) : (renameInstrs rest vars counter)
renameInstrs (IfElse expr prog1 prog2 : rest) vars counter =
  (IfElse (renameExpr expr vars) prog1 prog2: rest) : (renameInstrs rest vars counter)
renameInstrs (If expr prog : rest) =
  (If (renameExpr expr vars) prog) : (renameInstrs rest vars counter)

renameExpr :: Expr -> [String] -> Either String Expr
renameExpr (BinOp op expr1 expr2) vars
  | isLeft renamedExpr1 = Left $ "Use of variable before declaration"
  | isLeft renamedExpr2 == Left $ "Use of variable before declaration"
  | otherwise
  where
    renamedExpr1 = renameExpr expr1 vars
    renamedExpr2 = renameExpr expr2vars
renameExpr (Var var) vars
  | index >= 0 = Var ("$" ++ (show index))
  | otherwise = Var ("Use_of_variable_before_declaration:_" ++ var)
  where index = fromMaybe (-1) $ elemIndex var vars
renameExpr (Val n) _ = Val n

--TypeChecking:
---- Type environment: map variable names to their types
--type TypeEnv = [(String, Type)]
--
---- Function to get the type of a variable from the environment
--lookupVarType :: String -> TypeEnv -> Either String Type
--lookupVarType var env = case lookup var env of
--    Just t  -> Right t
--    Nothing -> Left $ "Variable " ++ var ++ " not found."
--
---- Function to infer the type of an expression
--inferExprType :: TypeEnv -> Expr -> Either String Type
--inferExprType env (Val _) = Right TypeInt
--inferExprType env (Var v) = lookupVarType v env
--inferExprType env (BinOp op e1 e2) =
--    case (op, inferExprType env e1, inferExprType env e2) of
--        (_, Left err, _) -> Left err
--        (_, _, Left err) -> Left err
--        (AddS, Right TypeInt, Right TypeInt) -> Right TypeInt
--        (SubS, Right TypeInt, Right TypeInt) -> Right TypeInt
--        (MultS, Right TypeInt, Right TypeInt) -> Right TypeInt
--        (PowS, Right TypeInt, Right TypeInt) -> Right TypeInt
--        (EQS, Right t1, Right t2) | t1 == t2 -> Right TypeBool
--        (AndS, Right TypeBool, Right TypeBool) -> Right TypeBool
--        (OrS, Right TypeBool, Right TypeBool) -> Right TypeBool
--        (NotS, Right TypeBool, Right TypeBool) -> Right TypeBool
--        (_, _, _) -> Left $ "Type error in binary operation " ++ show op
--
---- Function to type check a single instruction
--checkInstr :: TypeEnv -> Instr -> Either String TypeEnv
--checkInstr env (Decl _ t var expr) =
--    case inferExprType env expr of
--        Right t' | t == t' -> Right ((var, t) : env)
--                 | otherwise -> Left $ "Type error in declaration of " ++ var
--        Left err -> Left err
--checkInstr env (Assign var expr) =
--    case (lookupVarType var env, inferExprType env expr) of
--        (Right t, Right t') | t == t' -> Right env
--                            | otherwise -> Left $ "Type error in assignment to " ++ var
--        (Left err, _) -> Left err
--        (_, Left err) -> Left err
--checkInstr env (While expr prog) =
--    case inferExprType env expr of
--        Right TypeBool -> checkProgram env prog
--        Right _ -> Left "Type error in while condition"
--        Left err -> Left err
--checkInstr env (IfElse expr prog1 prog2) =
--    case inferExprType env expr of
--        Right TypeBool -> case checkProgram env prog1 of
--                            Right env' -> checkProgram env' prog2
--                            Left err -> Left err
--        Right _ -> Left "Type error in if condition"
--        Left err -> Left err
--checkInstr env (If expr prog) =
--    case inferExprType env expr of
--        Right TypeBool -> checkProgram env prog
--        Right _ -> Left "Type error in if condition"
--        Left err -> Left err
--checkInstr env (Print expr) =
--    case inferExprType env expr of
--        Right _ -> Right env
--        Left err -> Left err
--checkInstr env (Fork _ prog) = checkProgram env prog
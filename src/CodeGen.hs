module CodeGen where
import Parser
import Sprockell
import Data.List

main :: IO ()
--main = run $ codeGen testProg1
main = run $ codeGen program

-- Generates the code of each thread of a program; A program here is a block of IR instructions
codeGen :: Program -> [[Instruction]]
codeGen p = map (++ [EndProg]) $ main : forks                                           -- Append "EndProg" to all the generated code 
  where
    main =  progGen (shared, localVar p []) p                                           -- Generate the code main thread 
    forks = map (\p -> waitToStart ++ progGen (shared, localVar p []) p) (forkAST p []) -- Generate the code of the forked threads
    shared = sharedVar p []                                                             -- Generate the shared variable dictionary 
    waitToStart = [ ReadInstr (DirAddr regSprID)  
                  , Receive regA
                  , Compute Equal regA reg0 regA
                  , Branch regA (Rel (-3))
                  ]

-- Generates the code of a program, having as an argument the local/shared variable dictionary tuple
progGen :: (Dict,Dict) -> Program -> [Instruction]
progGen d = foldl (\xs x -> xs ++ instrGen d x) []

-- Generates code of an IR instruction, having as an argument the local/shared variable dictionary tuple
instrGen :: (Dict,Dict) -> Instr -> [Instruction]
instrGen d (Decl Local  _ n (Just e)) = exprGen regA d e ++ [Store regA (DirAddr i)]       where (_,i) = getAddr n d
instrGen d (Decl Shared _ n (Just e)) = exprGen regA d e ++ [WriteInstr regA (DirAddr i)]  where (_,i) = getAddr n d
instrGen _ (Decl Local  _ _ Nothing) = [] -- the case when the variable is just declared without assignment
instrGen _ (Decl Shared _ _ Nothing) = [] -- the same as above
instrGen d (Assign n e)
  | s == Local = exprGen regA d e ++ [Store regA (DirAddr i)]
  | otherwise  = exprGen regA d e ++ [WriteInstr regA (DirAddr i)]
  where (s,i) = getAddr n d
instrGen d (Print e) = exprGen regA d e ++ [WriteInstr regA numberIO]
instrGen d (While e p) = codeE ++
                          [ Compute Equal regA reg0 regA
                          , Branch regA (Rel (length codeP + 2))]
                          ++ codeP ++ [Jump (Rel (-length codeP - 2 - length codeE ))]
                             where
                               codeE = exprGen regA d e
                               codeP = progGen d p
instrGen d (If e p) = exprGen regA d e ++
                       [ Compute Equal regA reg0 regA
                       , Branch regA (Rel (length codeP + 1))]
                       ++ codeP
                          where
                            codeP = progGen d p
instrGen d (IfElse e p1 p2) = exprGen regA d e ++
                               [ Branch regA (Rel (length codeElse + 2))]
                               ++ codeElse ++ [Jump (Rel (length codeIf + 1))] ++ codeIf
                                  where
                                    codeIf   = progGen d p1
                                    codeElse = progGen d p2
instrGen _ (Fork (Just i) _) = [ Load (ImmValue 1) regA
                               , WriteInstr regA (DirAddr i)]
instrGen d (Lock n) = [          TestAndSet (DirAddr i)
                               , Receive regA
                               , Compute Equal regA reg0 regA
                               , Branch regA (Rel $ -3)
                               ]
                                  where (_,i) = getAddr n d
instrGen d (Unlock n) = [ WriteInstr reg0 (DirAddr i)]
                                  where (_,i) = getAddr n d
instrGen _ _ = error "invalid instruction"

exprGen :: Int -> (Dict,Dict) -> Expr -> [Instruction]
exprGen r d (BinOp o e1 e2) = exprGen r d e1
                              ++ exprGen (r+1) d e2
                              ++ [Compute (opGen o) r (r+1) r]
exprGen r d (NotOp e) = exprGen r d e 
                        ++ [Compute Equal r reg0 r]
exprGen r _    (Val x) = [Load (ImmValue x) r]
exprGen r d (Var x)
  | s == Local = [Load (DirAddr i) r]
  | otherwise  = [ReadInstr (DirAddr i), Receive r]
  where (s,i) = getAddr x d

opGen :: Op -> Operator
opGen AddS  = Add
opGen SubS  = Sub
opGen MultS = Mul
opGen EQS   = Equal
opGen LTS   = Lt
opGen LTES  = LtE
opGen AndS  = And
opGen OrS   = Or

          
type Dict  = [(String,Int)]

forkAST :: Program -> [Program] -> [Program]
forkAST [] ys = ys
forkAST (Fork (Just i) p:xs) ys = forkAST xs $ forkAST p $ ys ++ [p]
forkAST (_:xs) ys = forkAST xs ys

sharedVar :: Program -> Dict -> Dict
sharedVar [] d = d
sharedVar (Decl Shared _ n _ : xs) d
  | null d || minimum (map snd d) /= 0 = sharedVar xs $ (n,0) : d
  | otherwise                          = sharedVar xs $ (n, length d) : d
sharedVar (Fork (Just i) p : xs) d
  | null e    = sharedVar xs $ (n,i) : d
  | otherwise = sharedVar xs $ (fst $ head e, length d) : (n,i) : d'
  where
    (e,d') = partition (\(_,i') -> i==i') d
    n = "t" ++ show i
sharedVar (_:xs) d = sharedVar xs d

localVar :: Program -> Dict -> Dict
localVar [] d = d
localVar (Decl Local _ n _ : xs) d  = localVar xs $ (n, length d) : d
localVar (While _ p : xs) d         = localVar xs $ localVar p d
localVar (If _ p : xs) d            = localVar xs $ localVar p d
localVar (IfElse _ p1 p2 : xs) d    = localVar xs $ localVar p2 $ localVar p1 d
localVar (_:xs) d                   = localVar xs d

getAddr :: String -> (Dict, Dict) -> (Scope, Int)
getAddr n (shared, local)
  | l /= Nothing = (Local,  index l)
  | s /= Nothing = (Shared, index s)
  | otherwise    = error "variable not in scope"
  where
    search = find (\(n',_) -> n == n')
    index = \(Just (_,x)) -> x
    l = search local
    s = search shared


program :: Program
program =
        [ Decl Shared TypeInt "a" $ Just $ Val 10
        , Decl Shared TypeLock "lock" Nothing
        , Lock "lock"
        , Fork (Just 1) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Fork (Just 2) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Fork (Just 3) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Fork (Just 4) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Fork (Just 5) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Fork (Just 6) [ Lock "lock"
                        , Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        , Unlock "lock"
                        ]
        , Print $ Var "a"
        , Unlock "lock"
        ]


program' :: Program
program' =
        [ Decl Shared TypeInt "a" $ Just $ Val 10
        , Fork (Just 1) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Fork (Just 2) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Fork (Just 3) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Fork (Just 4) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Fork (Just 5) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Fork (Just 6) [ Print $ Var "a"
                        , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                        , Print $ Var "a"
                        ]
        , Print $ Var "a"
        , Print $ Var "a"
        , Print $ Var "a"
        , Print $ Var "a"
        , Print $ Var "a"
        ]
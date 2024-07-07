module CodeGen where
import Parser
import Sprockell
import Data.List

-- =====================================================================================================================
--                                                Code Generation
-- =====================================================================================================================

-- Generates the code of each thread of a program; A program here is a block of IR instructions
codeGen :: Program -> [[Instruction]]
codeGen p = map (++ [EndProg]) $ main : forks                                             -- Append "EndProg" to all the generated code
  where
    main =  progGen (localVar p [], shared) p                                             -- Generate the code main thread
    forks = map (\p -> waitToStart ++ progGen (localVar p [], shared) p) (forkAST p [])   -- Generate the code of the forked threads
    shared = sharedVar p []                                                               -- Generate the shared variable dictionary
    waitToStart = [ ReadInstr (IndAddr regSprID)                                          -- The necessary instructions for each forked thread; Checking when to start executing
                  , Receive regA
                  , Compute Equal regA reg0 regA
                  , Branch regA (Rel (-3))
                  ]

-- Generates the code of a program, having as an argument the local/shared variable dictionary tuple
progGen :: (Dict,Dict) -> Program -> [Instruction]
progGen d = foldl (\xs x -> xs ++ instrGen d x) []

-- Generates code of an IR instruction, having as an argument the local/shared variable dictionary tuple
instrGen :: (Dict,Dict) -> Instr -> [Instruction]
instrGen d (Decl Local  _ n (Just e)) = exprGen regA d e ++ [Store regA (DirAddr i)]      -- Generate the code of the expression and store local variables in the local memory
  where (_,i) = getAddr n d                                                               -- Get the address of the variable where it should be store; The scope is not needed as it is given already
instrGen d (Decl Shared _ n (Just e)) = exprGen regA d e ++ [WriteInstr regA (DirAddr i)] -- Generate the code of the expression and store shader variable in the shared memory
  where (_,i) = getAddr n d                                                               -- The same as above
instrGen _ (Decl Local  _ _ Nothing) = []                                                 -- The case when the variable is just declared without assignment
instrGen _ (Decl Shared _ _ Nothing) = []                                                 -- The same as above
instrGen d (Assign n e)
  | s == Local = exprGen regA d e ++ [Store regA (DirAddr i)]                             -- Check the scope and do the same as for declaration
  | otherwise  = exprGen regA d e ++ [WriteInstr regA (DirAddr i)]
  where (s,i) = getAddr n d                                                               -- Get the address and scope of the variable
instrGen d (Print e) = exprGen regA d e ++ [WriteInstr regA numberIO]                     -- Generate the code for the expression and print it out
instrGen d (While e p) = codeE ++
                         [ Compute Equal regA reg0 regA
                         , Branch regA (Rel (length codeP + 2))]                          -- If the condition is false jump over the while block
                         ++ codeP
                         ++ [Jump (Rel (-length codeP - 2 - length codeE ))]              -- Return to computing the expression inside the while condition
                         where
                           codeE = exprGen regA d e                                       -- The generated code of the expression
                           codeP = progGen d p                                            -- The generated code of the while block
instrGen d (If e p) = exprGen regA d e ++                                                 -- The same as for while, but without jumping back
                      [ Compute Equal regA reg0 regA
                      , Branch regA (Rel (length codeP + 1))]
                      ++ codeP
                      where codeP = progGen d p
instrGen d (IfElse e p1 p2) = exprGen regA d e ++                                         -- If the condition is true, jump over the else block (the else block comes before the if block)
                            [ Branch regA (Rel (length codeElse + 2))]
                            ++ codeElse
                            ++ [Jump (Rel (length codeIf + 1))]                           -- Add at the end of the else block a jump over the if block
                            ++ codeIf
                            where
                              codeIf   = progGen d p1                                     -- The generated code of the if block
                              codeElse = progGen d p2                                     -- The generated code of the else block
instrGen _ (Fork (Just i) _) = [ Load (ImmValue 1) regA                                   -- The parent thread updates the boolean value checked for starting the execution by the child thread
                               , WriteInstr regA (DirAddr i)]
instrGen d (Lock n) = [          TestAndSet (DirAddr i)                                   -- Do TestAndSet on the address of the lock until it succeeds
                               , Receive regA
                               , Compute Equal regA reg0 regA
                               , Branch regA (Rel $ -3)
                               ]
                               where (_,i) = getAddr n d                                  -- The scope is not needed, because all locks should be defined as shared
instrGen d (Unlock n) = [ WriteInstr reg0 (DirAddr i)]                                    -- Write 0 to the address memory of the lock
  where (_,i) = getAddr n d                                                               -- The same as above
instrGen _ _ = error "invalid instruction"                                                -- This pattern should never be reached. If it does, then something should be wrong in the Elaborator


-- Generates the code of an expression, having as arguments the register where it should be stored, the dictionaries and the expression
exprGen :: Int -> (Dict,Dict) -> Expr -> [Instruction]
exprGen r d (BinOp o e1 e2) = exprGen r d e1                                              -- Generate the code for the expression and store the result in r
                            ++ exprGen (r+1) d e2                                         -- The same as above
                            ++ [Compute (opGen o) r (r+1) r]                              -- Store in r the result of the operation between the registers where the result of both expressions was stored
exprGen r d (NotOp e) = exprGen r d e                                                     -- The same as above
                      ++ [Compute Equal r reg0 r]                                         -- 0 == 0 = 1; 1 == 0 = 0;
exprGen r _    (Val x) = [Load (ImmValue x) r]                                            -- For a constant, the dictionary is not needed
exprGen r d (Var x)                                                                       -- If x is local load from local memory, else load from shared memory
  | s == Local = [ Load (DirAddr i) r]
  | otherwise  = [ ReadInstr (DirAddr i)
                 , Receive r
                 ]
  where (s,i) = getAddr x d

-- Converts an IR operator to a Sprockell operator
opGen :: Op -> Operator
opGen AddS  = Add
opGen SubS  = Sub
opGen MultS = Mul
opGen EQS   = Equal
opGen LTS   = Lt
opGen LTES  = LtE
opGen AndS  = And
opGen OrS   = Or

-- =====================================================================================================================
--                                                Code Generation Preparation
-- =====================================================================================================================

-- Dictionary with the name of the variables as keys and the index in memory as the value
type Dict  = [(String,Int)]

-- Makes a list of all the blocks inside of forks (works as well for nested forks) in a preorder way
forkAST :: Program -> [Program] -> [Program]
forkAST [] ys = ys
forkAST (Fork (Just i) p:xs) ys = forkAST xs $ forkAST p $ ys ++ [p]
forkAST (Fork Nothing _:_) _ = error "fork not indexed"                                   -- This error should not happen. If it happens, it means there is an issue in the elaborator
forkAST (_:xs) ys = forkAST xs ys

-- Returns the scope and index of a variable, having as arguments the name of the variable and the local/shared variable dictionary tuple
getAddr :: String -> (Dict, Dict) -> (Scope, Int)
getAddr n (local, shared)
  | l /= Nothing = (Local,  index l)                                                      -- If it was found in local, return the local scope and the corresponding index
  | s /= Nothing = (Shared, index s)                                                      -- The same as above
  | otherwise    = error "variable not in scope"                                          -- This line should never be reached, because the Elaborator should check that all variables are in scope
  where
    index = \(Just (_,x)) -> x                                                            -- Helper function to retrieve the index of the found entry; Added for visibility
    l = find (\(n',_) -> n == n') local                                                   -- The entry from local dictionary with the given name
    s = find (\(n',_) -> n == n') shared                                                  -- The entry from shared dictionary with the given name

-- Make a dictionary of shared variables
sharedVar :: Program -> Dict -> Dict
sharedVar [] d
  | length d <= shMemSize = d
  | otherwise             = error "shared memory overflow"
sharedVar (Decl Shared _ n _ : xs) d                                                      -- Shared variables include also locks
  | null d || minimum (map snd d) /= 0 = sharedVar xs $ (n,0) : d                         -- Check if the dictionary is null and if the smallest index is not 0. The second condition is because spawned threads start counting from 1 (the global thread is 0)
  | otherwise                          = sharedVar xs $ (n, length d) : d                 -- Otherwise, the index of the variable will coincide with the size of the dictionary
sharedVar (Fork (Just i) p : xs) d                                                        -- For each spawned thread, a place in the shared memory is reserved for communicating that thread when to start executing
  | null e    = sharedVar xs $ sharedVar p $ (n,i) : d                                    -- If such an entry does not exist, simply append the new entry.
  | otherwise = sharedVar xs $ sharedVar p $ (fst $ head e, length d) : (n,i) : d'        -- Otherwise, change the index of the old entry and append the new entry.
  where
    (e,d') = partition (\(_,i') -> i==i') d                                               -- Partition the dictionary into 2 dictionaries: e - might consist of one entry with the index equal to the index of the spawned thread; d' - the rest of the dictionary
    n = "t" ++ show i                                                                     -- The variables associated with a spawned thread have a name like "t1"; This can be used for debugging
sharedVar (_:xs) d = sharedVar xs d

-- Make a dictionary of local variables of a thread
localVar :: Program -> Dict -> Dict
localVar [] d
  | length d <= localMemSize = d
  | otherwise                = error "local memory overflow"
localVar (Decl Local _ n _ : xs) d  = localVar xs $ (n, length d) : d
localVar (While _ p : xs) d         = localVar xs $ localVar p d
localVar (If _ p : xs) d            = localVar xs $ localVar p d
localVar (IfElse _ p1 p2 : xs) d    = localVar xs $ localVar p2 $ localVar p1 d
localVar (_:xs) d                   = localVar xs d


-- =====================================================================================================================
--                                                Testing Code Generation
-- =====================================================================================================================

main :: IO ()
--main = print testExprGenBinOp
main = run $ codeGen nestedThreadProgram

testLocalVar :: Dict
testLocalVar = localVar declProgram []                                                    -- Expected: [("c",2),("b",1),("i",0)]

testSharedVar :: Dict
testSharedVar = sharedVar declProgram []                                                  -- Expected: [("l",4),("t3",3),("t2",2),("t1",1),("s",0)]

testLocalMemoryOverflow :: Dict
testLocalMemoryOverflow = localVar localMemoryOverflowProgram []                          -- Expected: error local memory overflow

testSharedMemoryOverflow :: Dict
testSharedMemoryOverflow = sharedVar sharedMemoryOverflowProgram []                       -- Expected: error: shared memory overflow

testGetAddrLocal :: (Scope, Int)
testGetAddrLocal = getAddr "b" (testLocalVar, testSharedVar)                              -- Expected: (Local,1)

testGetAddrShared :: (Scope, Int)
testGetAddrShared = getAddr "s" (testLocalVar, testSharedVar)                             -- Expected: (Shared,0)

testGetAddrError :: (Scope, Int)
testGetAddrError = getAddr "f1" (testLocalVar, testSharedVar)                             -- Expected: error: variable not in scope

testForkAST :: [Program]
testForkAST = forkAST declProgram []                                                      -- Expected: [[Decl Local TypeInt "f1" Nothing,Fork (Just 2) [Decl Local TypeInt "f2" Nothing]],[Decl Local TypeInt "f2" Nothing],[Decl Local TypeInt "f3" Nothing]]

testExprGenVal :: [Instruction]
testExprGenVal = exprGen regA ([],[]) $ Val 10                                            -- Expected: [Load (ImmValue 10) 2]

testExprGenLocalVar :: [Instruction]
testExprGenLocalVar = exprGen regA (testLocalVar, testSharedVar) $ Var "i"                -- Expected: [Load (DirAddr 0) 2]

testExprGenSharedVar :: [Instruction]
testExprGenSharedVar = exprGen regA (testLocalVar, testSharedVar) $ Var "s"               -- Expected: [ReadInstr (DirAddr 0),Receive 2]

testExprGenNotOp :: [Instruction]
testExprGenNotOp = exprGen regA (testLocalVar, testSharedVar) $ NotOp (Val 1)             -- Expected: [Load (ImmValue 1) 2,Compute Equal 2 0 2]

testExprGenBinOp :: [Instruction]
testExprGenBinOp = exprGen regA (testLocalVar, testSharedVar) $                           -- Expected: [Load (DirAddr 0) 2,Load (ImmValue 10) 3,Load (ImmValue 11) 4,Compute Mul 3 4 3,Compute Add 2 3 2] 
  BinOp AddS (Var "i") (BinOp MultS (Val 10) (Val 11))

declProgram :: Program
declProgram =
  [ Decl Local TypeInt "i" Nothing
  , Print $ Var "i"
  , Decl Local TypeBool "b" $ Just $ Val 1
  , Print $ Var "b"
  , Decl Shared TypeInt "s" $ Just $ Val 2
  , Print $ Var "s"
  , Assign "s" $ Val 22
  , Print $ Var "s"
  , Decl Shared TypeLock "l" Nothing
  , If (Var "b") [Decl Local TypeInt "c" $ Just $ Val 3]
  , Fork (Just 1) [ Decl Local TypeInt "f1" Nothing
                  , Fork (Just 2) [ Decl Local TypeInt "f2" Nothing ]
                  ]
  , Fork (Just 3) [ Decl Local TypeInt "f3" Nothing]
  ]

ifProgram :: Program
ifProgram = 
  [ Decl Local TypeBool "b" $ Just $ Val 1
  , If (Var "b") [Print $ Val 10]
  , Print $ Val 10
  , If (Val 0) [Print $ Val 0]
  , Print $ Val 10
  ]

ifElseProgram :: Program
ifElseProgram = 
  [ IfElse (Val 0) [Print $ Val 0] [Print $ Val 10]
  , IfElse (Val 1) [Print $ Val 10] [Print $ Val 1]
  , Print $ Val 10
  ]

whileProgram :: Program
whileProgram =
  [ Decl Local TypeInt "i" Nothing
  , While (BinOp LTS (Var "i") (Val 10) ) 
    [ Print $ Var "i"
    , Assign "i" $ BinOp AddS (Var "i") (Val 1)
    ]
  ]

threadSafeProgram :: Program
threadSafeProgram =
  [ Decl Shared TypeInt "a" $ Just $ Val 10
  , Decl Shared TypeLock "l" Nothing
  , Print $ Var "a"
  , Fork (Just 1) [ Lock "l"
                  , Print $ Var "a"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Unlock "l"
                  ]
  , Fork (Just 2) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  , Unlock "l"
                  ]
  , Fork (Just 3) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  , Unlock "l"
                  ]
  , Fork (Just 4) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  , Unlock "l"
                  ]
  , Fork (Just 5) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  , Unlock "l"
                  ]
  , Fork (Just 6) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  , Unlock "l"
                  ]
  ]


threadProgram :: Program
threadProgram =
  [ Decl Shared TypeInt "a" $ Just $ Val 10
  , Fork (Just 1) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Fork (Just 2) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Fork (Just 3) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Fork (Just 4) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Fork (Just 5) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Fork (Just 6) [ Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
                  ]
  , Print $ Var "a"
  ]

nestedThreadProgram :: Program
nestedThreadProgram =
  [ Decl Shared TypeInt "sum" Nothing
  , Decl Local TypeInt "i" $ Just $ Val 10
  , Fork (Just 1) [ Decl Local TypeInt "i1" $ Just $ Val 1
                  , Assign "sum" $ BinOp AddS (Var "sum") (Var "i1")
                  , Fork (Just 2) [ Decl Local TypeInt "i2" $ Just $ Val 2
                                  , Assign "sum" $ BinOp AddS (Var "sum") (Var "i2")
                                  ]
                  ]
  , While (NotOp $ BinOp EQS (Var "sum") (Val 3)) []
  , Print $ Var "i"
  ]

multipleLocksProgram :: Program
multipleLocksProgram =
  [ Decl Shared TypeInt "a" Nothing
  , Decl Shared TypeInt "b" Nothing
  , Decl Shared TypeLock "l_a" Nothing
  , Decl Shared TypeLock "l_b" Nothing
  , Fork (Just 1) [ Lock "l_a"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Unlock "l_a"
                  , Lock "l_b"
                  , Assign "b" $ BinOp AddS (Var "b") (Val 2)
                  , Unlock "l_b"
                  ]
  , Fork (Just 2) [ Lock "l_b"
                  , Assign "b" $ BinOp AddS (Var "b") (Val 2)
                  , Unlock "l_b"
                  , Lock "l_a"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Unlock "l_a"
                  ]
  , While (BinOp OrS (NotOp $ BinOp EQS (Var "a") (Val 2)) (NotOp $ BinOp EQS (Var "b") (Val 4))) []
  , Print $ Var "a"
  , Print $ Var "b"
  ]

infiniteWhileProgram :: Program
infiniteWhileProgram =
  [ While (Val 1) []
  , Print $ Val 0
  ]

sharedMemoryOverflowProgram :: Program
sharedMemoryOverflowProgram =
  [ Decl Shared TypeInt "a1" $ Just $ Val 1
  , Decl Shared TypeInt "a2" $ Just $ Val 1
  , Decl Shared TypeInt "a3" $ Just $ Val 1
  , Decl Shared TypeInt "a4" $ Just $ Val 1
  , Decl Shared TypeInt "a5" $ Just $ Val 1
  , Decl Shared TypeLock "l" Nothing
  , Fork (Just 1) []
  , Fork (Just 2) []
  , Fork (Just 3) []
  ]

localMemoryOverflowProgram :: Program
localMemoryOverflowProgram = [(Decl Local TypeInt (show x) Nothing) | x <- [0..32]]
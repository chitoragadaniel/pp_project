import Parser
import Elaborator
import CodeGen
import Sprockell
import Test.Hspec
import Test.QuickCheck
--import Test.Hspec.Core.Clock
import System.IO.Silently
import Control.Exception

main :: IO ()
main = hspec $ do
  describe "Language (running code)" $ do
    it "shows that a thread can't access local variables of other threads" $ do
      runFile "./test/demos/p0" `shouldThrow` anyException
    it "defines a local lock" $ do
      runFile "./test/demos/p1" `shouldThrow` anyException
    it "overshadows a variable" $ do
      stdout <- capture_ $ runFile "./test/demos/p2"
      stdout `shouldBe` "Sprockell 0 says 2/nSprockell 0 says 1/n"
  describe "Code generation" $ do
    describe "dictionaries" $ do
      it "gets the local dictionary of a thread" $ do
        testLocalVar `shouldBe` [("c",2),("b",1),("i",0)]
      it "gets the shared dictionary" $ do
        testSharedVar `shouldBe` [("l",4),("t3",3),("t2",2),("t1",1),("s",0)]
      it "overflows the local memory while getting the local dictionary" $ do
        evaluate testLocalMemoryOverflow `shouldThrow` anyException
      it "overflows the shared memory while getting the local dictionary" $ do
        evaluate testSharedMemoryOverflow `shouldThrow` anyException
      it "gets the address of a local variable" $ do
        testGetAddrLocal `shouldBe` (Local,1)
      it "gets the address of a shared variable" $ do
        testGetAddrShared `shouldBe` (Shared,0)
      it "tries to get the address of a variable that's not in any dictionary" $ do
        evaluate testGetAddrError `shouldThrow` anyException
    describe "generating expressions" $ do
      it "generates the code for a value" $ do
        testExprGenVal `shouldBe` [Load (ImmValue 10) 2]
      it "generates the code for using a local variable" $ do
        testExprGenLocalVar `shouldBe` [Load (DirAddr 0) 2]
      it "generates the code for using a shared variable" $ do
        testExprGenSharedVar `shouldBe` [ReadInstr (DirAddr 0),Receive 2]
      it "generates the code for a not operation: not(1)" $ do
        testExprGenNotOp `shouldBe` [Load (ImmValue 1) 2,Compute Equal 2 0 2]
      it "generates the code for a binary operation: i+10*11" $ do
        testExprGenBinOp `shouldBe` [Load (DirAddr 0) 2,Load (ImmValue 10) 3,Load (ImmValue 11) 4,Compute Mul 3 4 3,Compute Add 2 3 2]
    describe "generating instructions" $ do -- add another print with an complicated expression
      it "prints the value 100" $ do
        stdout <- capture_ (run (codeGen [Print $ Val 100]))
        stdout `shouldBe` "Sprockell 0 says 100\n"
      it "prints the 1+2" $ do
        stdout <- capture_ (run (codeGen [Print $ BinOp AddS (Val 1) (Val 2)]))
        stdout `shouldBe` "Sprockell 0 says 3\n"
      it "declares all types of legal variables" $ do
        stdout <- capture_ (run (codeGen declProgram))
        stdout `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 22\n"
      it "tests running two if's; one with a true condition and one with a false one" $ do
        stdout <- capture_ (run (codeGen ifProgram))
        stdout `shouldBe` "Sprockell 0 says 10\n"
      it "tests running two if with else; one with a true condition and one with a false one" $ do
        stdout <- capture_ (run (codeGen ifElseProgram))
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 0 says 10\n"
      it "prints the values from 0 to 9 using while" $ do
        stdout <- capture_ (run (codeGen whileProgram))
        stdout `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 3\nSprockell 0 says 4\nSprockell 0 says 5\nSprockell 0 says 6\nSprockell 0 says 7\nSprockell 0 says 8\nSprockell 0 says 9\n"
      it "spawns 6 threads that try (not safely) to increment a shared variable of value 10" $ do
        stdout <- capture_ (run (codeGen threadProgram))
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 11\nSprockell 3 says 11\nSprockell 4 says 11\nSprockell 5 says 11\nSprockell 6 says 11\n"
      it "spawns 6 threads increment safely a shared variable of value 10" $ do
        stdout <- capture_ (run (codeGen threadSafeProgram))
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 12\nSprockell 3 says 13\nSprockell 4 says 14\nSprockell 5 says 15\nSprockell 6 says 16\n"
      it "spawns 2 threads (one being nested) and both increment a shared variable" $ do
        stdout <- capture_ (run (codeGen nestedThreadProgram))
        stdout `shouldBe` ""
      it "spawns 2 threads that increment a by 1 and b by 2 using 2 locks for synchronization" $ do
        stdout <- capture_ (run (codeGen multipleLocksProgram))
        stdout `shouldBe` "Sprockell 0 says 2\nSprockell 0 says 4\n"
    describe "others" $ do
      it "gets the IR's of all the spawned threads" $ do
        testForkAST `shouldBe` [[Decl Local TypeInt "f1" Nothing,Fork (Just 2) [Decl Local TypeInt "f2" Nothing]],[Decl Local TypeInt "f2" Nothing],[Decl Local TypeInt "f3" Nothing]]
      it "runs an infinite while loop" $ do
        stdout <- timeout 1 $ run $ codeGen infiniteWhileProgram
        stdout `shouldBe` Nothing

-- #####################################################################################################################
-- #                                                 Code Generation                                                   #
-- #####################################################################################################################

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

localMemoryOverflowProgram :: Program
localMemoryOverflowProgram = [Decl Local TypeInt (show x) Nothing | x <- [0..32]]

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
  , If (Val 0) [Print $ Val 0]
  ]

ifElseProgram :: Program
ifElseProgram =
  [ Decl Local TypeBool "b" $ Just $ Val 1
  , IfElse (Var "b")  [Print $ Val 10] [Print $ Val 0]
  , IfElse (Val 0)    [Print $ Val 0]  [Print $ Val 10]
  ]

whileProgram :: Program
whileProgram =
  [ Decl Local TypeInt "i" Nothing
  , While (BinOp LTS (Var "i") (Val 10) )
    [ Print $ Var "i"
    , Assign "i" $ BinOp AddS (Var "i") (Val 1)
    ]
  ]

threadProgram :: Program
threadProgram =
  [ Decl Shared TypeInt "a" $ Just $ Val 10
  , Print $ Var "a"
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
  ]

threadSafeProgram :: Program
threadSafeProgram =
  [ Decl Shared TypeInt "a" $ Just $ Val 10
  , Decl Shared TypeLock "l" Nothing
  , Print $ Var "a"
  , Fork (Just 1) [ Lock "l"
                  , Assign "a" $ BinOp AddS (Var "a") (Val 1)
                  , Print $ Var "a"
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

nestedThreadProgram :: Program
nestedThreadProgram =
  [ Decl Shared TypeInt "sum" Nothing
  , Fork (Just 1) [ Assign "sum" $ BinOp AddS (Var "sum") (Val 1)
                  , Fork (Just 2) [ Assign "sum" $ BinOp AddS (Var "sum") (Val 1)]
                  ]
  , While (NotOp $ BinOp EQS (Var "sum") (Val 2)) []
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

-- #####################################################################################################################
-- #                                                     Parsing                                                       #
-- #####################################################################################################################

-- Test cases for parseType
testParseTypeBool :: Either ParseError Type
testParseTypeBool = parse parseType "" "bool"
-- expected: Right TypeBool

testParseTypeInt :: Either ParseError Type
testParseTypeInt = parse parseType "" "int"
-- expected: Right TypeInt

testParseTypeLock :: Either ParseError Type
testParseTypeLock = parse parseType "" "lock"
-- expected: Right TypeLock

-- Test cases for parseScope
testParseScopeLocal :: Either ParseError Scope
testParseScopeLocal = parse parseScope "" ""
-- expected: Right Local

testParseScopeShared :: Either ParseError Scope
testParseScopeShared = parse parseScope "" "shared"
-- expected: Right Shared

-- Test cases for parseExpr
testParseExprInt :: Either ParseError Expr
testParseExprInt = parse parseExpr "" "5"
-- expected: Right (Val 5)

testParseExprBoolTrue :: Either ParseError Expr
testParseExprBoolTrue = parse parseExpr "" "true"
-- expected: Right (BVal True)

testParseExprBoolFalse :: Either ParseError Expr
testParseExprBoolFalse = parse parseExpr "" "false"
-- expected: Right (BVal False)

testParseExprVar :: Either ParseError Expr
testParseExprVar = parse parseExpr "" "x"
-- expected: Right (Var "x")

testParseExprAddition :: Either ParseError Expr
testParseExprAddition = parse parseExpr "" "x + 5"
-- expected: Right (BinOp AddS (Var "x") (Val 5))

testParseExprLogicalAnd :: Either ParseError Expr
testParseExprLogicalAnd = parse parseExpr "" "x and y"
-- expected: Right (BinOp AndS (Var "x") (Var "y"))

testParseExprComparison :: Either ParseError Expr
testParseExprComparison = parse parseExpr "" "x == y"
-- expected: Right (BinOp EQS (Var "x") (Var "y"))

-- Test cases for parseInstr
testParseInstrDecl :: Either ParseError Instr
testParseInstrDecl = parse parseInstr "" "int x"
-- expected: Right (Decl Local TypeInt "x" Nothing)

testParseInstrDeclInit :: Either ParseError Instr
testParseInstrDeclInit = parse parseInstr "" "int x = 5"
-- expected: Right (Decl Local TypeInt "x" (Just (Val 5)))

testParseInstrAssign :: Either ParseError Instr
testParseInstrAssign = parse parseInstr "" "x = 5"
-- expected: Right (Assign "x" (Val 5))

testParseInstrWhile :: Either ParseError Instr
testParseInstrWhile = parse parseInstr "" "while (true) { int x = 5 }"
-- expected: Right (While (BVal True) [Decl Local TypeInt "x" (Just (Val 5))])

testParseInstrIfElse :: Either ParseError Instr
testParseInstrIfElse = parse parseInstr "" "if (true) { int x = 5; } else { int y = 6 }"
-- expected: Right (IfElse (BVal True) [Decl Local TypeInt "x" (Just (Val 5))] [Decl Local TypeInt "y" (Just (Val 6))])

testParseInstrPrint :: Either ParseError Instr
testParseInstrPrint = parse parseInstr "" "print(x)"
-- expected: Right (Print (Var "x"))

testParseInstrLock :: Either ParseError Instr
testParseInstrLock = parse parseInstr "" "lock(l)"
-- expected: Right (Lock "l")

testParseInstrUnlock :: Either ParseError Instr
testParseInstrUnlock = parse parseInstr "" "unlock(l)"
-- expected: Right (Unlock "l")

testParseInstrFork :: Either ParseError Instr
testParseInstrFork = parse parseInstr "" "fork { int x = 5 }"
-- expected: Right (Fork Nothing [Decl Local TypeInt "x" (Just (Val 5))])

-- Test cases for whole programs
testParseProgram :: Either ParseError Program
testParseProgram = parse parseProgram "" "int x = 5; while (true) { int y = 6; }"
-- expected: Right [Decl Local TypeInt "x" (Just (Val 5)), While (BVal True) [Decl Local TypeInt "y" (Just (Val 6))]]

-- Test cases for comments
testParseWithComment :: Either ParseError Program
testParseWithComment = parse parseProgram  "" "int x = 5; // this is a comment\nint y = 6;"
-- expected: Right [Decl Local TypeInt "x" (Just (Val 5)), Decl Local TypeInt "y" (Just (Val 6))]

-- Test cases for error scenarios
testParseErrorIncompleteInstr :: Either ParseError Program
testParseErrorIncompleteInstr = parse parseProgram "" "while (true) { int x = }"
-- expected: Left

testParseErrorIncompleteExpr :: Either ParseError Program
testParseErrorIncompleteExpr = parse parseProgram "" "while (true) { int x = 5 + }"
-- expected: Left

testParseErrorInvalidInput :: Either ParseError Program
testParseErrorInvalidInput = parse parseProgram "" "while (true) { int x = 5 + 7} invalid input"
-- expected: Left

-- #####################################################################################################################
-- #                                                  Type Checking                                                    #
-- #####################################################################################################################

-- Test cases for lookupVarType
testLookupVarTypeFound :: Either String Type
testLookupVarTypeFound = lookupVarType "x" [("x", (Local, TypeInt))]
-- expected: Right TypeInt

testLookupVarTypeNotFound :: Either String Type
testLookupVarTypeNotFound = lookupVarType "y" [("x", (Local, TypeInt))]
-- expected: Left "Variable y not found"

-- Test cases for inferExprType
testInferExprTypeVal :: Either String Type
testInferExprTypeVal = inferExprType [] (Val 5)
-- expected: Right TypeInt

testInferExprTypeBVal :: Either String Type
testInferExprTypeBVal = inferExprType [] (BVal True)
-- Expected: Right TypeBool

testInferExprTypeVar :: Either String Type
testInferExprTypeVar = inferExprType [("x", (Local, TypeInt))] (Var "x")
 -- Expected: Right TypeInt

testInferExprTypeNotOp :: Either String Type
testInferExprTypeNotOp = inferExprType [("x", (Local, TypeBool))] (NotOp (Var "x"))
-- Expected: Right TypeBool

testInferExprTypeNotOpError :: Either String Type
testInferExprTypeNotOpError = inferExprType [("x", (Local, TypeInt))] (NotOp (Var "x"))
-- Expected: Left "Type error in NotOp"

testInferExprTypeBinOpAdd :: Either String Type
testInferExprTypeBinOpAdd = inferExprType [("x", (Local, TypeInt)), ("y", (Local, TypeInt))] (BinOp AddS (Var "x") (Var "y"))
-- Expected: Right TypeInt

testInferExprTypeBinOpError :: Either String Type
testInferExprTypeBinOpError = inferExprType [("x", (Local, TypeInt)), ("y", (Local, TypeBool))] (BinOp AddS (Var "x") (Var "y"))
-- Expected: "Type error in BinOp"

-- Test cases for checkInstr
testCheckInstrDecl :: Either String TypeEnv
testCheckInstrDecl = checkInstr [] (Decl Local TypeInt "x" Nothing) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrDeclInit :: Either String TypeEnv
testCheckInstrDeclInit = checkInstr [] (Decl Local TypeInt "x" (Just (Val 5))) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrDeclTypeError :: Either String TypeEnv
testCheckInstrDeclTypeError =  checkInstr [] (Decl Local TypeInt "x" (Just (BVal True))) GlobalScope
-- Expected: Left "Type error in Decl of x"

testCheckInstrDeclForkScopeError :: Either String TypeEnv
testCheckInstrDeclForkScopeError =  checkInstr [] (Decl Shared TypeInt "x" (Just (BVal True))) ForkScope
-- Expected: Left "Cannot declare shared variable in local scope"

testCheckInstrDeclControlScopeError :: Either String TypeEnv
testCheckInstrDeclControlScopeError =  checkInstr [] (Decl Shared TypeInt "x" (Just (BVal True))) ForkScope
-- Expected: Left "Cannot declare shared variable in local scope"

testCheckInstrDeclLockError :: Either String TypeEnv
testCheckInstrDeclLockError =  checkInstr [] (Decl Local TypeLock "x" Nothing) GlobalScope
-- Expected: Left "Cannot declare lock with local scope"

testCheckInstrDeclDuplicateDeclarationError :: Either String TypeEnv
testCheckInstrDeclDuplicateDeclarationError = checkInstr [("x", (Local, TypeInt))] (Decl Local TypeLock "x" Nothing) GlobalScope
-- Expected: Left "Duplicate declaration of variable: x"

testCheckInstrAssign :: Either String TypeEnv
testCheckInstrAssign = checkInstr [("x", (Local, TypeInt))] (Assign "x" (Val 5)) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrAssignError :: Either String TypeEnv
testCheckInstrAssignError = checkInstr [("x", (Local, TypeInt))] (Assign "x" (BVal True)) GlobalScope
-- Expected: Left "Type error in Assign"

testCheckInstrWhile :: Either String TypeEnv
testCheckInstrWhile = checkInstr [("x", (Local, TypeInt))] (While (BVal True) [Assign "x" (Val 5)]) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrWhileError :: Either String TypeEnv
testCheckInstrWhileError =  checkInstr [("x", (Local, TypeInt))] (While (Val 5) [Assign "x" (Val 5)]) GlobalScope
-- Expected Right Left "Type error in While condition"

testCheckInstrIf :: Either String TypeEnv
testCheckInstrIf = checkInstr [("x", (Local, TypeInt))] (If (BVal True) [Assign "x" (Val 5)]) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrIfError :: Either String TypeEnv
testCheckInstrIfError =  checkInstr [("x", (Local, TypeInt))] (If (Val 5) [Assign "x" (Val 5)]) GlobalScope
-- Expected: Left "Type error in if condition"

testCheckInstrIfElse :: Either String TypeEnv
testCheckInstrIfElse = checkInstr [("x", (Local, TypeInt)), ("y", (Local, TypeInt))] (IfElse (BVal True) [Assign "x" (Val 5)] [Assign "y" (Val 6)]) GlobalScope
-- Expected: Right [("x", (Local, TypeInt)), ("y", (Local, TypeInt))]

testCheckInstrIfElseError :: Either String TypeEnv
testCheckInstrIfElseError =  checkInstr [("x", (Local, TypeInt))] (IfElse (Val 5) [Assign "x" (Val 5)] []) GlobalScope
-- Expected: Left "Type error in if else condition"

testCheckInstrPrint :: Either String TypeEnv
testCheckInstrPrint = checkInstr [("x", (Local, TypeInt))] (Print (Var "x")) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrPrintError :: Either String TypeEnv
testCheckInstrPrintError = checkInstr [] (Print (Var "x")) GlobalScope
-- Expected: Left "Variable x not found in scope."

testCheckInstrLock :: Either String TypeEnv
testCheckInstrLock = checkInstr [("l", (Local, TypeLock))] (Lock "l") GlobalScope
-- Expected: Right [("l", (Local, TypeLock))]

testCheckInstrLockError :: Either String TypeEnv
testCheckInstrLockError = checkInstr [("l", (Local, TypeInt))] (Lock "l") GlobalScope
-- Expected: Left "Type error in lock instruction to l"

testCheckInstrUnlock :: Either String TypeEnv
testCheckInstrUnlock = checkInstr [("l", (Local, TypeLock))] (Unlock "l") GlobalScope
-- Expected: Right [("l", (Local, TypeLock))]

testCheckInstrUnlockError :: Either String TypeEnv
testCheckInstrUnlockError =  checkInstr [("l", (Local, TypeInt))] (Unlock "l") GlobalScope
-- Expected: Left "Type error in unlock instruction to l"

testCheckInstrFork :: Either String TypeEnv
testCheckInstrFork = checkInstr [] (Fork Nothing []) GlobalScope
-- Expected: Right []

testCheckInstrForkError :: Either String TypeEnv
testCheckInstrForkError =  checkInstr [] (Fork Nothing[]) ControlScope
-- Expected: Left "Cannot enter fork from outside global scope"

-- Test cases for checkProgram
testCheckProgram :: Either String TypeEnv
testCheckProgram = checkProg [] [Decl Local TypeInt "x" (Just (Val 5)), While (BVal True) [Assign "x" (Val 6)]] GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckProgramError_1 :: Either String TypeEnv
testCheckProgramError_1 = checkProg [] [Decl Local TypeInt "f1" (Just (Val 0)),Fork Nothing [Decl Local TypeInt "f2" (Just (Val 1)),Print (Var "f1")]] GlobalScope
-- Expected: Left "Variable f1 not found."

testCheckProgramError_2 :: Either String TypeEnv
testCheckProgramError_2 = checkProg [] [Decl Local TypeLock "l" Nothing] GlobalScope
-- Expected: Left "Cannot declare lock with local scope"

testCheckProgramError_3 :: Either String TypeEnv
testCheckProgramError_3 = checkProg [] [Decl Local TypeInt "a" (Just (Val 1)),If (BVal True) [Decl Local TypeInt "a" (Just (Val 2)),Print (Var "a")],Print (Var "a")] GlobalScope
-- Expected: Left "Duplicate declaration of variable: a"

testCheckProgramError_4 :: Either String TypeEnv
testCheckProgramError_4 = checkProg [] [Fork Nothing [Decl Shared TypeInt "a" Nothing]] GlobalScope
-- Expected: Left "Cannot declare shared variable in local scope"

testCheckProgramError_5 :: Either String TypeEnv
testCheckProgramError_5 = checkProg [] [If (BVal True) [Decl Shared TypeInt "a" Nothing]] GlobalScope
-- Expected: Left "Cannot declare shared variable in local scope"

testCheckProgramError_6 :: Either String TypeEnv
testCheckProgramError_6 = checkProg [] [If (BVal True) [Fork Nothing []]] GlobalScope
-- Expected: Left "Cannot enter fork from outside global scope"

testCheckProgramError_7 :: Either String TypeEnv
testCheckProgramError_7 = checkProg [] [If (BVal True) [Decl Local TypeInt "a" (Just (Val 1))],Print (Var "a")] GlobalScope
-- Expected: Left "Variable a not found in scope."

testCheckProgramError_8 :: Either String TypeEnv
testCheckProgramError_8 = checkProg [] [Print (Var "a"),Decl Local TypeInt "a" (Just (Val 10))] GlobalScope
-- Expected: Left "Variable a not found in scope."

testCheckProgramError_9 :: Either String TypeEnv
testCheckProgramError_9 = checkProg [] [Fork Nothing [Print (Var "a")],Decl Shared TypeInt "a" (Just (Val 10))] GlobalScope
-- Expected: Left "Variable a not found in scope."

-- #####################################################################################################################
-- #                                               Program Optimizations                                               #
-- #####################################################################################################################

-- Test cases for optimizeExpr
testOptimizeExprVal :: Expr
testOptimizeExprVal = either error id (optimizeExpr [] (Val 5))
-- expected: Val 5

testOptimizeExprBValTrue :: Expr
testOptimizeExprBValTrue = either error id (optimizeExpr [] (BVal True))
-- expected: Val 1

testOptimizeExprBValFalse :: Expr
testOptimizeExprBValFalse = either error id (optimizeExpr [] (BVal False))
-- expected: Val 0

testOptimizeExprVar :: Expr
testOptimizeExprVar = either error id (optimizeExpr [("x", "x1")] (Var "x"))
-- expected: Var "x1"

testOptimizeExprNotOp :: Expr
testOptimizeExprNotOp = either error id (optimizeExpr [("x", "x1")] (NotOp (Var "x")))
-- expected: NotOp (Var "x1")

testOptimizeExprBinOp :: Expr
testOptimizeExprBinOp = either error id (optimizeExpr [("x", "x1"), ("y", "y1")] (BinOp AddS (Var "x") (Var "y")))
-- expected: BinOp AddS (Var "x1") (Var "y1")

-- Test cases for optimizeInstr
testOptimizeInstrDecl :: Instr
testOptimizeInstrDecl = either error id (optimizeInstr (Decl Local TypeInt "x" Nothing) [] 0)
-- expected: (Decl Local TypeInt "$0" Nothing, [("x", "$0")], 0)

testOptimizeInstrDeclInit :: Instr
testOptimizeInstrDeclInit = either error id (optimizeInstr (Decl Local TypeInt "x" (Just (Val 5))) [] 0)
-- expected: (Decl Local TypeInt "$0" (Just (Val 5)), [("x", "$0")], 0)

testOptimizeInstrAssign :: Instr
testOptimizeInstrAssign = either error id (optimizeInstr (Assign "x" (Val 5)) [("x", "x1")] 0)
-- expected: (Assign "x1" (Val 5), [("x", "x1")], 0)

testOptimizeInstrWhile :: Instr
testOptimizeInstrWhile = either error id (optimizeInstr (While (BVal True) [Print (Var "x")]) [("x", "x1")] 0)
-- expected: (While (Val 1) [Print (Var "x1")], [("x", "x1")], 0)

testOptimizeInstrIfElse :: Instr
testOptimizeInstrIfElse = either error id (optimizeInstr (IfElse (BVal True) [Print (Var "x")] [Print (Var "y")]) [("x", "x1"), ("y", "y1")] 0)
-- expected: (IfElse (Val 1) [Print (Var "x1")] [Print (Var "y1")], [("x", "x1"), ("y", "y1")], 0)

testOptimizeInstrIf :: Instr
testOptimizeInstrIf = either error id (optimizeInstr (If (BVal True) [Print (Var "x")]) [("x", "x1")] 0)
-- expected: (If (Val 1) [Print (Var "x1")], [("x", "x1")], 0)

testOptimizeInstrPrint :: Instr
testOptimizeInstrPrint = either error id (optimizeInstr (Print (Var "x")) [("x", "x1")] 0)
-- expected: (Print (Var "x1"), [("x", "x1")], 0)

testOptimizeInstrFork :: Instr
testOptimizeInstrFork = either error id (optimizeInstr (Fork Nothing [Print (Var "x")]) [("x", "x1")] 0)
-- expected: (Fork (Just 0) [Print (Var "x1")], [("x", "x1")], 1)

testOptimizeInstrLock :: Instr
testOptimizeInstrLock = either error id (optimizeInstr (Lock "x") [("x", "x1")] 0)
-- expected: (Lock "x1", [("x", "x1")], 0)

testOptimizeInstrUnlock :: Instr
testOptimizeInstrUnlock = either error id (optimizeInstr (Unlock "x") [("x", "x1")] 0)
-- expected: (Unlock "x1", [("x", "x1")], 0)

-- Test cases for optimizeProg
testOptimizeProgEmpty :: Program
testOptimizeProgEmpty = either error fst (optimizeProg [] [] 0)
-- expected: []

testOptimizeProgSingleDecl :: Program
testOptimizeProgSingleDecl = either error fst (optimizeProg [Decl Local TypeInt "x" Nothing] [] 0)
-- expected: [Decl Local TypeInt "$0" Nothing]

testOptimizeProgMultipleInstrs :: Program
testOptimizeProgMultipleInstrs = either error fst (optimizeProg [Decl Local TypeInt "x" (Just (Val 5)), Assign "x" (Val 10)] [] 0)
-- expected: [Decl Local TypeInt "$0" (Just (Val 5)), Assign "$0" (Val 10)]
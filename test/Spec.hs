import Parser
import Elaborator
import CodeGen
import Sprockell
import Test.Hspec
import Test.QuickCheck
import System.Timeout
import System.IO.Silently
import Control.Exception
import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    describe "Parses Types" $ do
      it "Parses type bool" $ do
        testParseTypeBool `shouldBe` Right TypeBool
      it "Parses type Int" $ do
        testParseTypeInt `shouldBe` Right TypeInt
      it "Parses type Lock" $ do
        testParseTypeLock `shouldBe` Right TypeLock
    describe "Parses Scopes" $ do
      it "Parses local scope" $ do
        testParseScopeLocal `shouldBe` Right Local
      it "Parses shared scope" $ do
        testParseScopeShared `shouldBe` Right Shared
    describe "Parses Expressions" $ do
      it "Parses integer value" $ do
        testParseExprInt `shouldBe` Right (Val 5)
      it "Parses boolean value true" $ do
        testParseExprBoolTrue `shouldBe` Right (BVal True)
      it "Parses boolean value false" $ do
        testParseExprBoolFalse `shouldBe` Right (BVal False)
      it "Parses variables" $ do
        testParseExprVar `shouldBe` Right (Var "x")
      it "Parses addition/subtraction expressions" $ do
        testParseExprAddition `shouldBe` Right (BinOp AddS (Var "x") (Val 5))
      it "Parses logical expressions" $ do
        testParseExprLogicalAnd `shouldBe` Right (BinOp AndS (Var "x") (Var "y"))
      it "Parses comparison expressions" $ do
        testParseExprComparison `shouldBe` Right (BinOp EQS (Var "x") (Var "y"))
    describe "Parses Instuctions" $ do
      it "Parses declaration instructions" $ do
        testParseInstrDecl `shouldBe` Right (Decl Local TypeInt "x" Nothing)
      it "Parses declaration instructions with initialization" $ do
        testParseInstrDeclInit `shouldBe` Right (Decl Local TypeInt "x" (Just (Val 5)))
      it "Parses assign instructions" $ do
        testParseInstrAssign `shouldBe` Right (Assign "x" (Val 5))
      it "Parses while instructions" $ do
        testParseInstrWhile `shouldBe` Right (While (BVal True) [Decl Local TypeInt "x" (Just (Val 5))])
      it "Parses if else instructions" $ do
        testParseInstrIfElse `shouldBe` Right (IfElse (BVal True) [Decl Local TypeInt "x" (Just (Val 5))] [Decl Local TypeInt "y" (Just (Val 6))])
      it "Parses if instructions" $ do
        testParseInstrIf `shouldBe` Right (If (BVal True) [Decl Local TypeInt "x" (Just (Val 5))])
      it "Parses print instructions" $ do
        testParseInstrPrint `shouldBe` Right (Print (Var "x"))
      it "Parses lock instructions" $ do
        testParseInstrLock `shouldBe` Right (Lock "l")
      it "Parses unlock instructions" $ do
        testParseInstrUnlock `shouldBe` Right (Unlock "l")
      it "Parses fork instructions" $ do
        testParseInstrFork `shouldBe` Right (Fork Nothing [Decl Local TypeInt "x" (Just (Val 5))])
    describe "Parses Programs" $ do
      it "Parses a program" $ do
        testParseProgram `shouldBe` Right [Decl Local TypeInt "x" (Just (Val 5)), While (BVal True) [Decl Local TypeInt "y" (Just (Val 6))]]
      it "Parses a program with comments" $ do
        testParseWithComment `shouldBe` Right [Decl Local TypeInt "x" (Just (Val 5)), Decl Local TypeInt "y" (Just (Val 6))]
      it "Does not parse a program with incomplete instructions" $ do
        evaluate testParseErrorIncompleteInstr `shouldThrow` anyException
      it "Does not parse a program with incomplete expressions" $ do
        evaluate testParseErrorIncompleteExpr `shouldThrow` anyException
      it "Does not parse a program with invalid input" $ do
        evaluate testParseErrorInvalidInput `shouldThrow` anyException
  describe "Type Checking" $ do
    describe "Get types for variables" $ do
      it "Gets types for declared variable" $ do
        testLookupVarTypeFound `shouldBe` Right TypeInt
      it "Does not get types for undeclared variables" $ do
        testLookupVarTypeNotFound `shouldBe` Left "Variable y not found in scope"
    describe "Infers types of expressions" $ do
      it "Infers type of integers" $ do
        testInferExprTypeVal `shouldBe` Right TypeInt
      it "Infers type of booleans" $ do
        testInferExprTypeBVal `shouldBe` Right TypeBool
      it "Infers type of variables" $ do
        testInferExprTypeVar `shouldBe` Right TypeInt
      it "Infers type of unary expressions" $ do
        testInferExprTypeNotOp `shouldBe` Right TypeBool
      it "Does not Infer type of incorrect unary expressions" $ do
        testInferExprTypeNotOpError `shouldBe` Left "Type error cannot use unary on expr Var \"x\""
      it "Infers type of Binary operation expressions" $ do
        testInferExprTypeBinOpAdd `shouldBe` Right TypeInt
      it "Does not infer types of mismatched binary operations" $ do
        testInferExprTypeBinOpError `shouldBe` Left "Type error in binary operation AddS"
    describe "Type checks instructions" $ do
      it "Type checks declaration instructions" $ do
        testCheckInstrDecl `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks declaration instructions with value initialization" $ do
        testCheckInstrDeclInit `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks invalid declaration instructions" $ do
        testCheckInstrDeclTypeError `shouldBe` Left "Type error in declaration of x"
      it "Type checks declaration instructions" $ do
        testCheckInstrDeclForkScopeError `shouldBe` Left "Cannot declare shared variable in local scope"
      it "Type checks declaration instructions" $ do
        testCheckInstrDeclControlScopeError `shouldBe` Left "Cannot declare shared variable in local scope"
      it "Type checks declaration instructions" $ do
        testCheckInstrDeclLockError `shouldBe` Left "Cannot declare lock with local scope"
      it "Type checks declaration instructions" $ do
        testCheckInstrDeclDuplicateDeclarationError `shouldBe` Left "Duplicate declaration of variable: x"
      it "Type checks assign instructions" $ do
        testCheckInstrAssign `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks assign instructions" $ do
        testCheckInstrAssignError `shouldBe` Left "Type error in assignment to x"
      it "Type checks while instructions" $ do
        testCheckInstrWhile `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks while instructions" $ do
        testCheckInstrWhileError `shouldBe` Left "Type error in While condition"
      it "Type checks if instructions" $ do
        testCheckInstrIf `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks if instructions" $ do
        testCheckInstrIfError `shouldBe` Left "Type error in if condition"
      it "Type checks if else instructions" $ do
        testCheckInstrIfElse `shouldBe` Right [("x", (Local, TypeInt)), ("y", (Local, TypeInt))]
      it "Type checks if else instructions" $ do
        testCheckInstrIfElseError `shouldBe` Left "Type error in if else condition"
      it "Type checks print instructions" $ do
        testCheckInstrPrint `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks print instructions" $ do
        testCheckInstrPrintError `shouldBe` Left "Variable x not found in scope."
      it "Type checks lock instructions" $ do
        testCheckInstrLock `shouldBe` Right [("l", (Local, TypeLock))]
      it "Type checks lock instructions" $ do
        testCheckInstrLockError `shouldBe` Left "Type error in lock instruction to l"
      it "Type checks unlock instructions" $ do
        testCheckInstrUnlock `shouldBe` Right [("l", (Local, TypeLock))]
      it "Type checks unlock instructions" $ do
        testCheckInstrUnlockError `shouldBe` Left "Type error in unlock instruction to l"
      it "Type checks fork instructions" $ do
        testCheckInstrFork `shouldBe` Right []
      it "Type checks fork instructions" $ do
        testCheckInstrForkError `shouldBe` Left "Cannot enter fork from outside global scope"
    describe "Type checks programs" $ do
      it "Type checks valid programs" $ do
        testCheckProgram `shouldBe` Right [("x", (Local, TypeInt))]
      it "Type checks illegal programs" $ do
        testCheckProgramError_1 `shouldBe` Left "Variable f1 not found in scope"
      it "Type checks illegal programs" $ do
        testCheckProgramError_2 `shouldBe` Left "Cannot declare lock with local scope"
      it "Type checks illegal programs" $ do
        testCheckProgramError_3 `shouldBe` Left "Duplicate declaration of variable: a"
      it "Type checks illegal programs" $ do
        testCheckProgramError_4 `shouldBe` Left "Cannot declare shared variable in local scope"
      it "Type checks illegal programs" $ do
        testCheckProgramError_5 `shouldBe` Left "Cannot declare shared variable in local scope"
      it "Type checks illegal programs" $ do
        testCheckProgramError_6 `shouldBe` Left "Cannot enter fork from outside global scope"
      it "Type checks illegal programs" $ do
        testCheckProgramError_7 `shouldBe` Left "Variable a not found in scope."
      it "Type checks illegal programs" $ do
        testCheckProgramError_8 `shouldBe` Left "Variable a not found in scope."
      it "Type checks illegal programs" $ do
        testCheckProgramError_9 `shouldBe` Left "Variable a not found in scope."
  describe "Program Optimizations" $ do
    describe "Gets updated variabled names" $ do
      it "Gets the update variable name when variable is declared" $ do
        testLookupVarNameFound `shouldBe` Right TypeInt
      it "Does not get updated variable name when variable is undeclared" $ do
        testLookupVarNameNotFound `shouldBe` Left "Variable y not found"
    describe "Elaborates expressions" $ do
      it "Elaborates value expressions" $ do
        testElaborateExprVal `shouldBe` Right (Val 5)
      it "Elaborates boolean value expressions" $ do
        testElaborateExprBValTrue `shouldBe`Right (BVal True)
      it "Elaborates boolean value expressions" $ do
        testElaborateExprBValFalse `shouldBe` Right (BVal False)
      it "Elaborates variable expressions" $ do
        testElaborateExprInnerVar `shouldBe` Right (Var "$0")
      it "Elaborates more variable expressions" $ do
        testElaborateExprOuterVar `shouldBe` Right (Var "$0")
      it "Does not elaborate undeclared variable expressions" $ do
        testElaborateExprUndeclaredVar `shouldBe` Left "Variable x not found"
      it "Elaborates unary  expressions" $ do
        testElaborateExprNotOp `shouldBe` Right (NotOp (Var "$0"))
      it "Elaborates binary operation expressions" $ do
        testElaborateExprBinOp  `shouldBe`Right (BinOp AddS (Var "$0") (Var "$1"))
    describe "Elaborates instructions" $ do
      it "Elaborates declaration instructions" $ do
        testElaborateInstrDecl `shouldBe` Right (Decl Local TypeInt "$0" Nothing, [("x", "$0")], 0, 1)
      it "Elaborates declaration instructions with value initialization" $ do
        testElaborateInstrDeclInit `shouldBe` Right (Decl Local TypeInt "$0" (Just (Val 5)), [("x", "$0")], 0, 1)
      it "Elaborates assign instructions" $ do
        testElaborateInstrAssign `shouldBe` Right (Assign "$0" (Val 5), [("x", "$0")], 0, 1)
      it "Elaborates while instructions" $ do
        testElaborateInstrWhile `shouldBe` Right (While (Val 1) [Print (Var "$0")], [("x", "$0")], 0, 1)
      it "Elaborates if else instructions" $ do
        testElaborateInstrIfElse `shouldBe` Right (IfElse (Val 1) [Print (Var "x1")] [Print (Var "y1")], [("x", "x1"), ("y", "y1")], 0, 2)
      it "Elaborates if instructions" $ do
        testElaborateInstrIf `shouldBe` Right (If (Val 1) [Print (Var "$0")], [("x", "$0")], 0, 1)
      it "Elaborates print instructions" $ do
        testElaborateInstrPrint `shouldBe` Right (Print (Var "x1"), [("x", "$0")], 0, 1)
      it "Elaborates fork instructions" $ do
        testElaborateInstrFork `shouldBe` Right (Fork (Just 0) [Print (Var "$0")], [("x", "$0")], 1, 1)
      it "Elaborates lock instructions" $ do
        testElaborateInstrLock `shouldBe` Right (Lock "$0", [("x", "$0")], 0, 1)
      it "Elaborates unlock instructions" $ do
        testElaborateInstrUnlock `shouldBe` Right (Unlock "$0", [("x", "$0")], 0, 1)
    describe "Elaborates programs" $ do
      it "Elaborates empty programs" $ do
        testElaborateProgEmpty `shouldBe` Right ([], 0)
      it "Elaborates programs with one instruction" $ do
        testElaborateProgSingleDecl `shouldBe` Right ([Decl Local TypeInt "$0" Nothing], 0, 1)
      it "Elaborates programs with multiple instructions" $ do
        testElaborateProgMultipleInstrs `shouldBe` Right ([Decl Local TypeInt "$0" (Just (Val 5)),While (BVal True) [Decl Local TypeBool "$1" (Just (Val 5))]],0,2)
    describe "Removes BVal from expressions" $ do
      it "Removes BVal from value expressions" $ do
        testRemoveBValFromExprVal `shouldBe` (Val 5)
      it "Removes BVal from boolean value expressions" $ do
        testRemoveBValFromExprBValTrue `shouldBe` (Val 1)
      it "Removes BVal from boolean value expressions" $ do
        testRemoveBValFromExprBValFalse `shouldBe` (Val 0)
      it "Removes BVal from variable expressions" $ do
        testRemoveBValFromExprVar `shouldBe` (Var "x")
      it "Removes BVal from unary expressions" $ do
        testRemoveBValFromExprNotOp `shouldBe` (NotOp (Val 1))
      it "Removes BVal from binary operation expressions" $ do
        testRemoveBValFromExprBinOp `shouldBe` (BinOp AndS (Val 1) (Val 0))
    describe "Removes BVal from instructions" $ do
      it "Removes BVal from declaration instructions" $ do
        testRemoveBValFromInstrDecl `shouldBe` (Decl Local TypeInt "x" (Just (Val 1)))
      it "Removes BVal from assign instructions" $ do
        testRemoveBValFromInstrAssign `shouldBe` (Assign "x" (Val 0))
      it "Removes BVal from while instructions" $ do
        testRemoveBValFromInstrWhile `shouldBe` (While (Val 1) [Print (Val 1)])
      it "Removes BVal from if else instructions" $ do
        testRemoveBValFromInstrIfElse `shouldBe` (IfElse (Val 1) [Print (Val 1)] [Print (Val 1)])
      it "Removes BVal from if instructions" $ do
        testRemoveBValFromInstrIf `shouldBe`(If (Val 1) [Print (Val 1)])
      it "Removes BVal from print instructions" $ do
        testRemoveBValFromInstrPrint `shouldBe` (Print (Val 1))
    describe "Removes BVal from programs" $ do
      it "Removes BVal from empty programs" $ do
        testRemoveBValFromProgramEmpty `shouldBe` []
      it "Removes BVal from single instruction programs" $ do
        testRemoveBValFromProgramSingleInstr `shouldBe`[Assign "x" (Val 1)]
      it "Removes BVal from multiple instruction programs" $ do
        testRemoveBValFromProgramMultipleInstrs `shouldBe` [Decl Local TypeInt "x" (Just (Val 1)), Assign "x" (Val 0)]
  describe "Language (running code)" $ do
    describe "mandatory test programs" $ do
      it "runs 4 transactions, one for each thread, and prints the result of those transactions" $ do
        stdout <- capture_ $ runFile "./test/demos/mandatory/banking-system"
        stdout `shouldBe` "Sprockell 0 says 1000\nSprockell 0 says 1000\n"
      it "runs the Peterson's algorithm; in the critical section a variable is incremented" $ do
        stdout <- capture_ $ runFile "./test/demos/mandatory/peterson"
        stdout `shouldBe` "Sprockell 0 says 25\n"
    describe "legal examples" $ do
      it "overshadows a variable and prints both of them" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p0"
        stdout `shouldBe` "Sprockell 0 says 2\nSprockell 0 says 1\n"
      it "declare all type of legal variables" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p1"
        stdout `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 22\n"
      it "tests running two if's; one with a true condition and one with a false one" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p2"
        stdout `shouldBe` "Sprockell 0 says 10\n"
      it "tests running two if with else; one with a true condition and one with a false one" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p3"
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 0 says 10\n"
      it "prints the values from 0 to 9 using while" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p4"
        stdout `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 3\nSprockell 0 says 4\nSprockell 0 says 5\nSprockell 0 says 6\nSprockell 0 says 7\nSprockell 0 says 8\nSprockell 0 says 9\n"
      it "spawns 6 threads that try (not safely) to increment a shared variable of value 10" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p5"
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 11\nSprockell 3 says 11\nSprockell 4 says 11\nSprockell 5 says 11\nSprockell 6 says 11\n"
      it "spawns 6 threads increment safely a shared variable of value 10" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p6"
        stdout `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 12\nSprockell 3 says 13\nSprockell 4 says 14\nSprockell 5 says 15\nSprockell 6 says 16\n"
      it "runs 3 threads that increment a shared value sum by i = 1, i = 2 and i = 3, prints the result and prints the original i" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p7"
        stdout `shouldBe` "Sprockell 0 says 6\nSprockell 0 says 1\n"
      it "spawns 2 threads that increment a=0 by 1 and b=0 by 2 using 2 locks for synchronization" $ do
        stdout <- capture_ $ runFile "./test/demos/legal/p8"
        stdout `shouldBe` "Sprockell 0 says 2\nSprockell 0 says 4\n"
    describe "illegal examples" $ do
      it "shows that a thread can't access local variables of other threads" $ do
        runFile "./test/demos/illegal/p0" `shouldThrow` anyException
      it "defines a local lock" $ do
        runFile "./test/demos/illegal/p1" `shouldThrow` anyException
      it "declares a shared variable in a fork" $ do
        runFile "./test/demos/illegal/p2" `shouldThrow` anyException
      it "declares a shared variable inside an if" $ do
        runFile "./test/demos/illegal/p3" `shouldThrow` anyException
      it "forks inside an if" $ do
        runFile "./test/demos/illegal/p4" `shouldThrow` anyException
      it "prints a variable out of scope" $ do
        runFile "./test/demos/illegal/p5" `shouldThrow` anyException
      it "prints a uninitialized variable" $ do
        runFile "./test/demos/illegal/p6" `shouldThrow` anyException
      it "prints a uninitialized variable inside a fork" $ do
        runFile "./test/demos/illegal/p7" `shouldThrow` anyException
      it "runs an infinite while loop" $ do
        stdout <- timeout 1 $ runFile "./test/demos/illegal/p8"
        stdout `shouldBe` Nothing
      it "shared memory overflow" $ do
        runFile "./test/demos/illegal/p9" `shouldThrow` anyException
      it "local memory overflow" $ do
        runFile "./test/demos/illegal/p10" `shouldThrow` anyException
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
    describe "running generated instructions" $ do -- add another print with an complicated expression
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
testParseInstrIfElse = parse parseInstr "" "if (true) { int x = 5 } else { int y = 6 }"
-- expected: Right (IfElse (BVal True) [Decl Local TypeInt "x" (Just (Val 5))] [Decl Local TypeInt "y" (Just (Val 6))])

testParseInstrIf :: Either ParseError Instr
testParseInstrIf = parse parseInstr "" "if (true) { int x = 5 }"
-- expected: Right (If (BVal True) [Decl Local TypeInt "x" (Just (Val 5))])

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
testParseProgram = parse parseProgram "" "int x = 5 while (true) { int y = 6 }"
-- expected: Right [Decl Local TypeInt "x" (Just (Val 5)), While (BVal True) [Decl Local TypeInt "y" (Just (Val 6))]]

-- Test cases for comments
testParseWithComment :: Either ParseError Program
testParseWithComment = parse parseProgram  "" "int x = 5 // this is a comment\nint y = 6"
-- expected: Right [Decl Local TypeInt "x" (Just (Val 5)), Decl Local TypeInt "y" (Just (Val 6))]

-- Test cases for error scenarios
testParseErrorIncompleteInstr :: Either ParseError Program
testParseErrorIncompleteInstr = runParseProgram "while (true) { int x = }"
-- expected: ParseError

testParseErrorIncompleteExpr :: Either ParseError Program
testParseErrorIncompleteExpr = runParseProgram "while (true) { int x = 5 + }"
-- expected: ParseError

testParseErrorInvalidInput :: Either ParseError Program
testParseErrorInvalidInput = runParseProgram "while (true) { int x = 5 + 7} invalid input"
-- expected: ParseError

-- #####################################################################################################################
-- #                                                  Type Checking                                                    #
-- #####################################################################################################################

-- Test cases for lookupVarType
testLookupVarTypeFound :: Either String Type
testLookupVarTypeFound = lookupVarType "x" [("x", (Local, TypeInt))]
-- expected: Right TypeInt

testLookupVarTypeNotFound :: Either String Type
testLookupVarTypeNotFound = lookupVarType "y" [("x", (Local, TypeInt))]
-- expected: Left "Variable y not found in scope"

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
-- Expected: Left "Type error cannot use unary on expr Var \"x\""

testInferExprTypeBinOpAdd :: Either String Type
testInferExprTypeBinOpAdd = inferExprType [("x", (Local, TypeInt)), ("y", (Local, TypeInt))] (BinOp AddS (Var "x") (Var "y"))
-- Expected: Right TypeInt

testInferExprTypeBinOpError :: Either String Type
testInferExprTypeBinOpError = inferExprType [("x", (Local, TypeInt)), ("y", (Local, TypeBool))] (BinOp AddS (Var "x") (Var "y"))
-- Expected: Left "Type error in binary operation AddS"

-- Test cases for checkInstr
testCheckInstrDecl :: Either String TypeEnv
testCheckInstrDecl = checkInstr [] (Decl Local TypeInt "x" Nothing) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrDeclInit :: Either String TypeEnv
testCheckInstrDeclInit = checkInstr [] (Decl Local TypeInt "x" (Just (Val 5))) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrDeclTypeError :: Either String TypeEnv
testCheckInstrDeclTypeError =  checkInstr [] (Decl Local TypeInt "x" (Just (BVal True))) GlobalScope
-- Expected: Left "Type error in declaration of x"

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
testCheckInstrDeclDuplicateDeclarationError = checkInstr [("x", (Local, TypeInt))] (Decl Local TypeBool "x" Nothing) GlobalScope
-- Expected: Left "Duplicate declaration of variable: x"

testCheckInstrAssign :: Either String TypeEnv
testCheckInstrAssign = checkInstr [("x", (Local, TypeInt))] (Assign "x" (Val 5)) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrAssignError :: Either String TypeEnv
testCheckInstrAssignError = checkInstr [("x", (Local, TypeInt))] (Assign "x" (BVal True)) GlobalScope
-- Expected: Left "Type error in assignment to x"

testCheckInstrWhile :: Either String TypeEnv
testCheckInstrWhile = checkInstr [("x", (Local, TypeInt))] (While (BVal True) [Assign "x" (Val 5)]) GlobalScope
-- Expected: Right [("x", (Local, TypeInt))]

testCheckInstrWhileError :: Either String TypeEnv
testCheckInstrWhileError =  checkInstr [("x", (Local, TypeInt))] (While (Val 5) [Assign "x" (Val 5)]) GlobalScope
-- Expected: Left "Type error in While condition"

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
-- Expected: Left "Variable x not found in scope"

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
-- Expected: Left "Variable f1 not found in scope"

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
-- Expected: Left "Variable a not found in scope"

testCheckProgramError_8 :: Either String TypeEnv
testCheckProgramError_8 = checkProg [] [Print (Var "a"),Decl Local TypeInt "a" (Just (Val 10))] GlobalScope
-- Expected: Left "Variable a not found in scope"

testCheckProgramError_9 :: Either String TypeEnv
testCheckProgramError_9 = checkProg [] [Fork Nothing [Print (Var "a")],Decl Shared TypeInt "a" (Just (Val 10))] GlobalScope
-- Expected: Left "Variable a not found in scope"

-- #####################################################################################################################
-- #                                                Program Elaboration                                                #
-- #####################################################################################################################

-- Test cases for lookupVarName
testLookupVarNameFound :: Either String String
testLookupVarNameFound = lookupVarName "x" [("x", "$0")]
-- expected: Right TypeInt

testLookupVarNameNotFound :: Either String String
testLookupVarNameNotFound = lookupVarName "y" [("x", "$0")]
-- expected: Left "Variable y not found"

-- Test cases for elaborateExpr
testElaborateExprVal :: Either String Expr
testElaborateExprVal = elaborateExpr [] [] (Val 5)
-- expected: Right (Val 5)

testElaborateExprBValTrue :: Either String Expr
testElaborateExprBValTrue = elaborateExpr [] [] (BVal True)
-- expected: Right (BVal True)

testElaborateExprBValFalse :: Either String Expr
testElaborateExprBValFalse = elaborateExpr [] [](BVal False)
-- expected: Right (BVal False)

testElaborateExprInnerVar :: Either String Expr
testElaborateExprInnerVar = elaborateExpr [] [("x", "$0")] (Var "x")
-- expected: Right (Var "$0")

testElaborateExprOuterVar :: Either String Expr
testElaborateExprOuterVar = elaborateExpr [("x", "$0")] [] (Var "x")
-- expected: Right (Var "$0")

testElaborateExprUndeclaredVar :: Either String Expr
testElaborateExprUndeclaredVar = elaborateExpr [] [] (Var "x")
-- expected: Left "Variable x not found"

testElaborateExprNotOp :: Either String Expr
testElaborateExprNotOp = elaborateExpr [] [("x", "$0")] (NotOp (Var "x"))
-- expected: Right (NotOp (Var "$0"))

testElaborateExprBinOp :: Either String Expr
testElaborateExprBinOp = elaborateExpr [] [("x", "$0"), ("y", "$1")] (BinOp AddS (Var "x") (Var "y"))
-- expected: Right (BinOp AddS (Var "$0") (Var "$1"))

-- Test cases for elaborateInstr
testElaborateInstrDecl :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrDecl = elaborateInstr (Decl Local TypeInt "x" Nothing) [] [] 0 0
-- expected: Right (Decl Local TypeInt "$0" Nothing, [("x", "$0")], 0, 1)

testElaborateInstrDeclInit :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrDeclInit = elaborateInstr (Decl Local TypeInt "x" (Just (Val 5))) [] [] 0 0
-- expected: Right (Decl Local TypeInt "$0" (Just (Val 5)), [("x", "$0")], 0, 1)

testElaborateInstrAssign :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrAssign = elaborateInstr (Assign "x" (Val 5)) [] [("x", "$0")] 0 1
-- expected: Right (Assign "$0" (Val 5), [("x", "$0")], 0, 1)

testElaborateInstrWhile :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrWhile = elaborateInstr (While (BVal True) [Print (Var "x")]) [] [("x", "$0")] 0 1
-- expected: Right (While (Val 1) [Print (Var "$0")], [("x", "$0")], 0, 1)

testElaborateInstrIfElse :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrIfElse = elaborateInstr (IfElse (BVal True) [Print (Var "x")] [Print (Var "y")]) [] [("x", "$0"), ("y", "$1")] 0 2
-- expected: Right (IfElse (Val 1) [Print (Var "x1")] [Print (Var "y1")], [("x", "x1"), ("y", "y1")], 0, 2)

testElaborateInstrIf :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrIf = elaborateInstr (If (BVal True) [Print (Var "x")]) [] [("x", "$0")] 0 1
-- expected: Right (If (Val 1) [Print (Var "$0")], [("x", "$0")], 0, 1)

testElaborateInstrPrint :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrPrint = elaborateInstr (Print (Var "x")) [] [("x", "$0")] 0 1
-- expected: Right (Print (Var "x1"), [("x", "$0")], 0, 1)

testElaborateInstrFork :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrFork = elaborateInstr (Fork Nothing [Print (Var "x")]) [] [("x", "$0")] 0 1
-- expected: Right (Fork (Just 0) [Print (Var "$0")], [("x", "$0")], 1, 1)

testElaborateInstrLock :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrLock = elaborateInstr (Lock "x") [] [("x", "$0")] 0 1
-- expected: Right (Lock "$0", [("x", "$0")], 0, 1)

testElaborateInstrUnlock :: Either String (Instr, VarEnv, Int, Int)
testElaborateInstrUnlock = elaborateInstr (Unlock "x") [] [("x", "$0")] 0 1
-- expected: Right (Unlock "$0", [("x", "$0")], 0, 1)

-- Test cases for elaborateProg
testElaborateProgEmpty ::Either String (Program, Int, Int)
testElaborateProgEmpty = elaborateProg [] [] [] 0 0
-- expected: Right ([], 0)

testElaborateProgSingleDecl :: Either String (Program, Int, Int)
testElaborateProgSingleDecl = elaborateProg [Decl Local TypeInt "x" Nothing] [] [] 0 0
-- expected: Right ([Decl Local TypeInt "$0" Nothing], 0, 1)

testElaborateProgMultipleInstrs :: Either String (Program, Int, Int)
testElaborateProgMultipleInstrs = elaborateProg [Decl Local TypeInt "x" (Just (Val 5)), While (BVal True) [Decl Local TypeBool "x" (Just (Val 5))]] [] [] 0 0
-- expected: Right ([Decl Local TypeInt "$0" (Just (Val 5)),While (BVal True) [Decl Local TypeBool "$1" (Just (Val 5))]],0,2)

-- Test cases for removeBValFromExpr
testRemoveBValFromExprVal :: Expr
testRemoveBValFromExprVal = removeBValFromExpr (Val 5)
-- Expected: (Val 5)

testRemoveBValFromExprBValTrue :: Expr
testRemoveBValFromExprBValTrue = removeBValFromExpr (BVal True)
-- Expected: (Val 1)

testRemoveBValFromExprBValFalse :: Expr
testRemoveBValFromExprBValFalse = removeBValFromExpr (BVal False)
-- Expected: (Val 0)

testRemoveBValFromExprVar :: Expr
testRemoveBValFromExprVar = removeBValFromExpr (Var "x")
-- Expected: (Var "x")

testRemoveBValFromExprNotOp :: Expr
testRemoveBValFromExprNotOp = removeBValFromExpr (NotOp (BVal True))
-- Expected: (NotOp (Val 1))

testRemoveBValFromExprBinOp :: Expr
testRemoveBValFromExprBinOp = removeBValFromExpr (BinOp AndS (BVal True) (BVal False))
-- Expected (BinOp AndS (Val 1) (Val 0))

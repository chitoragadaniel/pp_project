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
  describe "CodeGen" $ do
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
        actual <- capture_ (run (codeGen printProgram))
        actual `shouldBe` "Sprockell 0 says 100\n"
      it "declares all types of legal variables" $ do
        actual <- capture_ (run (codeGen declProgram))
        actual `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 22\n"
      it "tests running two if's; one with a true condition and one with a false one" $ do
        actual <- capture_ (run (codeGen ifProgram))
        actual `shouldBe` "Sprockell 0 says 10\n"
      it "tests running two if with else; one with a true condition and one with a false one" $ do
        actual <- capture_ (run (codeGen ifElseProgram))
        actual `shouldBe` "Sprockell 0 says 10\nSprockell 0 says 10\n"
      it "prints the values from 0 to 9 using while" $ do
        actual <- capture_ (run (codeGen whileProgram))
        actual `shouldBe` "Sprockell 0 says 0\nSprockell 0 says 1\nSprockell 0 says 2\nSprockell 0 says 3\nSprockell 0 says 4\nSprockell 0 says 5\nSprockell 0 says 6\nSprockell 0 says 7\nSprockell 0 says 8\nSprockell 0 says 9\n"
      it "spawns 6 threads that try (not safely) to increment a shared variable of value 10" $ do
        actual <- capture_ (run (codeGen threadProgram))
        actual `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 11\nSprockell 3 says 11\nSprockell 4 says 11\nSprockell 5 says 11\nSprockell 6 says 11\n"
      it "spawns 6 threads increment safely a shared variable of value 10" $ do
        actual <- capture_ (run (codeGen threadSafeProgram))
        actual `shouldBe` "Sprockell 0 says 10\nSprockell 1 says 11\nSprockell 2 says 12\nSprockell 3 says 13\nSprockell 4 says 14\nSprockell 5 says 15\nSprockell 6 says 16\n"
      it "spawns 2 threads (one being nested) and both increment a shared variable" $ do
        actual <- capture_ (run (codeGen nestedThreadProgram))
        actual `shouldBe` ""
      it "spawns 2 threads that increment a by 1 and b by 2 using 2 locks for synchronization" $ do
        actual <- capture_ (run (codeGen multipleLocksProgram))
        actual `shouldBe` "Sprockell 0 says 2\nSprockell 0 says 4\n"
    describe "others" $ do
      it "gets the IR's of all the spawned threads" $ do
        testForkAST `shouldBe` [[Decl Local TypeInt "f1" Nothing,Fork (Just 2) [Decl Local TypeInt "f2" Nothing]],[Decl Local TypeInt "f2" Nothing],[Decl Local TypeInt "f3" Nothing]]
--      it "runs an infinite while loop" $ do
--        actual <- capture_ $ timeout 1 $ run $ codeGen infiniteWhileProgram
--        actual `shouldBe` Nothing


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

printProgram :: Program
printProgram = [Print $ Val 100]

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

module Language.LoopGotoWhile.Base.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.LoopGotoWhile.Base.Base

tests :: [Test]
tests = [ testCase "base/defaultvalue" testDefaultvalue
        , testCase "base/assignment1" testAssignment1
        , testCase "base/assignment2" testAssignment2
        , testCase "base/assignment3" testAssignment3
        , testCase "base/assignment4" testAssignment4
        , testCase "base/arithmetic1" testArithmetic1
        , testCase "base/arithmetic2" testArithmetic2
        , testCase "base/arguments1" testArguments1
        , testCase "base/parsing1" testParsing1
        , testCase "base/parsing2" testParsing2
        , testCase "base/parsing3" testParsing3
        , testCase "base/parsing4" testParsing4
        , testCase "base/parsing5" testParsing5
        , testCase "base/parsing6" testParsing6
        ]

testDefaultvalue :: Assertion
testDefaultvalue = right (runProgram "" []) >>= (@?= 0)

testAssignment1 :: Assertion
testAssignment1 = right (runProgram "x0 := x1 + 1" []) >>= (@?= 1)

testAssignment2 :: Assertion
testAssignment2 = right (runProgram "x1 := x1 + 1" []) >>= (@?= 0)

testAssignment3 :: Assertion
testAssignment3 = right (runProgram "x1 := x1 + 1; x0 := x1 + 0" []) >>= (@?= 1)

testAssignment4 :: Assertion
testAssignment4 = right (runProgram "x2 := x9 + 2; x1 := x2 + 2; x0 := x1 + 2" []) >>= (@?= 6)

testArithmetic1 :: Assertion
testArithmetic1 = right (runProgram "x0 := x1 + 10" []) >>= (@?= 10)

testArithmetic2 :: Assertion
testArithmetic2 = right (runProgram "x0 := x1 + 10; x0 := x1 - 100" []) >>= (@?= 0)

testParsing1 :: Assertion
testParsing1 = right (runProgram "x0:=x121+827;x1:=x2+0" []) >>= (@?= 827)

testParsing2 :: Assertion
testParsing2 = right (runProgram "x0:=  x121 \n\n +827\n ;\n x1 :=x2 \n + 0" []) >>= (@?= 827)

testParsing3 :: Assertion
testParsing3 = assertBool "" =<< isLeft (runProgram "x := x1 + 2" [])

testParsing4 :: Assertion
testParsing4 = assertBool "" =<< isLeft (runProgram "x0 := x1 + 2;" [])

testParsing5 :: Assertion
testParsing5 = assertBool "" =<< isLeft (runProgram "x0 = x1 + 2" [])

testParsing6 :: Assertion
testParsing6 = assertBool "" =<< isLeft (runProgram "x0 := x1 + x2" [])

testArguments1 :: Assertion
testArguments1 = right (runProgram "x0 := x2 + 0" [1,2]) >>= (@?= 2)


right :: Show a => IO (Either a b) -> IO b
right e = do e' <- e
             case e' of
               (Right r) -> return r
               (Left  l) -> error $ show l

isLeft :: IO (Either a b) -> IO Bool
isLeft e = do e' <- e
              case e' of
                (Left l) -> return True
                _        -> return False

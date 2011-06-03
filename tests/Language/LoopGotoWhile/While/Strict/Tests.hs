module Language.LoopGotoWhile.While.Strict.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.LoopGotoWhile.Shared.Util
import Language.LoopGotoWhile.While.Strict (eval, parse)


tests :: [Test]
tests = [ testCase "while/strict/assignment1" testAssignment1
        , testCase "while/strict/assignment2" testAssignment2
        , testCase "while/strict/assignment3" testAssignment3
        , testCase "while/strict/assignment4" testAssignment4
        , testCase "while/strict/arithmetic1" testArithmetic1
        , testCase "while/strict/arithmetic2" testArithmetic2
        , testCase "while/strict/arguments1" testArguments1
        , testCase "while/strict/looping1" testLooping1
        , testCase "while/strict/looping2" testLooping2
        , testCase "while/strict/parsing1" testParsing1
        , testCase "while/strict/parsing2" testParsing2
        , testCase "while/strict/parsing3" testParsing3
        , testCase "while/strict/parsing4" testParsing4
        , testCase "while/strict/parsing5" testParsing5
        , testCase "while/strict/parsing6" testParsing6
        , testCase "while/strict/parsing7" testParsing7
        , testCase "while/strict/parsing8" testParsing8
        , testCase "while/strict/parsing9" testParsing9
        , testCase "while/strict/parsing10" testParsing10
        ]


testAssignment1 :: Assertion
testAssignment1 
    = runProgram' [] "x0 := x1 + 1"  
  @?= 1

testAssignment2 :: Assertion
testAssignment2 
    = runProgram' [] "x1 := x1 + 1"  
  @?= 0

testAssignment3 :: Assertion
testAssignment3 
    = runProgram' [] "x1 := x1 + 1; x0 := x1 + 0"  
  @?= 1

testAssignment4 :: Assertion
testAssignment4 
    = runProgram' [] "x2 := x9 + 2; x1 := x2 + 2; x0 := x1 + 2"  
  @?= 6

testArithmetic1 :: Assertion
testArithmetic1 
    = runProgram' [] "x0 := x1 + 10"  
  @?= 10

testArithmetic2 :: Assertion
testArithmetic2 
    = runProgram' [] "x0 := x1 + 10; x0 := x1 - 100"
  @?= 0

testArguments1 :: Assertion
testArguments1 
    = runProgram' [1,2] "x0 := x2 + 0"  
  @?= 2

testLooping1 :: Assertion
testLooping1 
    = runProgram' [10] "x3 := x1 + 0; WHILE x3 != 0 DO x3 := x3 - 1; x0 := x0 + 1; x1 := x2 + 1 END" 
  @?= 10

testLooping2 :: Assertion
testLooping2 
    = runProgram' [8,7] "WHILE x1 != 0 DO x1 := x1 - 1; x3 := x2 + 0; WHILE x3 != 0 DO x3 := x3 - 1; x0 := x0 + 1 END END" 
  @?= 8 * 7

testParsing1 :: Assertion
testParsing1 
    = runProgram' [] "x0:=x121+827;x1:=x2+0" 
  @?= 827

testParsing2 :: Assertion
testParsing2 
    = runProgram' [] "x0:=  x121 \n\n +827\n ;\n x1 :=x2 \n + 0;  \n x5 := x5 + 5; WHILE \n\n x5 != 0 DO x5 := x5 - 1; x8 := x8 +\n0END" 
  @?= 827

testParsing3 :: Assertion
testParsing3 = assertBool "" $ isLeft $ runProgram [] $
    "x := x1 + 2" 

testParsing4 :: Assertion
testParsing4 = assertBool "" $ isLeft $ runProgram [] $
    "x0 := x1 + 2;"

testParsing5 :: Assertion
testParsing5 = assertBool "" $ isLeft $ runProgram [] $
    "x0 = x1 + 2" 

testParsing6 :: Assertion
testParsing6 = assertBool "" $ isLeft $ runProgram [] $
    "x0 := x1 + x2"

testParsing7 :: Assertion
testParsing7 = assertBool "" $ isLeft $ runProgram [] $
    "WHILE x0 x1 := x1 + 1 END"

testParsing8 :: Assertion
testParsing8 = assertBool "" $ isLeft $ runProgram [] $
    "while x0 do x1 := x1 + 1 end"

testParsing9 :: Assertion
testParsing9 = assertBool "" $ isLeft $ runProgram [] $ 
    "x0 := x1 + 1\n" ++
    "x3 := x1 + 1"

testParsing10 :: Assertion
testParsing10 = assertBool "" $ isLeft $ runProgram [] " "


-- Helper

runProgram :: [Integer] -> String -> Either String Integer
runProgram = flip $ mkStdRunner parse eval

runProgram' :: [Integer] -> String -> Integer
runProgram' = flip $ mkStdRunner' parse eval

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

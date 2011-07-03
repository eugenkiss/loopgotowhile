module Language.LoopGotoWhile.Goto.Strict.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.LoopGotoWhile.Shared.Util
import Language.LoopGotoWhile.Goto.Strict (eval, parse)


tests :: [Test]
tests = [ testCase "goto/strict/assignment1" testAssignment1
        , testCase "goto/strict/assignment2" testAssignment2
        , testCase "goto/strict/assignment3" testAssignment3
        , testCase "goto/strict/assignment4" testAssignment4
        , testCase "goto/strict/arithmetic1" testArithmetic1
        , testCase "goto/strict/arithmetic2" testArithmetic2
        , testCase "goto/strict/arguments1" testArguments1
        , testCase "goto/strict/looping1" testLooping1
        , testCase "goto/strict/looping2" testLooping2
        , testCase "goto/strict/parsing1" testParsing1
        {-, testCase "goto/strict/parsing2" testParsing2-}
        , testCase "goto/strict/parsing3" testParsing3
        , testCase "goto/strict/parsing4" testParsing4
        , testCase "goto/strict/parsing5" testParsing5
        , testCase "goto/strict/parsing6" testParsing6
        , testCase "goto/strict/parsing7" testParsing7
        , testCase "goto/strict/parsing8" testParsing8
        , testCase "goto/strict/parsing9" testParsing9
        , testCase "goto/strict/parsing10" testParsing10
        , testCase "goto/strict/parsing11" testParsing11
        , testCase "goto/strict/parsing12" testParsing12
        , testCase "goto/strict/parsing13" testParsing13
        , testCase "goto/strict/parsing14" testParsing14
        ]


testAssignment1 :: Assertion
testAssignment1 
    = runProgram' [] "M1: x0 := x1 + 1; M2: HALT"  
  @?= 1

testAssignment2 :: Assertion
testAssignment2 
    = runProgram' [] "M1: x1 := x1 + 1; M2: HALT"  
  @?= 0

testAssignment3 :: Assertion
testAssignment3 
    = runProgram' [] "M1: x1 := x1 + 1; M2: x0 := x1 + 0; M3: HALT"  
  @?= 1

testAssignment4 :: Assertion
testAssignment4 
    = runProgram' [] "M1: x2 := x9 + 2; M2: x1 := x2 + 2; M3: x0 := x1 + 2; M4: HALT"  
  @?= 6

testArithmetic1 :: Assertion
testArithmetic1 
    = runProgram' [] "M1: x0 := x1 + 10; M2: HALT"  
  @?= 10

testArithmetic2 :: Assertion
testArithmetic2 
    = runProgram' [] "M1: x0 := x1 + 10; M2: x0 := x1 - 100; M3: HALT"
  @?= 0

testArguments1 :: Assertion
testArguments1 
    = runProgram' [1,2] "M1: x0 := x2 + 0; M2: HALT"  
  @?= 2

testLooping1 :: Assertion
testLooping1 = runProgram' [10] p @?= 10
  where p = "M1: x3 := x1 + 0;" ++ 
            "M2: IF x3 = 0 THEN GOTO M7;" ++
            "M3: x3 := x3 - 1; " ++
            "M4: x0 := x0 + 1; " ++
            "M5: x1 := x2 + 1;" ++
            "M6: GOTO M2;" ++
            "M7: HALT"

testLooping2 :: Assertion
testLooping2 = runProgram' [8,7] p @?= 8 * 7
  where p = "M1: IF x1 = 0 THEN GOTO M9;" ++
            "M2: x1 := x1 - 1; " ++
            "M3: x3 := x2 + 0; " ++
            "M4: IF x3 = 0 THEN GOTO M8;" ++
            "M5: x3 := x3 - 1; " ++
            "M6: x0 := x0 + 1; " ++
            "M7: GOTO M4; " ++ 
            "M8: GOTO M1; " ++
            "M9: HALT"

testParsing1 :: Assertion
testParsing1 
    = runProgram' [] "M1:x0:=x121+827;M2:x1:=x2+0;M3:HALT" 
  @?= 827

{-testParsing2 :: Assertion-}
{-testParsing2 = runProgram' [] p @?= 827-}
  {-where p = "x0:=  x121 \n\n +827\n ;\n" ++-}
            {-"x1 :=x2 \n + 0;  \n x5 := x5 + 5;" ++-}
            {-"M1 \n : \n\n x5 != 0; " ++-}
            {-"x5 := x5 - 1; x8 := x8 +\n0 ;" ++-}
            {-"IF \n\n x5 != 0 THEN \n GOTO \n\n M1 \nEND"-}

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
    "M: x1 := x1 + 1; M2: HALT"

testParsing8 :: Assertion
testParsing8 = assertBool "" $ isLeft $ runProgram [] $
    "m1: x1 := x1 + 1; M2: HALT"

-- Label numbers are not successive!
testParsing9 :: Assertion
testParsing9 = assertBool "" $ isLeft $ runProgram [] $
    "M1: x1 := x1 + 1; M3: x1 := x1 + 1; M2: HALT"

testParsing10 :: Assertion
testParsing10 = assertBool "" $ isLeft $ runProgram [] $
    "M1: x1 := x1 + 1; M2: x1 := x1 + 1" -- neither ends with HALT nor GOTO

testParsing11 :: Assertion
testParsing11 = assertBool "" $ isLeft $ runProgram [] $ 
    "x0 := x1 + 1\n" ++
    "x3 := x1 + 1"

testParsing12 :: Assertion
testParsing12 = assertBool "" $ isLeft $ runProgram [] " " -- neither ends with HALT nor GOTO

testParsing13 :: Assertion
testParsing13 = assertBool "" $ isRight $ runProgram [] "M1: HALT"

testParsing14 :: Assertion
testParsing14 = assertBool "" $ isRight $ runProgram [] "M1: GOTO M1"


-- Helper

runProgram :: [Integer] -> String -> Either String Integer
runProgram = flip $ mkStdRunner parse eval

runProgram' :: [Integer] -> String -> Integer
runProgram' = flip $ mkStdRunner' parse eval

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

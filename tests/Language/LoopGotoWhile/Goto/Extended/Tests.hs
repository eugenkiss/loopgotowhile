module Language.LoopGotoWhile.Goto.Extended.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.LoopGotoWhile.Shared.Util
import Language.LoopGotoWhile.Goto.Extended (parse, eval)


tests :: [Test]
tests = [ testCase "goto/extended/parsing1" testParsing1
        , testCase "goto/extended/parsing2" testParsing2
        , testCase "goto/extended/strict1" testStrict1
        , testCase "goto/extended/comments1" testComments1
        , testCase "goto/extended/comments2" testComments2
        , testCase "goto/extended/comments3" testComments3
        , testCase "goto/extended/assignment1" testAssignment1
        , testCase "goto/extended/assignment2" testAssignment2
        , testCase "goto/extended/assignment3" testAssignment3
        , testCase "goto/extended/arithmetic1" testArithmetic1
        , testCase "goto/extended/arithmetic2" testArithmetic2
        , testCase "goto/extended/arithmetic3" testArithmetic3
        , testCase "goto/extended/arithmetic4" testArithmetic4
        , testCase "goto/extended/arithmetic5" testArithmetic5
        , testCase "goto/extended/arithmetic6" testArithmetic6
        , testCase "goto/extended/arithmetic7" testArithmetic7
        , testCase "goto/extended/arithmetic8" testArithmetic8
        , testCase "goto/extended/arithmetic9" testArithmetic9
        , testCase "goto/extended/arithmetic10" testArithmetic10
        , testCase "goto/extended/arithmetic11" testArithmetic11
        , testCase "goto/extended/arithmetic12" testArithmetic12
        , testCase "goto/extended/control2" testControl2
        , testCase "goto/extended/control3" testControl3
        , testCase "goto/extended/control4" testControl4
        , testCase "goto/extended/control5" testControl5
        , testCase "goto/extended/control6" testControl6
        ]


testParsing1 :: Assertion
testParsing1 = assertBool "" $ isLeft $ runProgram [] $ " "

-- There must not be duplicate labels.
testParsing2 :: Assertion
testParsing2 = assertBool "" $ isLeft $ runProgram [] $ 
    "M1: x0 := 0; M1: x0 := 1; HALT"

-- A valid strict program should be a valid extended Program.
testStrict1 :: Assertion
testStrict1 = assertBool "" $ isRight $ runProgram [] $ 
    "M1: x0 := x1 + 3;"           ++
    "M2: IF x0 = 5 THEN GOTO M5;" ++
    "M3: x0 := x2 + 3;"           ++
    "M4: GOTO M6;"                ++
    "M5: x0 := x3 + 3;"           ++
    "M6: x0 := x4 + 3;"           ++
    "M7: HALT"

testComments1 :: Assertion
testComments1 = assertBool "" $ isRight $ runProgram [] $
    "// This is a comment\n" ++
    "x0 := x1 + 2"

testComments2 :: Assertion
testComments2 = assertBool "" $ isRight $ runProgram [] $
    "x0 := x1 + 2; // This is a comment\n" ++
    "x0 := x1 + 2"

testComments3 :: Assertion
testComments3 = assertBool "" $ isRight $ runProgram [] $
    "x0 := x1 + 2 /*" ++
    "This is a longer\n comment" ++
    "*/; x0 := x1 + 2"

testAssignment1 :: Assertion
testAssignment1 
    = runProgram' [] "x0 := x1 + 2"  
  @?= 2

testAssignment2 :: Assertion
testAssignment2 
    = runProgram' [] "x0 := 2"  
  @?= 2

testAssignment3 :: Assertion
testAssignment3 
    = runProgram' [] "x1 := 2; x0 := x1"  
  @?= 2

testArithmetic1 :: Assertion
testArithmetic1 
    = runProgram' [5, 6] "x0 := x1 + x2"  
  @?= 11

testArithmetic2 :: Assertion
testArithmetic2 
    = runProgram' [6, 5] "x0 := x1 - x2"  
  @?= 1

testArithmetic3 :: Assertion
testArithmetic3 
    = runProgram' [6, 9] "x0 := x1 - x2"  
  @?= 0

testArithmetic4 :: Assertion
testArithmetic4 
    = runProgram' [6, 5] "x0 := x1 * x2"  
  @?= 30

testArithmetic5 :: Assertion
testArithmetic5 
    = runProgram' [6, 0] "x0 := x1 * x2"  
  @?= 0

testArithmetic6 :: Assertion
testArithmetic6 
    = runProgram' [4, 2] "x0 := x1^x2"  
  @?= 16

testArithmetic7 :: Assertion
testArithmetic7 
    = runProgram' [4, 0] "x0 := x1^x2"  
  @?= 1

testArithmetic8 :: Assertion
testArithmetic8
    = runProgram' [8, 2] "x0 := x1 / x2" 
  @?= 4

testArithmetic9 :: Assertion
testArithmetic9
    = runProgram' [9, 2] "x0 := x1 / x2" 
  @?= 4

testArithmetic10 :: Assertion
testArithmetic10 
    = runProgram' [0, 2] "x0 := x1 / x2"  
  @?= 0

testArithmetic11 :: Assertion
testArithmetic11
    = runProgram' [7, 3] "x0 := x1 % x2" 
  @?= 1

testArithmetic12 :: Assertion
testArithmetic12
    = runProgram' [10, 1] "x0 := 16 / 2^2 + (x1 * (x2 % 2)) - 1" 
  @?= 13

testControl2 :: Assertion
testControl2 = runProgram' [10, 1] p @?= 13
 where p = "c := 16 / 2^2 + (x1 * (x2 % 2)) - 1;" ++
           "M1: c := c - 1;"                      ++
           "x0 := x0 + 1;"                        ++
           "x0 := x0 - 1;"                        ++
           "x0 := x0 + 1;"                        ++
           "IF c = 0 THEN HALT END;"              ++
           "GOTO M1"

testControl3 :: Assertion
testControl3 = runProgram' [10, 1] p @?= 26
 where p = "c0 := 16 / 2^2 + (x1 * (x2 % 2)) - 1;" ++
           "M1: c0 := c0 - 1;"                     ++
           "    c1 := 2;"                          ++
           "M2:   c1 := c1 - 1;"                   ++
           "      x0 := x0 + 1;"                   ++
           "      x0 := x0 - 1;"                   ++
           "      x0 := x0 + 1;"                   ++
           "      IF c1 != 0 THEN GOTO M2 END;"    ++
           "    IF c0 != 0 THEN GOTO M1 END"

testControl4 :: Assertion
testControl4 = runProgram' [] p @?= 1
  where p = "IF 2 = 2 THEN x0 := 1 END"

testControl5 :: Assertion
testControl5 = runProgram' [] p @?= 1
  where p = "IF 2 >= 2 THEN x0 := 1 ELSE x0 := 2 END"

testControl6 :: Assertion
testControl6 = runProgram' [10, 1] p @?= 42
 where p = "IF !(16 / 2^2 + (x1 * (x2 % 2)) - 1 < 8 && x1 >= x2 || 2 = 2) THEN" ++
           "      c := 2;"                                                      ++
           "  M1: c  := c  - 1;"                                                ++
           "      x0 := x0 + 1;"                                                ++
           "      x0 := x0 - 1;"                                                ++
           "      x0 := x0 + 1;"                                                ++
           "      IF c != 0 THEN GOTO M1 END "                                  ++ -- space important!
           "ELSE "                                                              ++
           "  IF !(!(3 >= 3)) THEN"                                             ++
           "    x0 := 42 "                                                      ++
           "  END "                                                             ++ -- space important!
           "END"                                                               


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

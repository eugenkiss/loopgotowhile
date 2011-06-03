module Language.LoopGotoWhile.Loop.Extended.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.LoopGotoWhile.Shared.Util
import Language.LoopGotoWhile.Loop.Extended (parse, eval)


tests :: [Test]
tests = [ testCase "loop/extended/parsing1" testParsing1
        , testCase "loop/extended/comments1" testComments1
        , testCase "loop/extended/comments2" testComments2
        , testCase "loop/extended/comments3" testComments3
        , testCase "loop/extended/assignment1" testAssignment1
        , testCase "loop/extended/assignment2" testAssignment2
        , testCase "loop/extended/assignment3" testAssignment3
        , testCase "loop/extended/arithmetic1" testArithmetic1
        , testCase "loop/extended/arithmetic2" testArithmetic2
        , testCase "loop/extended/arithmetic3" testArithmetic3
        , testCase "loop/extended/arithmetic4" testArithmetic4
        , testCase "loop/extended/arithmetic5" testArithmetic5
        , testCase "loop/extended/arithmetic6" testArithmetic6
        , testCase "loop/extended/arithmetic7" testArithmetic7
        , testCase "loop/extended/arithmetic8" testArithmetic8
        , testCase "loop/extended/arithmetic9" testArithmetic9
        , testCase "loop/extended/arithmetic10" testArithmetic10
        , testCase "loop/extended/arithmetic11" testArithmetic11
        , testCase "loop/extended/arithmetic12" testArithmetic12
        , testCase "loop/extended/control1" testControl1
        , testCase "loop/extended/control2" testControl2
        , testCase "loop/extended/control3" testControl3
        , testCase "loop/extended/control4" testControl4
        , testCase "loop/extended/control5" testControl5
        ]

testParsing1 :: Assertion
testParsing1 = assertBool "" $ isLeft $ runProgram [] $ " "

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

testControl1 :: Assertion
testControl1 
    = runProgram' [10, 1] p @?= 13
 where p = "LOOP 16 / 2^2 + (x1 * (x2 % 2)) - 1 DO" ++
           "  x0 := x0 + 1;"                        ++ 
           "  x0 := x0 - 1;"                        ++ 
           "  x0 := x0 + 1"                         ++
           "END"

testControl2 :: Assertion
testControl2 
    = runProgram' [10, 1] p @?= 26
 where p = "LOOP 16 / 2^2 + (x1 * (x2 % 2)) - 1 DO" ++
           "  LOOP 2 DO"                            ++
           "    x0 := x0 + 1;"                      ++ 
           "    x0 := x0 - 1;"                      ++ 
           "    x0 := x0 + 1"                       ++
           "  END "                                 ++ -- space important!
           "END"

testControl3 :: Assertion
testControl3 = runProgram' [] p @?= 1
  where p = "IF 2 = 2 THEN x0 := 1 END" 

testControl4 :: Assertion
testControl4 = runProgram' [] p @?= 1
  where p = "IF 2 >= 2 THEN x0 := 1 ELSE x0 := 2 END" 

testControl5 :: Assertion
testControl5 = runProgram' [10, 1] p @?= 42 
 where p = "IF !(16 / 2^2 + (x1 * (x2 % 2)) - 1 < 8 && x1 >= x2 || 2 = 2) THEN" ++
           "  LOOP 2 DO"                                                        ++
           "    x0 := x0 + 1;"                                                  ++ 
           "    x0 := x0 - 1;"                                                  ++ 
           "    x0 := x0 + 1"                                                   ++
           "  END "                                                             ++ -- space important!
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

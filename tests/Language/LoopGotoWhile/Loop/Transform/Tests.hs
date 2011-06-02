module Language.LoopGotoWhile.Loop.Transform.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Language.LoopGotoWhile.Loop.Strict as Strict
import qualified Language.LoopGotoWhile.Loop.StrictAS as StrictAS
import           Language.LoopGotoWhile.Loop.ExtendedAS (Stat)
import           Language.LoopGotoWhile.Loop.Extended (parse)
import           Language.LoopGotoWhile.Loop.Transform (toStrict, toWhile)
import qualified Language.LoopGotoWhile.While.Extended as WhileE
import qualified Language.LoopGotoWhile.While.ExtendedAS as WhileEAS


tests :: [Test]
tests = [ testCase "loop/transform/strict/renaming1" testStrictRenaming1
        , testCase "loop/transform/strict/renaming2" testStrictRenaming2
        , testCase "loop/transform/strict/renaming3" testStrictRenaming3
        , testCase "loop/transform/strict/assignment1" testStrictAssignment1
        , testCase "loop/transform/strict/assignment2" testStrictAssignment2
        , testCase "loop/transform/strict/arithmetic1" testStrictArithmetic1
        , testCase "loop/transform/strict/arithmetic2" testStrictArithmetic2
        , testCase "loop/transform/strict/arithmetic3" testStrictArithmetic3
        , testCase "loop/transform/strict/arithmetic4" testStrictArithmetic4
        , testCase "loop/transform/strict/arithmetic5" testStrictArithmetic5
        , testCase "loop/transform/strict/arithmetic6" testStrictArithmetic6
        , testCase "loop/transform/strict/arithmetic7" testStrictArithmetic7
        , testCase "loop/transform/strict/arithmetic8" testStrictArithmetic8
        , testCase "loop/transform/strict/control1" testStrictControl1
        , testCase "loop/transform/strict/control2" testStrictControl2
        , testCase "loop/transform/strict/control3" testStrictControl3
        , testCase "loop/transform/strict/control4" testStrictControl4
        , testCase "loop/transform/strict/control5" testStrictControl5
        , testCase "loop/transform/strict/control6" testStrictControl6
        , testCase "loop/transform/strict/control7" testStrictControl7
        , testCase "loop/transform/strict/control8" testStrictControl8
        , testCase "loop/transform/strict/control9" testStrictControl9
        , testCase "loop/transform/strict/control10" testStrictControl10
        , testCase "loop/transform/strict/control11" testStrictControl11
        , testCase "loop/transform/strict/control12" testStrictControl12
        , testCase "loop/transform/strict/control13" testStrictControl13
        , testCase "loop/transform/strict/control14" testStrictControl14
        , testCase "loop/transform/strict/control15" testStrictControl15
        , testCase "loop/transform/strict/control16" testStrictControl16
        , testCase "loop/transform/strict/control17" testStrictControl17

        , testCase "loop/transform/while/control1" testToWhile1
        , testCase "loop/transform/while/control2" testToWhile2
        ]


testStrictRenaming1 :: Assertion 
testStrictRenaming1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 + 1"
        s = "x0 := x1 + 1"

testStrictRenaming2 :: Assertion 
testStrictRenaming2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := v + 1"
        s = "x0 := x1 + 1"

testStrictRenaming3 :: Assertion 
testStrictRenaming3 = toStrict (parseE e) @?= parseS s
  where e = "quux := foo  + 1;" ++
            "bar  := foo  + 2;" ++
            "x3   := quux + 0;" ++
            "LOOP quux DO"      ++
            "  x6 := bar + 0"   ++
            "END"
        s = "x1   := x2   + 1;" ++
            "x4   := x2   + 2;" ++
            "x3   := x1   + 0;" ++
            "LOOP x1 DO"        ++
            "  x6 := x4  + 0"   ++
            "END"

testStrictAssignment1 :: Assertion 
testStrictAssignment1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1"
        s = "x0 := x1 + 0"

testStrictAssignment2 :: Assertion 
testStrictAssignment2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := 42"
        s = "x0 := x1 + 42" -- x1 is unused

testStrictArithmetic1 :: Assertion 
testStrictArithmetic1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 + x2"
        s = "x0 := x1 + 0;"  ++
            "LOOP x2 DO"     ++
            "  x0 := x0 + 1" ++
            "END"            

testStrictArithmetic2 :: Assertion 
testStrictArithmetic2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 - x2"
        s = "x0 := x1 + 0;"  ++
            "LOOP x2 DO"     ++
            "  x0 := x0 - 1" ++
            "END"            

testStrictArithmetic3 :: Assertion 
testStrictArithmetic3 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 * x2"
        e2 = "x3 := 0;"         ++ -- x3 is unused
             "LOOP x2 DO"       ++
             "  x3 := x3 + x1 " ++ -- whitespace is important here
             "END;"             ++
             "x0 := x3"

testStrictArithmetic4 :: Assertion 
testStrictArithmetic4 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1^x2"
        e2 = "x3 := 1;"         ++ -- x3 is unused
             "LOOP x2 DO"       ++
             "  x3 := x3 * x1 " ++ -- whitespace is important here
             "END;"             ++
             "x0 := x3"

testStrictArithmetic5 :: Assertion 
testStrictArithmetic5 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 / x2"
        e2 = "x3 := 0;"           ++ -- x3 is unused
             "x0 := x1;"          ++
             "LOOP x0 DO"         ++
             "  IF x0 >= x2 THEN" ++ 
             "    x3 := x3 + 1;"  ++ 
             "    x0 := x0 - x2"  ++ 
             "  END "             ++ -- whitepsace is important here
             "END;"               ++
             "x0 := x3"

testStrictArithmetic6 :: Assertion 
testStrictArithmetic6 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 % x2"
        e2 = "x0 := x1;"          ++
             "LOOP x0 DO"         ++
             "  IF x0 >= x2 THEN" ++ 
             "    x0 := x0 - x2"  ++ 
             "  END "             ++ -- whitepsace is important here
             "END"                

testStrictArithmetic7 :: Assertion 
testStrictArithmetic7 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := 7 + 42"
        e2 = "x1 := 7;"      ++ -- x1 is unused 
             "x2 := 42;"     ++ -- x2 us unused
             "x0 := x1 + x2"

testStrictArithmetic8 :: Assertion 
testStrictArithmetic8 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := (x1 + 3) - x2"
        e2 = "x3 := x1 + 3;" ++ -- x3 is unused
             "x4 := x2;"     ++ -- x4 is unused
             "x0 := x3 - x4" 

testStrictControl1 :: Assertion 
testStrictControl1 = toStrict (parseE e) @?= parseS s
  where e = "LOOP 1 DO"      ++
            "  x0 := x0 + 0" ++
            "END"
        s = "x1 := x2 + 1;"  ++ -- x1, x2 is unused
            "LOOP x1 DO"     ++
            "  x0 := x0 + 0" ++
            "END"

testStrictControl2 :: Assertion 
testStrictControl2 = toStrict (parseE e) @?= parseS s
  where e = "LOOP x1 + 1 DO" ++
            "  x0 := x0 + 0" ++
            "END"
        s = "x2 := x1 + 1;"  ++ -- x2 is unused
            "LOOP x2 DO"     ++
            "  x0 := x0 + 0" ++ 
            "END"

testStrictControl3 :: Assertion 
testStrictControl3 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 = 0 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "x1 := (x0 - 0) + (0 - x0);"  ++ -- x1 is unused
             "x2 := 1;"                    ++ -- x2 is unused
             "LOOP x1 DO"                  ++
             "  x2 := 0"                   ++ 
             "END;"                        ++
             "LOOP x2 DO"                  ++
             "  x0 := x0 + 0"              ++
             "END"

testStrictControl4 :: Assertion 
testStrictControl4 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 + 8 = x0 - x0 THEN" ++ 
             "  x0 := x0 + 0"           ++
             "ELSE"                     ++
             "  x1 := x1 + 0"           ++
             "END"
        e2 = "x2 := ((x0 + 8) - (x0 - x0)) +" ++ -- x2 is unused
             "      ((x0 - x0) - (x0 + 8));"  ++
             "x3 := 1;"                       ++ -- x3 is unused
             "x4 := 0;"                       ++ -- x4 is unused
             "LOOP x2 DO"                     ++
             "  x3 := 0;"                     ++
             "  x4 := 1"                      ++ 
             "END;"                           ++                          
             "LOOP x3 DO"                     ++
             "  x0 := x0 + 0"                 ++
             "END;"                           ++
             "LOOP x4 DO"                     ++
             "  x1 := x1 + 0"                 ++
             "END"

testStrictControl5 :: Assertion 
testStrictControl5 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 != 0 THEN" ++
             "  x0 := x0 + 0"  ++
             "END"
        e2 = "x1 := (x0 - 0) + (0 - x0);"  ++ -- x1 is unused
             "x2 := 0;"                    ++ -- x2 is unused
             "LOOP x1 DO"                  ++
             "  x2 := 1"                   ++ 
             "END;"                        ++
             "LOOP x2 DO"                  ++
             "  x0 := x0 + 0"              ++
             "END"

testStrictControl6 :: Assertion 
testStrictControl6 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 + 8 != x0 - x0 THEN" ++ 
             "  x0 := x0 + 0"            ++
             "ELSE"                      ++
             "  x1 := x1 + 0"            ++
             "END"
        e2 = "x2 := ((x0 + 8) - (x0 - x0)) +" ++ -- x2 is unused
             "      ((x0 - x0) - (x0 + 8));"  ++
             "x3 := 0;"                       ++ -- x3 is unused
             "x4 := 1;"                       ++ -- x4 is unused
             "LOOP x2 DO"                     ++
             "  x3 := 1;"                     ++ 
             "  x4 := 0"                      ++ 
             "END;"                           ++                          
             "LOOP x3 DO"                     ++
             "  x0 := x0 + 0"                 ++
             "END;"                           ++
             "LOOP x4 DO"                     ++
             "  x1 := x1 + 0"                 ++
             "END"

testStrictControl7 :: Assertion 
testStrictControl7 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 < 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "x1 := 5 - x0;"  ++ -- x1 is unused
             "x2 := 0;"       ++ -- x2 is unused
             "LOOP x1 DO"     ++
             "  x2 := 1"      ++ 
             "END;"           ++
             "LOOP x2 DO"     ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl8 :: Assertion 
testStrictControl8 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 + 8 < x0 - x0 THEN" ++ 
             "  x0 := x0 + 0"           ++
             "ELSE"                     ++
             "  x1 := x1 + 0"           ++
             "END"
        e2 = "x2 := (x0 - x0) - (x0 + 8);" ++ -- x2 is unused
             "x3 := 0;"                    ++ -- x3 is unused
             "x4 := 1;"                    ++ -- x4 is unused
             "LOOP x2 DO"                  ++
             "  x3 := 1;"                  ++
             "  x4 := 0"                   ++ 
             "END;"                        ++                          
             "LOOP x3 DO"                  ++
             "  x0 := x0 + 0"              ++
             "END;"                        ++
             "LOOP x4 DO"                  ++
             "  x1 := x1 + 0"              ++
             "END"

testStrictControl9 :: Assertion 
testStrictControl9 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "IF 5 < x0 THEN" ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl10 :: Assertion 
testStrictControl10 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 && 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "END"
        e2 = "IF x0 > 5 THEN"   ++
             "  IF 5 < 6 THEN"  ++
             "    x0 := x0 + 0" ++
             "  END "           ++ -- the space at the end is important!
             "END"

testStrictControl11 :: Assertion 
testStrictControl11 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 && 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "ELSE"                    ++
             "  x1 := x1 + 0"          ++
             "END"
        e2 = "x2 := 1;"         ++ -- x2 is unused
             "IF x0 > 5 THEN"   ++
             "  IF 5 < 6 THEN"  ++
             "    x2 := 0;"     ++
             "    x0 := x0 + 0" ++
             "  END "           ++
             "END;"             ++
             "LOOP x2 DO"       ++
             "  x1 := x1 + 0"   ++
             "END"

testStrictControl12 :: Assertion 
testStrictControl12 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 || 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "END"
        e2 = "x1 := 0;"       ++ -- x1 is unused
             "IF x0 > 5 THEN" ++
             "  x1 := 1"      ++ 
             "END;"           ++
             "IF 5 < 6 THEN"  ++
             "  x1 := 1"      ++
             "END;"           ++
             "LOOP x1 DO"     ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl13 :: Assertion 
testStrictControl13 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 || 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "ELSE"                    ++
             "  x1 := x1 + 0"          ++
             "END"
        e2 = "x2 := 0;"       ++ -- x2 is unused
             "x3 := 1;"       ++ -- x3 is unused
             "IF x0 > 5 THEN" ++
             "  x2 := 1;"     ++
             "  x3 := 0"      ++
             "END;"           ++
             "IF 5 < 6 THEN"  ++
             "  x2 := 1;"     ++
             "  x3 := 0"      ++
             "END;"           ++
             "LOOP x2 DO"     ++
             "  x0 := x0 + 0" ++
             "END;"           ++
             "LOOP x3 DO"     ++
             "  x1 := x1 + 0" ++
             "END"

testStrictControl14 :: Assertion 
testStrictControl14 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 >= 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "IF x0 > 5 || x0 = 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl15 :: Assertion 
testStrictControl15 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF !(x0 = 5) THEN" ++
             "  x0 := x0 + 0"    ++
             "END"
        e2 = "IF x0 != 5 THEN" ++
             "  x0 := x0 + 0"  ++ 
             "END"             

testStrictControl16 :: Assertion 
testStrictControl16 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF !(!(!(!(x0 = 5)))) THEN" ++
             "  x0 := x0 + 0"             ++
             "END"
        e2 = "IF x0 = 5 THEN" ++
             "  x0 := x0 + 0" ++ 
             "END"             

testStrictControl17 :: Assertion 
testStrictControl17 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF !(x0 = 5 && !(!(x1 < 6 || x0 > 6))) THEN" ++
             "  x0 := x0 + 0"                              ++
             "ELSE"                                        ++
             "  x1 := x1 + 0"                              ++
             "END"
        e2 = "IF x0 != 5 || (x1 >= 6 && x0 <= 6) THEN" ++
             "  x0 := x0 + 0"                          ++ 
             "ELSE"                                    ++
             "  x1 := x1 + 0"                          ++
             "END"                                   

testToWhile1 :: Assertion
testToWhile1 = toWhile (parseE e1) @?= parseWhileE e2
  where e1 = "LOOP x1 DO"     ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "x2 := x1;"        ++
             "WHILE x2 != 0 DO" ++
             "  x2 := x2 - 1;"  ++
             "  x0 := x0 + 0"   ++
             "END"

testToWhile2 :: Assertion
testToWhile2 = toWhile (parseE e1) @?= parseWhileE e2
  where e1 = "LOOP x1 + 1 DO" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "x2 := x1 + 1;"        ++
             "WHILE x2 != 0 DO" ++
             "  x2 := x2 - 1;"  ++
             "  x0 := x0 + 0"   ++
             "END"


-- Helper

-- | Parse a string representation of an extended Loop program and return the AST.
parseE :: String -> Stat
parseE code = case parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of a strict Loop program and return the AST.
parseS :: String -> StrictAS.Stat
parseS code = case Strict.parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of an extended While program and return the AST.
parseWhileE :: String -> WhileEAS.Stat
parseWhileE code = case WhileE.parse code of
    Left  err -> error err
    Right ast -> ast

-- |
--
-- Note: Some of the tests don't use a hand-written strict program to test the
-- transformations against but instead they test against a simplified extended
-- program that is automatically transformed to a strict program (e.g. most of
-- the control tests). The reason for this decision is that a strict program
-- would simply be too long and too convoluted to comprehend the applied
-- transformations. Nonetheless these tests are not useless as the more
-- primitive tests "ensure" that the transformations of the simplified extended
-- version to a strict version will be correct.
module Language.LoopGotoWhile.Loop.Transform.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import           Language.LoopGotoWhile.Util
import qualified Language.LoopGotoWhile.Loop.Strict as Strict
import           Language.LoopGotoWhile.Loop.ExtendedADT (Stat)
import           Language.LoopGotoWhile.Loop.Extended (parser)
import           Language.LoopGotoWhile.Loop.Transform (toStrict)

tests :: [Test]
tests = [ testCase "loop/transform/extended-to-strict/renaming1" testStrictRenaming1
        , testCase "loop/transform/extended-to-strict/renaming2" testStrictRenaming2
        , testCase "loop/transform/extended-to-strict/renaming3" testStrictRenaming3
        , testCase "loop/transform/extended-to-strict/assignment1" testStrictAssignment1
        , testCase "loop/transform/extended-to-strict/assignment2" testStrictAssignment2
        , testCase "loop/transform/extended-to-strict/arithmetic1" testStrictArithmetic1
        , testCase "loop/transform/extended-to-strict/arithmetic2" testStrictArithmetic2
        , testCase "loop/transform/extended-to-strict/arithmetic3" testStrictArithmetic3
        , testCase "loop/transform/extended-to-strict/arithmetic4" testStrictArithmetic4
        , testCase "loop/transform/extended-to-strict/arithmetic5" testStrictArithmetic5
        , testCase "loop/transform/extended-to-strict/arithmetic6" testStrictArithmetic6
        , testCase "loop/transform/extended-to-strict/arithmetic7" testStrictArithmetic7
        , testCase "loop/transform/extended-to-strict/arithmetic8" testStrictArithmetic8
        , testCase "loop/transform/extended-to-strict/control1" testStrictControl1
        , testCase "loop/transform/extended-to-strict/control2" testStrictControl2
        , testCase "loop/transform/extended-to-strict/control3" testStrictControl3
        , testCase "loop/transform/extended-to-strict/control4" testStrictControl4
        , testCase "loop/transform/extended-to-strict/control5" testStrictControl5
        , testCase "loop/transform/extended-to-strict/control6" testStrictControl6
        , testCase "loop/transform/extended-to-strict/control7" testStrictControl7
        , testCase "loop/transform/extended-to-strict/control8" testStrictControl8
        , testCase "loop/transform/extended-to-strict/control9" testStrictControl9
        , testCase "loop/transform/extended-to-strict/control10" testStrictControl10
        , testCase "loop/transform/extended-to-strict/control11" testStrictControl11
        , testCase "loop/transform/extended-to-strict/control12" testStrictControl12
        , testCase "loop/transform/extended-to-strict/control13" testStrictControl13
        , testCase "loop/transform/extended-to-strict/control14" testStrictControl14
        , testCase "loop/transform/extended-to-strict/control15" testStrictControl15
        , testCase "loop/transform/extended-to-strict/control16" testStrictControl16
        , testCase "loop/transform/extended-to-strict/control17" testStrictControl17
        ]

testStrictRenaming1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 + 1"
        s = "x0 := x1 + 1"

testStrictRenaming2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := v + 1"
        s = "x0 := x1 + 1"

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

testStrictAssignment1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1"
        s = "x0 := x1 + 0"

testStrictAssignment2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := 42"
        s = "x0 := x1 + 42" -- x1 is unused

testStrictArithmetic1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 + x2"
        s = "x0 := x1 + 0;"  ++
            "LOOP x2 DO"     ++
            "  x0 := x0 + 1" ++
            "END"            

testStrictArithmetic2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 - x2"
        s = "x0 := x1 + 0;"  ++
            "LOOP x2 DO"     ++
            "  x0 := x0 - 1" ++
            "END"            

testStrictArithmetic3 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 * x2"
        e2 = "x3 := 0;"         ++ -- x3 is unused
             "LOOP x2 DO"       ++
             "  x3 := x3 + x1 " ++ -- whitespace is important here
             "END;"             ++
             "x0 := x3"

testStrictArithmetic4 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1^x2"
        e2 = "x3 := 1;"         ++ -- x3 is unused
             "LOOP x2 DO"       ++
             "  x3 := x3 * x1 " ++ -- whitespace is important here
             "END;"             ++
             "x0 := x3"

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

testStrictArithmetic6 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 % x2"
        e2 = "x0 := x1;"          ++
             "LOOP x0 DO"         ++
             "  IF x0 >= x2 THEN" ++ 
             "    x0 := x0 - x2"  ++ 
             "  END "             ++ -- whitepsace is important here
             "END"                

testStrictArithmetic7 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := 7 + 42"
        e2 = "x1 := 7;"      ++ -- x1 is unused 
             "x2 := 42;"     ++ -- x2 us unused
             "x0 := x1 + x2"

testStrictArithmetic8 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := (x1 + 3) - x2"
        e2 = "x3 := x1 + 3;" ++ -- x3 is unused
             "x4 := x2;"     ++ -- x4 is unused
             "x0 := x3 - x4" 

testStrictControl1 = toStrict (parseE e) @?= parseS s
  where e = "LOOP 1 DO"      ++
            "  x0 := x0 + 0" ++
            "END"
        s = "x1 := x2 + 1;"  ++ -- x1, x2 is unused
            "LOOP x1 DO"     ++
            "  x0 := x0 + 0" ++
            "END"

testStrictControl2 = toStrict (parseE e) @?= parseS s
  where e = "LOOP x1 + 1 DO" ++
            "  x0 := x0 + 0" ++
            "END"
        s = "x2 := x1 + 1;"  ++ -- x2 is unused
            "LOOP x2 DO"     ++
            "  x0 := x0 + 0" ++ 
            "END"

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

testStrictControl9 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "IF 5 < x0 THEN" ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl10 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 && 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "END"
        e2 = "IF x0 > 5 THEN"   ++
             "  IF 5 < 6 THEN"  ++
             "    x0 := x0 + 0" ++
             "  END "           ++ -- the space at the end is important!
             "END"

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

testStrictControl14 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 >= 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"
        e2 = "IF x0 > 5 || x0 = 5 THEN" ++
             "  x0 := x0 + 0" ++
             "END"

testStrictControl15 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF !(x0 = 5) THEN" ++
             "  x0 := x0 + 0"    ++
             "END"
        e2 = "IF x0 != 5 THEN" ++
             "  x0 := x0 + 0"  ++ 
             "END"             

testStrictControl16 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF !(!(!(!(x0 = 5)))) THEN" ++
             "  x0 := x0 + 0"             ++
             "END"
        e2 = "IF x0 = 5 THEN" ++
             "  x0 := x0 + 0" ++ 
             "END"             

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

-- TODO: test functions

-- Helper

-- | Parse a string representation of an extended Loop program and return the AST.
parseE :: String -> Stat
parseE code = case parser code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of a strict Loop program and return the AST.
parseS :: String -> Strict.Statement
parseS code = case Strict.parser code of
    Left  err -> error err
    Right ast -> ast

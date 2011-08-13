module Language.LoopGotoWhile.Goto.Transform.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Language.LoopGotoWhile.Goto.Strict as Strict
import qualified Language.LoopGotoWhile.Goto.StrictAS as StrictAS
import           Language.LoopGotoWhile.Goto.ExtendedAS (Stat)
import           Language.LoopGotoWhile.Goto.Extended (parse)
import           Language.LoopGotoWhile.Goto.Transform (toStrict, toWhile)
import qualified Language.LoopGotoWhile.While.Extended as While
import qualified Language.LoopGotoWhile.While.ExtendedAS as WhileAS


tests :: [Test]
tests = [ testCase "goto/transform/strict/renaming1" testStrictRenaming1
        , testCase "goto/transform/strict/renaming2" testStrictRenaming2
        , testCase "goto/transform/strict/renaming3" testStrictRenaming3
        , testCase "goto/transform/strict/renaming4" testStrictRenaming4
        , testCase "goto/transform/strict/renaming5" testStrictRenaming5
        , testCase "goto/transform/strict/assignment1" testStrictAssignment1
        , testCase "goto/transform/strict/assignment2" testStrictAssignment2
        , testCase "goto/transform/strict/arithmetic1" testStrictArithmetic1
        {-, testCase "goto/transform/strict/arithmetic2" testStrictArithmetic2-}
        {-, testCase "goto/transform/strict/arithmetic3" testStrictArithmetic3-}
        {-, testCase "goto/transform/strict/arithmetic4" testStrictArithmetic4-}
        {-, testCase "goto/transform/strict/arithmetic5" testStrictArithmetic5-}
        {-, testCase "goto/transform/strict/arithmetic6" testStrictArithmetic6-}
        , testCase "goto/transform/strict/arithmetic7" testStrictArithmetic7
        , testCase "goto/transform/strict/arithmetic8" testStrictArithmetic8
        {-, testCase "goto/transform/strict/control1" testStrictControl1-}
        {-, testCase "goto/transform/strict/control2" testStrictControl2-}
        {-, testCase "goto/transform/strict/control3" testStrictControl3-}
        {-, testCase "goto/transform/strict/control4" testStrictControl4-}
        {-, testCase "goto/transform/strict/control5" testStrictControl5-}
        {-, testCase "goto/transform/strict/control6" testStrictControl6-}
        {-, testCase "goto/transform/strict/control7" testStrictControl7-}
        {-, testCase "goto/transform/strict/control8" testStrictControl8-}
        , testCase "goto/transform/strict/control9" testStrictControl9
        , testCase "goto/transform/strict/control10" testStrictControl10
        {-, testCase "goto/transform/strict/control11" testStrictControl11-}
        {-, testCase "goto/transform/strict/control12" testStrictControl12-}
        {-, testCase "goto/transform/strict/control13" testStrictControl13-}
        , testCase "goto/transform/strict/control14" testStrictControl14
        , testCase "goto/transform/strict/control15" testStrictControl15
        , testCase "goto/transform/strict/control16" testStrictControl16
        , testCase "goto/transform/strict/control17" testStrictControl17

        , testCase "goto/transform/while/towhile1" testToWhile1
        ]

-- TODO: If I could somehow reduce the code duplication by using common code
-- for all three languages to transform them to their strict subset, I would
-- only need to test the transformation of a "for loop" to a Goto loop and the
-- transformation of some Goto specific things (e.g. a HALT in the body of an
-- if) since the other transformations would already be covered by the Loop
-- language tests.

testStrictRenaming1 :: Assertion
testStrictRenaming1 = toStrict (parseE e) @?= parseS s
  where e = "M1: x0 := x1 + 1"
        s = "M1: x0 := x1 + 1; M2: HALT"

testStrictRenaming2 :: Assertion
testStrictRenaming2 = toStrict (parseE e) @?= parseS s
  where e = "M1: x0 := v + 1"
        s = "M1: x0 := x1 + 1; M2: HALT"

-- GOTOs with an undefined label are simply transformed to M1.
testStrictRenaming3 :: Assertion
testStrictRenaming3 = toStrict (parseE e) @?= parseS s
  where e = "M1: GOTO timbuktu"
        s = "M1: GOTO M1"

testStrictRenaming4 :: Assertion
testStrictRenaming4 = toStrict (parseE e) @?= parseS s
  where e = "      quux := foo  + 1;"  ++
            "M3:   bar  := foo  + 2;"  ++
            "      x3   := quux + 0;"  ++
            "ij8h: quux := quux + 1;"  ++
            "      x6   := bar  + 0;"  ++
            "GOTO ij8h"
        s = "M1:   x1   := x2   + 1;" ++
            "M2:   x4   := x2   + 2;" ++
            "M3:   x3   := x1   + 0;" ++
            "M4:   x1   := x1   + 1;" ++
            "M5:   x6   := x4   + 0;" ++
            "M6: GOTO M4"

-- This is a correct transformation of "x0 := x1 * x2". I encountered a
-- logical error of mine for the label renaming and fixed the mistake. This is
-- therefore a regression test. 
-- The problem arises from the "clever" use of labels. The Label "M4" is
-- actually on 5th position, which should lead to changing all "M4"s in the
-- code to "M5". Then, on 6th position, there is the label "M5". Therefore, all
-- "M5"s in the code should be changed to "M6"s. But wait! That would change
-- the semantics of the program! The GOTO instruction on line 2 would not
-- anymore "go to" line 5 but instead to line 6! Hence, already changed lines
-- must be marked in order to prevent this problem from happening.
testStrictRenaming5 :: Assertion
testStrictRenaming5 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 + 3;"                   ++
            "Nt: IF x0 = 5 THEN GOTO M4 END;" ++ 
            "M3: x0 := x2 + 3;"               ++
            "Mx: GOTO M5;"                    ++
            "M4: x0 := x3 + 3;"               ++
            "M5: x0 := x4 + 3" 
        s = "M1: x0 := x1 + 3;"           ++
            "M2: IF x0 = 5 THEN GOTO M5;" ++ 
            "M3: x0 := x2 + 3;"           ++
            "M4: GOTO M6;"                ++
            "M5: x0 := x3 + 3;"           ++
            "M6: x0 := x4 + 3;"           ++
            "M7: HALT"

testStrictAssignment1 :: Assertion
testStrictAssignment1 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1"
        s = "M1: x0 := x1 + 0; M2: HALT"

testStrictAssignment2 :: Assertion
testStrictAssignment2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := 42"
        s = "M1: x0 := x1 + 42; M2: HALT" -- x1 is unused

-- Using the label "M1" for e is important because I found a bug in my code
-- this way. Therefore, this is a regression test.
testStrictArithmetic1 = toStrict (parseE e) @?= parseS s
  where e = "M1: x0 := x1 + x2" 
        s = "M1: x0 := x1 + 0;"           ++
            "M2: x3 := x2 + 0;"           ++ -- x3 is counter
            "M3: IF x3 = 0 THEN GOTO M7;" ++
            "M4: x3 := x3 - 1;"           ++
            "M5: x0 := x0 + 1;"           ++
            "M6: GOTO M3;"                ++
            "M7: HALT"

{-testStrictArithmetic2 = toStrict (parseE e) @?= parseS s-}
  {-where e = "x0 := x1 - x2"-}
        {-s = "x0 := x1 + 0;"    ++-}
            {-"x3 := x2 + 0;"    ++ -- x3 is counter-}
            {-"WHILE x3 != 0 DO" ++-}
            {-"  x3 := x3 - 1;"  ++-}
            {-"  x0 := x0 - 1"   ++-}
            {-"END"            -}

{-testStrictArithmetic3 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "x0 := x1 * x2"-}
        {-e2 = "x3 := 0;"         ++ -- x3 is unused-}
             {-"x4 := x2;"        ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO" ++-}
             {-"  x4 := x4 - 1;"  ++-}
             {-"  x3 := x3 + x1 " ++ -- whitespace is important here-}
             {-"END;"             ++-}
             {-"x0 := x3"-}

{-testStrictArithmetic4 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "x0 := x1^x2"-}
        {-e2 = "x3 := 1;"         ++ -- x3 is unused-}
             {-"x4 := x2;"        ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO" ++-}
             {-"  x4 := x4 - 1;"  ++-}
             {-"  x3 := x3 * x1 " ++ -- whitespace is important here-}
             {-"END;"             ++-}
             {-"x0 := x3"-}

{-testStrictArithmetic5 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "x0 := x1 / x2"-}
        {-e2 = "x3 := 0;"           ++ -- x3 is unused-}
             {-"x0 := x1;"          ++-}
             {-"x4 := x0;"          ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO"   ++-}
             {-"  x4 := x4 - 1;"    ++-}
             {-"  IF x0 >= x2 THEN" ++ -}
             {-"    x3 := x3 + 1;"  ++ -}
             {-"    x0 := x0 - x2"  ++ -}
             {-"  END "             ++ -- whitepsace is important here-}
             {-"END;"               ++-}
             {-"x0 := x3"-}

{-testStrictArithmetic6 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "x0 := x1 % x2"-}
        {-e2 = "x0 := x1;"          ++-}
             {-"x3 := x0;"          ++ -- x3 is counter-}
             {-"WHILE x3 != 0 DO"   ++-}
             {-"  x3 := x3 - 1;"    ++-}
             {-"  IF x0 >= x2 THEN" ++ -}
             {-"    x0 := x0 - x2"  ++ -}
             {-"  END "             ++ -- whitepsace is important here-}
             {-"END"                -}

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

{-testStrictControl1 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "WHILE 1 != 0 DO" ++-}
             {-"  x0 := x0 + 0"  ++-}
             {-"END"-}
        {-e2 = "IF 1 != 0 THEN"   ++-}
             {-"  x1 := 1"        ++-}
             {-"ELSE"             ++-}
             {-"  x1 := 0"        ++-}
             {-"END;"             ++-}
             {-"WHILE x1 != 0 DO" ++-}
             {-"  x0 := x0 + 0;"  ++-}
             {-"  IF 1 != 0 THEN" ++-}
             {-"    x1 := 1"      ++-}
             {-"  ELSE"           ++-}
             {-"    x1 := 0"      ++-}
             {-"  END "           ++  -}
             {-"END"              -}

{-testStrictControl1 = toStrict (parseE e) @?= parseS s-}
  {-where e = "WHILE 1 != 0 DO" ++-}
            {-"  x0 := x0 + 0"  ++-}
            {-"END"-}
        {-s = "x1 := x2 + 1;"    ++ -- x1, x2 is unused-}
            {-"WHILE x1 != 0 DO" ++-}
            {-"  x0 := x0 + 0"   ++-}
            {-"END"-}

{-testStrictControl2 = toStrict (parseE e) @?= parseS s-}
  {-where e = "WHILE x1 + 1 != 0 DO" ++-}
            {-"  x0 := x0 + 0"       ++-}
            {-"END"-}
        {-s = "x2 := x1 + 1;"  ++ -- x2 is unused-}
            {-"WHILE x2 != 0 DO"     ++-}
            {-"  x0 := x0 + 0" ++ -}
            {-"END"-}

{-testStrictControl3 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 = 0 THEN" ++-}
             {-"  x0 := x0 + 0" ++-}
             {-"END"-}
        {-e2 = "x1 := (x0 - 0) + (0 - x0);"  ++ -- x1 is unused-}
             {-"x2 := 1;"                    ++ -- x2 is unused-}
             {-"x3 := x1;"                   ++ -- x3 is counter-}
             {-"WHILE x3 != 0 DO"            ++-}
             {-"  x2 := 0"                   ++ -}
             {-"END;"                        ++-}
             {-"x4 := x2;"                   ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO"            ++-}
             {-"  x4 := x4 - 1;"             ++-}
             {-"  x0 := x0 + 0"              ++-}
             {-"END"-}

{-testStrictControl4 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 + 8 = x0 - x0 THEN" ++ -}
             {-"  x0 := x0 + 0"           ++-}
             {-"ELSE"                     ++-}
             {-"  x1 := x1 + 0"           ++-}
             {-"END"-}
        {-e2 = "x2 := ((x0 + 8) - (x0 - x0)) +" ++ -- x2 is unused-}
             {-"      ((x0 - x0) - (x0 + 8));"  ++-}
             {-"x3 := 1;"                       ++ -- x3 is unused-}
             {-"x4 := 0;"                       ++ -- x4 is unused-}
             {-"x5 := x2;"                      ++ -- x5 is counter-}
             {-"WHILE x5 != 0 DO"               ++-}
             {-"  x5 := x5 - 1;"                ++-}
             {-"  x3 := 0;"                     ++-}
             {-"  x4 := 1"                      ++ -}
             {-"END;"                           ++                          -}
             {-"x6 := x3;"                      ++ -- x6 is counter-}
             {-"WHILE x6 != 0 DO"               ++-}
             {-"  x6 := x6 - 1;"                ++-}
             {-"  x0 := x0 + 0"                 ++-}
             {-"END;"                           ++-}
             {-"x7 := x4;"                      ++ -- x7 is counter-}
             {-"WHILE x7 != 0 DO"               ++-}
             {-"  x7 := x7 - 1;"                ++-}
             {-"  x1 := x1 + 0"                 ++-}
             {-"END"-}

{-testStrictControl5 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 != 0 THEN" ++-}
             {-"  x0 := x0 + 0"  ++-}
             {-"END"-}
        {-e2 = "x1 := (x0 - 0) + (0 - x0);"  ++ -- x1 is unused-}
             {-"x2 := 0;"                    ++ -- x2 is unused-}
             {-"x3 := x1;"                   ++ -- x3 is counter-}
             {-"WHILE x3 != 0 DO"            ++-}
             {-"  x3 := x3 - 1;"             ++-}
             {-"  x2 := 1"                   ++ -}
             {-"END;"                        ++-}
             {-"x4 := x2;"                   ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO"            ++-}
             {-"  x4 := x4 - 1;"             ++-}
             {-"  x0 := x0 + 0"              ++-}
             {-"END"-}

{-testStrictControl6 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 + 8 != x0 - x0 THEN" ++ -}
             {-"  x0 := x0 + 0"            ++-}
             {-"ELSE"                      ++-}
             {-"  x1 := x1 + 0"            ++-}
             {-"END"-}
        {-e2 = "x2 := ((x0 + 8) - (x0 - x0)) +" ++ -- x2 is unused-}
             {-"      ((x0 - x0) - (x0 + 8));"  ++-}
             {-"x3 := 0;"                       ++ -- x3 is unused-}
             {-"x4 := 1;"                       ++ -- x4 is unused-}
             {-"x5 := x2;"                      ++ -- x5 is counter-}
             {-"WHILE x5 != 0 DO"               ++-}
             {-"  x5 := x5 - 1;"                ++-}
             {-"  x3 := 1;"                     ++ -}
             {-"  x4 := 0"                      ++ -}
             {-"END;"                           ++                          -}
             {-"x6 := x3;"                      ++ -- x6 is counter-}
             {-"WHILE x6 != 0 DO"               ++-}
             {-"  x6 := x6 - 1;"                ++-}
             {-"  x0 := x0 + 0"                 ++-}
             {-"END;"                           ++-}
             {-"x7 := x4;"                      ++ -- x7 is counter-}
             {-"WHILE x7 != 0 DO"               ++-}
             {-"  x7 := x7 - 1;"                ++-}
             {-"  x1 := x1 + 0"                 ++-}
             {-"END"-}

{-testStrictControl7 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 < 5 THEN" ++-}
             {-"  x0 := x0 + 0" ++-}
             {-"END"-}
        {-e2 = "x1 := 5 - x0;"    ++ -- x1 is unused-}
             {-"x2 := 0;"         ++ -- x2 is unused-}
             {-"x3 := x1;"        ++ -- x3 is counter-}
             {-"WHILE x3 != 0 DO" ++-}
             {-"  x3 := x3 - 1;"  ++-}
             {-"  x2 := 1"        ++ -}
             {-"END;"             ++-}
             {-"x4 := x2;"        ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO" ++-}
             {-"  x4 := x4 - 1;"  ++-}
             {-"  x0 := x0 + 0"   ++-}
             {-"END"-}

{-testStrictControl8 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 + 8 < x0 - x0 THEN" ++ -}
             {-"  x0 := x0 + 0"           ++-}
             {-"ELSE"                     ++-}
             {-"  x1 := x1 + 0"           ++-}
             {-"END"-}
        {-e2 = "x2 := (x0 - x0) - (x0 + 8);" ++ -- x2 is unused-}
             {-"x3 := 0;"                    ++ -- x3 is unused-}
             {-"x4 := 1;"                    ++ -- x4 is unused-}
             {-"x5 := x2;"                   ++ -- x5 is counter-}
             {-"WHILE x5 != 0 DO"            ++-}
             {-"  x5 := x5 - 1;"             ++-}
             {-"  x3 := 1;"                  ++-}
             {-"  x4 := 0"                   ++ -}
             {-"END;"                        ++                          -}
             {-"x6 := x3;"                   ++ -- x6 is counter-}
             {-"WHILE x6 != 0 DO"            ++-}
             {-"  x6 := x6 - 1;"             ++-}
             {-"  x0 := x0 + 0"              ++-}
             {-"END;"                        ++-}
             {-"x7 := x4;"                   ++ -- x7 is counter-}
             {-"WHILE x7 != 0 DO"            ++-}
             {-"  x7 := x7 - 1;"             ++-}
             {-"  x1 := x1 + 0"              ++-}
             {-"END"-}

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

{-testStrictControl11 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 > 5 && 5 < 6 THEN" ++-}
             {-"  x0 := x0 + 0"          ++-}
             {-"ELSE"                    ++-}
             {-"  x1 := x1 + 0"          ++-}
             {-"END"-}
        {-e2 = "x2 := 1;"         ++ -- x2 is unused-}
             {-"IF x0 > 5 THEN"   ++-}
             {-"  IF 5 < 6 THEN"  ++-}
             {-"    x2 := 0;"     ++-}
             {-"    x0 := x0 + 0" ++-}
             {-"  END "           ++-}
             {-"END;"             ++-}
             {-"x3 := x2;"        ++ -- x3 is counter-}
             {-"WHILE x3 != 0 DO" ++-}
             {-"  x3 := x3 - 1;"  ++-}
             {-"  x1 := x1 + 0"   ++-}
             {-"END"-}

{-testStrictControl12 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 > 5 || 5 < 6 THEN" ++-}
             {-"  x0 := x0 + 0"          ++-}
             {-"END"-}
        {-e2 = "x1 := 0;"         ++ -- x1 is unused-}
             {-"IF x0 > 5 THEN"   ++-}
             {-"  x1 := 1"        ++ -}
             {-"END;"             ++-}
             {-"IF 5 < 6 THEN"    ++-}
             {-"  x1 := 1"        ++-}
             {-"END;"             ++-}
             {-"x2 := x1;"        ++ -- x2 is counter-}
             {-"WHILE x2 != 0 DO" ++-}
             {-"  x2 := x2 - 1;"  ++-}
             {-"  x0 := x0 + 0"   ++-}
             {-"END"-}

{-testStrictControl13 = toStrict (parseE e1) @?= toStrict (parseE e2)-}
  {-where e1 = "IF x0 > 5 || 5 < 6 THEN" ++-}
             {-"  x0 := x0 + 0"          ++-}
             {-"ELSE"                    ++-}
             {-"  x1 := x1 + 0"          ++-}
             {-"END"-}
        {-e2 = "x2 := 0;"         ++ -- x2 is unused-}
             {-"x3 := 1;"         ++ -- x3 is unused-}
             {-"IF x0 > 5 THEN"   ++-}
             {-"  x2 := 1;"       ++-}
             {-"  x3 := 0"        ++-}
             {-"END;"             ++-}
             {-"IF 5 < 6 THEN"    ++-}
             {-"  x2 := 1;"       ++-}
             {-"  x3 := 0"        ++-}
             {-"END;"             ++-}
             {-"x4 := x2;"        ++ -- x4 is counter-}
             {-"WHILE x4 != 0 DO" ++-}
             {-"  x4 := x4 - 1;"  ++-}
             {-"  x0 := x0 + 0"   ++-}
             {-"END;"             ++-}
             {-"x5 := x3;"        ++ -- x5 is counter-}
             {-"WHILE x5 != 0 DO" ++-}
             {-"  x5 := x5 - 1;"  ++-}
             {-"  x1 := x1 + 0"   ++-}
             {-"END"-}

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


-- In reality the extended Goto program is transformed to a strict goto program
-- and then this strict program is transformed to a While program. Otherwise,
-- it would be too tricky to transform Goto to While. That's why I don't use
-- any extended Goto feature in this test (again to keep it simple).
testToWhile1 :: Assertion
testToWhile1 = toWhile (parseE e1) @?= parseWhile e2
  where e1 = "M1: x0 := x0 + 1;" ++
             "M2: GOTO M3;"      ++
             "M3: IF x0 = 0 THEN GOTO M4 END;" ++
             "M4: HALT"
        e2 = "x1 := 1;"        ++
             "WHILE x1 != 0 DO" ++
             "  IF x1 = 1 THEN x0 := x0 + 1; x1 := x1 + 1 END;"  ++
             "  IF x1 = 2 THEN x1 := 3 END;"  ++
             "  IF x1 = 3 THEN IF x0 = 0 THEN x1 := 4 ELSE x1 := x1 + 1 END END;" ++
             "  IF x1 = 4 THEN x1 := 0 END "  ++
             "END"


-- Helper

-- | Parse a string representation of an extended Goto program and return the AST.
parseE :: String -> Stat
parseE code = case parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of a strict Goto program and return the AST.
parseS :: String -> StrictAS.Stat
parseS code = case Strict.parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of an extended While program and return the AST.
parseWhile :: String -> WhileAS.Stat
parseWhile code = case While.parse code of
    Left  err -> error err
    Right ast -> ast

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

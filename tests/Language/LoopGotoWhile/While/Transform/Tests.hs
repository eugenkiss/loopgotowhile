module Language.LoopGotoWhile.While.Transform.Tests (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Language.LoopGotoWhile.While.Strict as Strict
import qualified Language.LoopGotoWhile.While.StrictAS as StrictAS
import           Language.LoopGotoWhile.While.ExtendedAS (Stat)
import           Language.LoopGotoWhile.While.Extended (parse)
import           Language.LoopGotoWhile.While.Transform (toStrict, toGoto)
import qualified Language.LoopGotoWhile.Goto.Extended as Goto
import qualified Language.LoopGotoWhile.Goto.ExtendedAS as GotoAS


tests :: [Test]
tests = [ testCase "while/transform/strict/renaming1" testStrictRenaming1
        , testCase "while/transform/strict/renaming2" testStrictRenaming2
        , testCase "while/transform/strict/renaming3" testStrictRenaming3
        , testCase "while/transform/strict/assignment1" testStrictAssignment1
        , testCase "while/transform/strict/assignment2" testStrictAssignment2
        , testCase "while/transform/strict/arithmetic1" testStrictArithmetic1
        , testCase "while/transform/strict/arithmetic2" testStrictArithmetic2
        {-, testCase "while/transform/strict/arithmetic3" testStrictArithmetic3-}
        {-, testCase "while/transform/strict/arithmetic4" testStrictArithmetic4-}
        {-, testCase "while/transform/strict/arithmetic5" testStrictArithmetic5-}
        , testCase "while/transform/strict/arithmetic6" testStrictArithmetic6
        , testCase "while/transform/strict/arithmetic7" testStrictArithmetic7
        , testCase "while/transform/strict/arithmetic8" testStrictArithmetic8
        , testCase "while/transform/strict/control1" testStrictControl1
        {-, testCase "while/transform/strict/control2" testStrictControl2-}
        {-, testCase "while/transform/strict/control3" testStrictControl3-}
        {-, testCase "while/transform/strict/control4" testStrictControl4-}
        {-, testCase "while/transform/strict/control5" testStrictControl5-}
        {-, testCase "while/transform/strict/control6" testStrictControl6-}
        {-, testCase "while/transform/strict/control7" testStrictControl7-}
        {-, testCase "while/transform/strict/control8" testStrictControl8-}
        , testCase "while/transform/strict/control9" testStrictControl9
        , testCase "while/transform/strict/control10" testStrictControl10
        , testCase "while/transform/strict/control11" testStrictControl11
        , testCase "while/transform/strict/control12" testStrictControl12
        , testCase "while/transform/strict/control13" testStrictControl13
        , testCase "while/transform/strict/control14" testStrictControl14
        , testCase "while/transform/strict/control15" testStrictControl15
        , testCase "while/transform/strict/control16" testStrictControl16
        , testCase "while/transform/strict/control17" testStrictControl17

        , testCase "while/transform/goto/control1" testToGoto1
        ]

-- TODO: If I could somehow reduce the code duplication by using common code
-- for all three languages to transform them to their strict subset, I would
-- only need to test the transformation of a "for loop" to a While loop and the
-- transformation of a While with an arbitrary boolean expression to a While
-- loop of the form "WHILE x0 != 0..." since the other transformations would
-- already be covered by the Loop language tests.

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
  where e = "quux := foo  + 1;"  ++
            "bar  := foo  + 2;"  ++
            "x3   := quux + 0;"  ++
            "WHILE quux != 0 DO" ++
            "  x6 := bar + 0"    ++
            "END"
        s = "x1   := x2   + 1;"  ++
            "x4   := x2   + 2;"  ++
            "x3   := x1   + 0;"  ++
            "WHILE x1   != 0 DO" ++
            "  x6 := x4  + 0"    ++
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
        s = "x0 := x1 + 0;"    ++
            "x3 := x2 + 0;"    ++ -- x3 is counter
            "WHILE x3 != 0 DO" ++
            "  x3 := x3 - 1;"  ++
            "  x0 := x0 + 1"   ++
            "END"            

testStrictArithmetic2 :: Assertion
testStrictArithmetic2 = toStrict (parseE e) @?= parseS s
  where e = "x0 := x1 - x2"
        s = "x0 := x1 + 0;"    ++
            "x3 := x2 + 0;"    ++ -- x3 is counter
            "WHILE x3 != 0 DO" ++
            "  x3 := x3 - 1;"  ++
            "  x0 := x0 - 1"   ++
            "END"            

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

testStrictArithmetic6 :: Assertion
testStrictArithmetic6 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "x0 := x1 % x2"
        e2 = "x0 := x1;"          ++
             "x3 := x0;"          ++ -- x3 is counter
             "WHILE x3 != 0 DO"   ++
             "  x3 := x3 - 1;"    ++
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
testStrictControl1 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "WHILE 1 != 0 DO" ++
             "  x0 := x0 + 0"  ++
             "END"
        e2 = "IF 1 != 0 THEN"   ++
             "  x1 := 1"        ++
             "ELSE"             ++
             "  x1 := 0"        ++
             "END;"             ++
             "WHILE x1 != 0 DO" ++
             "  x0 := x0 + 0;"  ++
             "  IF 1 != 0 THEN" ++
             "    x1 := 1"      ++
             "  ELSE"           ++
             "    x1 := 0"      ++
             "  END "           ++  
             "END"              

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
             "x3 := x2;"        ++ -- x3 is counter
             "WHILE x3 != 0 DO" ++
             "  x3 := x3 - 1;"  ++
             "  x1 := x1 + 0"   ++
             "END"

testStrictControl12 :: Assertion
testStrictControl12 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 || 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "END"
        e2 = "x1 := 0;"         ++ -- x1 is unused
             "IF x0 > 5 THEN"   ++
             "  x1 := 1"        ++ 
             "END;"             ++
             "IF 5 < 6 THEN"    ++
             "  x1 := 1"        ++
             "END;"             ++
             "x2 := x1;"        ++ -- x2 is counter
             "WHILE x2 != 0 DO" ++
             "  x2 := x2 - 1;"  ++
             "  x0 := x0 + 0"   ++
             "END"

testStrictControl13 :: Assertion
testStrictControl13 = toStrict (parseE e1) @?= toStrict (parseE e2)
  where e1 = "IF x0 > 5 || 5 < 6 THEN" ++
             "  x0 := x0 + 0"          ++
             "ELSE"                    ++
             "  x1 := x1 + 0"          ++
             "END"
        e2 = "x2 := 0;"         ++ -- x2 is unused
             "x3 := 1;"         ++ -- x3 is unused
             "IF x0 > 5 THEN"   ++
             "  x2 := 1;"       ++
             "  x3 := 0"        ++
             "END;"             ++
             "IF 5 < 6 THEN"    ++
             "  x2 := 1;"       ++
             "  x3 := 0"        ++
             "END;"             ++
             "x4 := x2;"        ++ -- x4 is counter
             "WHILE x4 != 0 DO" ++
             "  x4 := x4 - 1;"  ++
             "  x0 := x0 + 0"   ++
             "END;"             ++
             "x5 := x3;"        ++ -- x5 is counter
             "WHILE x5 != 0 DO" ++
             "  x5 := x5 - 1;"  ++
             "  x1 := x1 + 0"   ++
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

testToGoto1 :: Assertion
testToGoto1 = toGoto (parseE e1) @?= parseGoto e2
  where e1 = "WHILE x1 < 6 && 8 = 9 DO" ++
             "  x1 := x1 + 1;"          ++
             "  x1 := x1 + 1"          ++
             "END"
        e2 = "M1: IF !(x1 < 6 && 8 = 9) THEN GOTO M2 END;" ++
             "    x1 := x1 + 1;"                           ++
             "    x1 := x1 + 1;"                           ++
             "    GOTO M1;"                                ++
             "M2: x0 := x0"


-- Helper

-- | Parse a string representation of an extended While program and return the AST.
parseE :: String -> Stat
parseE code = case parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of a strict While program and return the AST.
parseS :: String -> StrictAS.Stat
parseS code = case Strict.parse code of
    Left  err -> error err
    Right ast -> ast

-- | Parse a string representation of an extended Goto program and return the AST.
parseGoto :: String -> GotoAS.Stat
parseGoto code = case Goto.parse code of
    Left  err -> error err
    Right ast -> ast

module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Language.LoopGotoWhile.Loop.Strict.Tests
import qualified Language.LoopGotoWhile.Loop.Extended.Tests
import qualified Language.LoopGotoWhile.Loop.Transform.Tests
import qualified Language.LoopGotoWhile.While.Strict.Tests
import qualified Language.LoopGotoWhile.While.Extended.Tests
import qualified Language.LoopGotoWhile.While.Transform.Tests
import qualified Language.LoopGotoWhile.Goto.Strict.Tests
import qualified Language.LoopGotoWhile.Goto.Extended.Tests
import qualified Language.LoopGotoWhile.Goto.Transform.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Language.LoopGotoWhile.Loop.Strict.Tests"
        Language.LoopGotoWhile.Loop.Strict.Tests.tests
    , testGroup "Language.LoopGotoWhile.Loop.Extended.Tests"
        Language.LoopGotoWhile.Loop.Extended.Tests.tests
    , testGroup "Language.LoopGotoWhile.Loop.Transform.Tests"
        Language.LoopGotoWhile.Loop.Transform.Tests.tests
    , testGroup "Language.LoopGotoWhile.While.Strict.Tests"
        Language.LoopGotoWhile.While.Strict.Tests.tests
    , testGroup "Language.LoopGotoWhile.While.Extended.Tests"
        Language.LoopGotoWhile.While.Extended.Tests.tests
    , testGroup "Language.LoopGotoWhile.While.Transform.Tests"
        Language.LoopGotoWhile.While.Transform.Tests.tests
    , testGroup "Language.LoopGotoWhile.Goto.Strict.Tests"
        Language.LoopGotoWhile.Goto.Strict.Tests.tests
    , testGroup "Language.LoopGotoWhile.Goto.Extended.Tests"
        Language.LoopGotoWhile.Goto.Extended.Tests.tests
    , testGroup "Language.LoopGotoWhile.Goto.Transform.Tests"
        Language.LoopGotoWhile.Goto.Transform.Tests.tests
    ]

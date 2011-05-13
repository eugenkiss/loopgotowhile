module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Language.LoopGotoWhile.Abstract.Tests
import qualified Language.LoopGotoWhile.Loop.Strict.Tests
import qualified Language.LoopGotoWhile.Loop.Extended.Tests
import qualified Language.LoopGotoWhile.Loop.Transform.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Language.LoopGotoWhile.Abstract.Tests"
        Language.LoopGotoWhile.Abstract.Tests.tests
    , testGroup "Language.LoopGotoWhile.Loop.Strict.Tests"
        Language.LoopGotoWhile.Loop.Strict.Tests.tests
    , testGroup "Language.LoopGotoWhile.Loop.Extended.Tests"
        Language.LoopGotoWhile.Loop.Extended.Tests.tests
    , testGroup "Language.LoopGotoWhile.Loop.Transform.Tests"
        Language.LoopGotoWhile.Loop.Transform.Tests.tests
    ]

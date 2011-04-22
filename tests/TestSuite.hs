module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Language.LoopGotoWhile.Base.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Language.LoopGotoWhile.Base.Tests"
        Language.LoopGotoWhile.Base.Tests.tests
    ]

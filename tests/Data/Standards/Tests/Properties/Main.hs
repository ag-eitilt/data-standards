module Main where

import qualified Data.Standards.ISO.Country.Primary.Translation.Tests.Tasty as Translation

import qualified Test.Tasty as T

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "Property tests"
    [ Translation.tests
    ]

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Translation.Tests.Tasty where

import Data.Standards.ISO.Country.Primary.Translation.Tests.SmallCheck

import qualified Test.SmallCheck as S
import qualified Test.Tasty as T
import qualified Test.Tasty.SmallCheck as T

tests :: T.TestTree
tests = T.testGroup "ISO.Country.Primary.Translation"
    [ T.testProperty "Unpacking and repacking a 'Country' is idempotent through its own type"
        $ S.forAll scprop_idempotentUnpackPack
    ]

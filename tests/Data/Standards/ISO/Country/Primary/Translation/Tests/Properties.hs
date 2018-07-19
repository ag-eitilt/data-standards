{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Translation.Tests.Properties where

import Data.Standards.ISO.Country.Primary.Translation
import Data.Standards.ISO.Country.Primary.Alpha2.Tests.Properties ()
import Data.Standards.ISO.Country.Primary.Alpha3.Tests.Properties ()
import Data.Standards.ISO.Country.Primary.Numeric.Tests.Properties ()

import qualified Data.Standards.ISO.Country.Primary.Alpha2 as A2
import qualified Data.Standards.ISO.Country.Primary.Alpha3 as A3
import qualified Data.Standards.ISO.Country.Primary.Numeric as N

import qualified Test.SmallCheck as S
import qualified Test.SmallCheck.Series as S
import qualified Test.Tasty as T
import qualified Test.Tasty.SmallCheck as T

instance Monad m => S.Serial m Country where
    series = S.cons1 A2 S.\/ S.cons1 A3 S.\/ S.cons1 N

tests :: T.TestTree
tests = T.testGroup "ISO.Country.Primary.Translation"
    [ T.testProperty "Unpacking and repacking a 'Country' is idempotent through its own type"
        $ S.forAll $ \c -> case c of
            A2 _ -> Just c == fmap pack (unpack c :: Maybe A2.Alpha2)
            A3 _ -> Just c == fmap pack (unpack c :: Maybe A3.Alpha3)
            N _  -> Just c == fmap pack (unpack c :: Maybe N.Numeric)
    ]

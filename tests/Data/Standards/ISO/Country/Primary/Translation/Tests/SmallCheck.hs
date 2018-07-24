{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Translation.Tests.SmallCheck where

import Data.Standards.ISO.Country.Primary.Translation
import Data.Standards.ISO.Country.Primary.Alpha2.Tests.SmallCheck ()
import Data.Standards.ISO.Country.Primary.Alpha3.Tests.SmallCheck ()
import Data.Standards.ISO.Country.Primary.Numeric.Tests.SmallCheck ()

import qualified Data.Standards.ISO.Country.Primary.Alpha2 as A2
import qualified Data.Standards.ISO.Country.Primary.Alpha3 as A3
import qualified Data.Standards.ISO.Country.Primary.Numeric as N

--import qualified Test.SmallCheck as S
import qualified Test.SmallCheck.Series as S

instance Monad m => S.Serial m Country where
    series = S.cons1 A2 S.\/ S.cons1 A3 S.\/ S.cons1 N

scprop_idempotentUnpackPack :: Country -> Bool
scprop_idempotentUnpackPack c = Just c == case c of
    A2 _ -> fmap pack (unpack c :: Maybe A2.Alpha2)
    A3 _ -> fmap pack (unpack c :: Maybe A3.Alpha3)
    N _  -> fmap pack (unpack c :: Maybe N.Numeric)

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Translation.Tests where

import Data.Standards.ISO.Country.Primary.Translation
import Data.Standards.ISO.Country.Primary.Alpha2.Tests ()
import Data.Standards.ISO.Country.Primary.Alpha3.Tests ()
import Data.Standards.ISO.Country.Primary.Numeric.Tests ()

import qualified Test.SmallCheck.Series as S

instance Monad m => S.Serial m Country where
    series = S.cons1 A2 S.\/ S.cons1 A3 S.\/ S.cons1 N

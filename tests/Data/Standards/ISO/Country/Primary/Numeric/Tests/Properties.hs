{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Numeric.Tests.Properties where

import Data.Standards.ISO.Country.Primary.Numeric

import qualified GHC.Generics as G
import qualified Test.SmallCheck.Series as S

deriving instance G.Generic Numeric
instance Monad m => S.Serial m Numeric

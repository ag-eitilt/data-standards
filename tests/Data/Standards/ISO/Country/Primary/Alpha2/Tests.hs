{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Alpha2.Tests where

import Data.Standards.ISO.Country.Primary.Alpha2

import qualified GHC.Generics as G
import qualified Test.SmallCheck.Series as S

deriving instance G.Generic Alpha2
instance Monad m => S.Serial m Alpha2

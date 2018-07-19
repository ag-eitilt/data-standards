{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# OPTIONS -Wno-orphans #-}

module Data.Standards.ISO.Country.Primary.Alpha3.Tests where

import Data.Standards.ISO.Country.Primary.Alpha3

import qualified GHC.Generics as G
import qualified Test.SmallCheck.Series as S

deriving instance G.Generic Alpha3
instance Monad m => S.Serial m Alpha3

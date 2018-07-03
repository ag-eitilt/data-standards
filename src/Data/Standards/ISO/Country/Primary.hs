{-# OPTIONS_HADDOCK not-home #-}

{- | Module      : Data.Standards.ISO.Country.Primary
 -   Description : The country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary
    ( module Data.Standards.ISO.Country.Primary.Translation
    , A2.Alpha2 ( .. )
    , A3.Alpha3 ( .. )
    , N.Numeric ( .. )
    , C.Status ( .. )
    ) where

import qualified Data.Standards.ISO.Country.Primary.Alpha2 as A2
import qualified Data.Standards.ISO.Country.Primary.Alpha3 as A3
import qualified Data.Standards.ISO.Country.Primary.Common as C
import qualified Data.Standards.ISO.Country.Primary.Numeric as N

import Data.Standards.ISO.Country.Primary.Translation

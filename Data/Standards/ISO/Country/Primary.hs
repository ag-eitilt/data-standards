{-# OPTIONS_HADDOCK not-home #-}

{- | Module      : Data.Standards.ISO.Country.Primary
 -   Description : The country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : provisional
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary
    ( module Data.Standards.ISO.Country.Primary.Translation
    , Alpha2 ( .. )
    , Alpha3 ( .. )
    , Numeric ( .. )
    , Status ( .. )
    ) where

import Data.Standards.ISO.Country.Primary.Alpha2
import Data.Standards.ISO.Country.Primary.Alpha3
import Data.Standards.ISO.Country.Primary.Numeric

import Data.Standards.ISO.Country.Primary.Translation

{- | Module      : Data.Standards.ISO.Country.Primary.Common
 -   Description : Types used by all representations described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Common where

-- | Categories within the standard describing how much warning is given if a
-- code were to change or be replaced.
data Status
    -- | The code is active, and would only be replaced after a period as a
    -- 'TransitionalReservation'.
    = Official
    -- | The code is reserved and any other use is subject to approval.
    | ExceptionalReservation
    -- | The code has been marked for deletion, but is currently still reserved
    -- as implementations remove it or migrate to the replacement.
    | TransitionalReservation
    -- | Use of the code is restricted to historic vehicle registrations, but
    -- may still be reassigned at any point.
    | IndeterminateReservation
    -- | The code is used by another standard or high-profile organization, and
    -- will not be assigned in ISO 3166.  Officially, these are listed as an
    -- 'IndeterminateReservation'.
    | NotInUse
    -- | The code was previously included in the standard, but the country it
    -- referred to has been reassigned or no longer exists, and so (unless it's
    -- 'Numeric') it may be reused at any point.
    | Withdrawn

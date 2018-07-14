{- | Module      : Data.Standards.ISO.Country.Primary.Numeric
 -   Description : The numeric country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : provisional
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Numeric
    ( Numeric ( .. )
    , Status ( .. )
    , codeStatus
    ) where

import Data.Standards.ISO.Country.Primary.Common

import qualified Data.Hashable as H
import qualified Text.Printf as P

{-# DEPRECATED C080 "Use new code C010 after merge with C216 and C260" #-}
{-# DEPRECATED C128 "Use new code C296" #-}
{-# DEPRECATED C200 "Divided into C203 and C703" #-}
{-# DEPRECATED C216 "Use new code C010 after merge with C080 and C260" #-}
{-# DEPRECATED C230 "Divided into C231 and C232" #-}
{-# DEPRECATED C249 "Use new code C250" #-}
{-# DEPRECATED C278 "Use new code C276 after merge with C280" #-}
{-# DEPRECATED C280 "Use new code C276 after merge with C278" #-}
{-# DEPRECATED C396 "Use new code C581 after merge with C488, C849 and C872" #-}
{-# DEPRECATED C488 "Use new code C581 after merge with C396, C849 and C872" #-}
{-# DEPRECATED C530 "Divided into C531, C535 and C534" #-}
{-# DEPRECATED C532 "Divided into C530 and C533" #-}
{-# DEPRECATED C536 "Divided between C368 and C682" #-}
{-# DEPRECATED C582 "Divided into C580, C583, C584 and C585" #-}
{-# DEPRECATED C590 "Use new code C591 after merge with C594" #-}
{-# DEPRECATED C594 "Use new code C591 after merge with C590" #-}
{-# DEPRECATED C658 "Divided between C659 and C660" #-}
{-# DEPRECATED C698 "Use new code C356" #-}
{-# DEPRECATED C714 "Use new code C704" #-}
{-# DEPRECATED C720 "Use new code C887 after merge with C886" #-}
{-# DEPRECATED C736 "Divided between C728 and C729" #-}
{-# DEPRECATED C810 "Divided into C031, C051, C233, C268, C398, C417, C428, C440, C498, C643, C762, C795 and C860" #-}
{-# DEPRECATED C849 "Use new code C581 after merge with C396, C488 and C872" #-}
{-# DEPRECATED C872 "Use new code C581 after merge with C396, C488 and C849" #-}
{-# DEPRECATED C886 "Use new code C887 after merge with C720" #-}
{-# DEPRECATED C890 "Divided into C070, C191, C705, C807 and C891" #-}
{-# DEPRECATED C891 "Divided into C499 and C688" #-}

-- | The official numeric codes for countries, preventing collisions but
-- using an arbitrary mapping, translated into a type-safe representation.
-- Unless otherwise specified, all codes are 'Official'.
data Numeric
    = C004  -- ^ Afghanistan
    | C008  -- ^ Albania
    | C010  -- ^ Antarctica
    | C012  -- ^ Algeria
    | C016  -- ^ American Samoa
    | C020  -- ^ Andorra
    | C024  -- ^ Angola
    | C028  -- ^ Antigua and Barbuda
    | C031  -- ^ Azerbaijan
    | C032  -- ^ Argentina
    | C036  -- ^ Australia
    | C040  -- ^ Austria
    | C044  -- ^ Bahamas (the)
    | C048  -- ^ Bahrain

    | C050  -- ^ Bangladesh
    | C051  -- ^ Armenia
    | C052  -- ^ Barbados
    | C056  -- ^ Belgium
    | C060  -- ^ Bermuda
    | C064  -- ^ Bhutan
    | C068  -- ^ Bolivia (Plurinational State of)
    | C070  -- ^ Bosnia and Herzegovina
    | C072  -- ^ Botswana
    | C074  -- ^ Bouvet Island
    | C076  -- ^ Brazil
    | C080  -- ^ 'Withdrawn': British Antarctic Territory (the)
    | C084  -- ^ Belize
    | C086  -- ^ British Indian Ocean Territory (the)
    | C090  -- ^ Solomon Islands
    | C092  -- ^ Virgin Islands (British)
    | C096  -- ^ Brunei Darussalam

    | C100  -- ^ Bulgaria
    | C104  -- ^ Myanmar
    | C108  -- ^ Burundi
    | C112  -- ^ Belarus
    | C116  -- ^ Cambodia
    | C120  -- ^ Cameroon
    | C124  -- ^ Canada
    | C128  -- ^ 'Withdrawn': Canton and Enderbury Islands (the)
    | C132  -- ^ Cabo Verde
    | C136  -- ^ Cayman Islands (the)
    | C140  -- ^ Central African Republic (the)
    | C144  -- ^ Sri Lanka
    | C148  -- ^ Chad

    | C152  -- ^ Chile
    | C156  -- ^ China
    | C158  -- ^ Taiwan (Province of China)
    | C162  -- ^ Christmas Island
    | C166  -- ^ Cocos (Keeling) Islands (the)
    | C170  -- ^ Colombia
    | C174  -- ^ Comoros (the)
    | C175  -- ^ Mayotte
    | C178  -- ^ Congo (the)
    | C180  -- ^ Congo (the Democratic Republic of the)
    | C184  -- ^ Cook Islands (the)
    | C188  -- ^ Costa Rica
    | C191  -- ^ Croatia
    | C192  -- ^ Cuba
    | C196  -- ^ Cyprus

    | C200  -- ^ 'Withdrawn': Czechoslovakia
    | C203  -- ^ Czechia
    | C204  -- ^ Benin
    | C208  -- ^ Denmark
    | C212  -- ^ Dominica
    | C214  -- ^ Dominican Republic (the)
    | C216  -- ^ 'Withdrawn': Dronning Maud Land
    | C218  -- ^ Ecuador
    | C222  -- ^ El Salvador
    | C226  -- ^ Equatorial Guinea
    | C230  -- ^ 'Withdrawn': Ethiopia
    | C231  -- ^ Ethiopia
    | C232  -- ^ Eritrea
    | C233  -- ^ Estonia
    | C234  -- ^ Faroe Islands (the)
    | C238  -- ^ Falkland Islands (the) [Malvinas]
    | C239  -- ^ South Georgia and the South Sandwich Islands
    | C242  -- ^ Fiji
    | C246  -- ^ Finland
    | C248  -- ^ Ãland Islands
    | C249  -- ^ 'Withdrawn': France, Metropolitan

    | C250  -- ^ France
    | C254  -- ^ French Guiana
    | C258  -- ^ French Polynesia
    | C260  -- ^ French Southern Territories (the)
    | C262  -- ^ Djibouti
    | C266  -- ^ Gabon
    | C268  -- ^ Georgia
    | C270  -- ^ Gambia (the)
    | C275  -- ^ Palestine, State of
    | C276  -- ^ Germany
    | C278  -- ^ 'Withdrawn': German Democratic Republic (the)
    | C280  -- ^ 'Withdrawn': Germany (Federal Republic of)
    | C288  -- ^ Ghana
    | C292  -- ^ Gibraltar
    | C296  -- ^ Kiribati

    | C300  -- ^ Greece
    | C304  -- ^ Greenland
    | C308  -- ^ Grenada
    | C312  -- ^ Guadeloupe
    | C316  -- ^ Guam
    | C320  -- ^ Guatemala
    | C324  -- ^ Guinea
    | C328  -- ^ Guyana
    | C332  -- ^ Haiti
    | C334  -- ^ Heard Island and McDonald Islands
    | C336  -- ^ Holy See (the)
    | C340  -- ^ Honduras
    | C344  -- ^ Hong Kong
    | C348  -- ^ Hungary

    | C352  -- ^ Iceland
    | C356  -- ^ India
    | C360  -- ^ Indonesia
    | C364  -- ^ Iran (Islamic Republic of)
    | C368  -- ^ Iraq
    | C372  -- ^ Ireland
    | C376  -- ^ Israel
    | C380  -- ^ Italy
    | C384  -- ^ CÃ´te d'Ivoire
    | C388  -- ^ Jamaica
    | C392  -- ^ Japan
    | C396  -- ^ 'Withdrawn': Johnston Island
    | C398  -- ^ Kazakhstan

    | C400  -- ^ Jordan
    | C404  -- ^ Kenya
    | C408  -- ^ Korea (the Democratic People's Republic of)
    | C410  -- ^ Korea (the Republic of)
    | C414  -- ^ Kuwait
    | C417  -- ^ Kyrgyzstan
    | C418  -- ^ Lao People's Democratic Republic (the)
    | C422  -- ^ Lebanon
    | C426  -- ^ Lesotho
    | C428  -- ^ Latvia
    | C430  -- ^ Liberia
    | C434  -- ^ Libya
    | C438  -- ^ Liechtenstein
    | C440  -- ^ Lithuania
    | C442  -- ^ Luxembourg
    | C446  -- ^ Macao

    | C450  -- ^ Madagascar
    | C454  -- ^ Malawi
    | C458  -- ^ Malaysia
    | C462  -- ^ Maldives
    | C466  -- ^ Mali
    | C470  -- ^ Malta
    | C474  -- ^ Martinique
    | C478  -- ^ Mauritania
    | C480  -- ^ Mauritius
    | C484  -- ^ Mexico
    | C488  -- ^ 'Withdrawn': Midway Islands (the)
    | C492  -- ^ Monaco
    | C496  -- ^ Mongolia
    | C498  -- ^ Moldova (the Republic of)
    | C499  -- ^ Montenegro

    | C500  -- ^ Montserrat
    | C504  -- ^ Morocco
    | C508  -- ^ Mozambique
    | C512  -- ^ Oman
    | C516  -- ^ Namibia
    | C520  -- ^ Nauru
    | C524  -- ^ Nepal
    | C528  -- ^ Netherlands (the)
    | C530  -- ^ 'Withdrawn': Netherlands Antilles
    | C531  -- ^ CuraÃ§ao
    | C532  -- ^ 'Withdrawn': Netherlands Antilles
    | C533  -- ^ Aruba
    | C534  -- ^ Sint Maarten (Dutch part)
    | C535  -- ^ Bonaire, Sint Eustatius and Saba
    | C536  -- ^ 'Withdrawn': Saudi Arabian-Iraqi neutral zone (the)
    | C540  -- ^ New Caledonia
    | C548  -- ^ Vanuatu

    | C554  -- ^ New Zealand
    | C558  -- ^ Nicaragua
    | C562  -- ^ Niger (the)
    | C566  -- ^ Nigeria
    | C570  -- ^ Niue
    | C574  -- ^ Norfolk Island
    | C578  -- ^ Norway
    | C580  -- ^ Northern Mariana Islands (the)
    | C581  -- ^ United States Minor Outlying Islands (the)
    | C582  -- ^ 'Withdrawn': Pacific Islands (Trust Territory of the)
    | C583  -- ^ Micronesia (Federated States of)
    | C584  -- ^ Marshall Islands (the)
    | C585  -- ^ Palau
    | C586  -- ^ Pakistan
    | C590  -- ^ 'Withdrawn': Panama
    | C591  -- ^ Panama
    | C594  -- ^ 'Withdrawn': Panama Canal Zone (the)
    | C598  -- ^ Papua New Guinea

    | C600  -- ^ Paraguay
    | C604  -- ^ Peru
    | C608  -- ^ Philippines (the)
    | C612  -- ^ Pitcairn
    | C616  -- ^ Poland
    | C620  -- ^ Portugal
    | C624  -- ^ Guinea-Bissau
    | C626  -- ^ Timor-Leste
    | C630  -- ^ Puerto Rico
    | C634  -- ^ Qatar
    | C638  -- ^ RÃ©union
    | C642  -- ^ Romania
    | C643  -- ^ Russian Federation (the)
    | C646  -- ^ Rwanda

    | C652  -- ^ Saint BarthÃ©lemy
    | C654  -- ^ Saint Helena, Ascension and Tristan da Cunha
    | C658  -- ^ 'Withdrawn': Saint Kitts-Nevis-Anguilla
    | C659  -- ^ Saint Kitts and Nevis
    | C660  -- ^ Anguilla
    | C662  -- ^ Saint Lucia
    | C663  -- ^ Saint Martin (French part)
    | C666  -- ^ Saint Pierre and Miquelon
    | C670  -- ^ Saint Vincent and the Grenadines
    | C674  -- ^ San Marino
    | C678  -- ^ Sao Tome and Principe
    | C682  -- ^ Saudi Arabia
    | C686  -- ^ Senegal
    | C688  -- ^ Serbia
    | C690  -- ^ Seychelles
    | C694  -- ^ Sierra Leone
    | C698  -- ^ 'Withdrawn': Sikkim

    | C702  -- ^ Singapore
    | C703  -- ^ Slovakia
    | C704  -- ^ Viet Nam
    | C705  -- ^ Slovenia
    | C706  -- ^ Somalia
    | C710  -- ^ South Africa
    | C714  -- ^ 'Withdrawn': Viet Nam (Democratic Republic of)
    | C716  -- ^ Zimbabwe
    | C720  -- ^ 'Withdrawn': Yemen (Democratic)
    | C724  -- ^ Spain
    | C728  -- ^ South Sudan
    | C729  -- ^ Sudan (the)
    | C732  -- ^ Western Sahara
    | C736  -- ^ 'Withdrawn': Sudan (the)
    | C740  -- ^ Suriname
    | C744  -- ^ Svalbard and Jan Mayen
    | C748  -- ^ Swaziland

    | C752  -- ^ Sweden
    | C756  -- ^ Switzerland
    | C760  -- ^ Syrian Arab Republic
    | C762  -- ^ Tajikistan
    | C764  -- ^ Thailand
    | C768  -- ^ Togo
    | C772  -- ^ Tokelau
    | C776  -- ^ Tonga
    | C780  -- ^ Trinidad and Tobago
    | C784  -- ^ United Arab Emirates (the)
    | C788  -- ^ Tunisia
    | C792  -- ^ Turkey
    | C795  -- ^ Turkmenistan
    | C796  -- ^ Turks and Caicos Islands (the)
    | C798  -- ^ Tuvalu

    | C800  -- ^ Uganda
    | C804  -- ^ Ukraine
    | C807  -- ^ Macedonia (the former Yugoslav Republic of)
    | C810  -- ^ 'Withdrawn': USSR (the)
    | C818  -- ^ Egypt
    | C826  -- ^ United Kingdom of Great Britain and Northern Ireland (the)
    | C831  -- ^ Guernsey
    | C832  -- ^ Jersey
    | C833  -- ^ Isle of Man
    | C834  -- ^ Tanzania, United Republic of
    | C840  -- ^ United States of America (the)
    | C849  -- ^ 'Withdrawn': United States Miscellaneous Pacific Islands (the)

    | C850  -- ^ Virgin Islands (U.S.)
    | C854  -- ^ Burkina Faso
    | C858  -- ^ Uruguay
    | C860  -- ^ Uzbekistan
    | C862  -- ^ Venezuela (Bolivarian Republic of)
    | C872  -- ^ 'Withdrawn': Wake Island
    | C876  -- ^ Wallis and Futuna
    | C882  -- ^ Samoa
    | C886  -- ^ 'Withdrawn': Yemen Arab Republic
    | C887  -- ^ Yemen
    | C890  -- ^ 'Withdrawn': Yugoslavia (Socialist Federal Republic of)
    | C891  -- ^ 'Withdrawn': Serbia and Montenegro
    | C894  -- ^ Zambia
  deriving ( Eq, Show, Read, Ord, Bounded )

-- | The 'Int' values returned or processed by this instance are the true value
-- of the country code, rather than being incremental.
instance Enum Numeric where
    toEnum i = read $ 'C' : P.printf "%03d" i
    fromEnum = read . tail . show

    -- The derived instance takes a path through 'fromEnum' and 'toEnum' rather
    -- than using 'succ' and 'pred'.  This seems like a bug, but if so, it's a
    -- bug in the Haskell Report not just GHC.
    -- 
    -- Relevant sections of the Report: 6.3.4 (which gives rules for 'enumFrom'
    -- and 'enumFromThen' based on 'enumFromTo' and 'enumFromThenTo' when also
    -- Bounded that GHC isn't following) and 11.2 (same as above, plus defaults
    -- for the latter based on 'Int' lists) as well as the definition in the
    -- Prelude (section 9; the same implementations as GHC despite all being
    -- 'Int' lists -- running into the same problem with 6.3.4 -- and having a
    -- note that explicitly says they only make sense for injective mappings).
    enumFrom = flip enumFromTo maxBound
    enumFromTo x y
        | x > y     = []
        | x == y    = [x]
        | otherwise = x : enumFromTo (succ x) y

    enumFromThen x s = enumFromThenTo x s bound
       where bound
               | x <= s    = maxBound
               | otherwise = minBound
    enumFromThenTo x s y
        | past x y  = []
        | otherwise = x : skipOver thenTo
      where (ln, thenTo)       = break ((==) s) $ fromTo x
            fromTo x1
                | x1 == y      = [x1]
                | otherwise    = x1 : fromTo (step x1)
            (past, step)
                | x <= s       = ((>), succ)
                | otherwise    = ((<), pred)
            n = length ln
            skipOver []        = []
            skipOver xs@(x1:_) = x1 : skipOver (drop n xs)

    --TODO: Depending on how the compiler transforms these, it might be better
    -- to put them into a lookup table; check with benchmarking.
    succ C004 = C008
    succ C008 = C010
    succ C010 = C012
    succ C012 = C016
    succ C016 = C020
    succ C020 = C024
    succ C024 = C028
    succ C028 = C031
    succ C032 = C036
    succ C036 = C040
    succ C040 = C044
    succ C044 = C048
    succ C048 = C050

    succ C052 = C056
    succ C056 = C060
    succ C060 = C064
    succ C064 = C068
    succ C068 = C070
    succ C070 = C072
    succ C072 = C074
    succ C074 = C076
    succ C076 = C080
    succ C080 = C084
    succ C084 = C086
    succ C086 = C090
    succ C090 = C092
    succ C092 = C096
    succ C096 = C100

    succ C100 = C104
    succ C104 = C108
    succ C108 = C112
    succ C112 = C116
    succ C116 = C120
    succ C120 = C124
    succ C124 = C128
    succ C128 = C132
    succ C132 = C136
    succ C136 = C140
    succ C140 = C144
    succ C144 = C148
    succ C148 = C152

    succ C152 = C156
    succ C156 = C158
    succ C158 = C162
    succ C162 = C166
    succ C166 = C170
    succ C170 = C174
    succ C175 = C178
    succ C178 = C180
    succ C180 = C184
    succ C184 = C188
    succ C188 = C191
    succ C192 = C196
    succ C196 = C200

    succ C200 = C203
    succ C204 = C208
    succ C208 = C212
    succ C212 = C214
    succ C214 = C216
    succ C216 = C218
    succ C218 = C222
    succ C222 = C226
    succ C226 = C230
    succ C234 = C238
    succ C239 = C242
    succ C242 = C246
    succ C246 = C248

    succ C250 = C254
    succ C254 = C258
    succ C258 = C260
    succ C260 = C262
    succ C262 = C266
    succ C266 = C268
    succ C268 = C270
    succ C270 = C275
    succ C276 = C278
    succ C278 = C280
    succ C280 = C288
    succ C288 = C292
    succ C292 = C296
    succ C296 = C300

    succ C300 = C304
    succ C304 = C308
    succ C308 = C312
    succ C312 = C316
    succ C316 = C320
    succ C320 = C324
    succ C324 = C328
    succ C328 = C332
    succ C332 = C334
    succ C334 = C336
    succ C336 = C340
    succ C340 = C344
    succ C344 = C348
    succ C348 = C352

    succ C352 = C356
    succ C356 = C360
    succ C360 = C364
    succ C364 = C368
    succ C368 = C372
    succ C372 = C376
    succ C376 = C380
    succ C380 = C384
    succ C384 = C388
    succ C388 = C392
    succ C392 = C396
    succ C396 = C398
    succ C398 = C400

    succ C400 = C404
    succ C404 = C408
    succ C408 = C410
    succ C410 = C414
    succ C414 = C417
    succ C418 = C422
    succ C422 = C426
    succ C426 = C428
    succ C428 = C430
    succ C430 = C434
    succ C434 = C438
    succ C438 = C440
    succ C440 = C442
    succ C442 = C446
    succ C446 = C450

    succ C450 = C454
    succ C454 = C458
    succ C458 = C462
    succ C462 = C466
    succ C466 = C470
    succ C470 = C474
    succ C474 = C478
    succ C478 = C480
    succ C480 = C484
    succ C484 = C488
    succ C488 = C492
    succ C492 = C496
    succ C496 = C498

    succ C500 = C504
    succ C504 = C508
    succ C508 = C512
    succ C512 = C516
    succ C516 = C520
    succ C520 = C524
    succ C524 = C528
    succ C528 = C530
    succ C536 = C540
    succ C540 = C548
    succ C548 = C554

    succ C554 = C558
    succ C558 = C562
    succ C562 = C566
    succ C566 = C570
    succ C570 = C574
    succ C574 = C578
    succ C578 = C580
    succ C586 = C590
    succ C591 = C594
    succ C594 = C598
    succ C598 = C600

    succ C600 = C604
    succ C604 = C608
    succ C608 = C612
    succ C612 = C616
    succ C616 = C620
    succ C620 = C624
    succ C624 = C626
    succ C626 = C630
    succ C630 = C634
    succ C634 = C638
    succ C638 = C642
    succ C643 = C646
    succ C646 = C652

    succ C652 = C654
    succ C654 = C658
    succ C660 = C662
    succ C663 = C666
    succ C666 = C670
    succ C670 = C674
    succ C674 = C678
    succ C678 = C682
    succ C682 = C686
    succ C686 = C688
    succ C688 = C690
    succ C690 = C694
    succ C694 = C698
    succ C698 = C702

    succ C706 = C710
    succ C710 = C714
    succ C714 = C716
    succ C716 = C720
    succ C720 = C724
    succ C724 = C728
    succ C729 = C732
    succ C732 = C736
    succ C736 = C740
    succ C740 = C744
    succ C744 = C748
    succ C748 = C752

    succ C752 = C756
    succ C756 = C760
    succ C760 = C762
    succ C762 = C764
    succ C764 = C768
    succ C768 = C772
    succ C772 = C776
    succ C776 = C780
    succ C780 = C784
    succ C784 = C788
    succ C788 = C792
    succ C792 = C795
    succ C796 = C798
    succ C798 = C800

    succ C800 = C804
    succ C804 = C807
    succ C807 = C810
    succ C810 = C818
    succ C818 = C826
    succ C826 = C831
    succ C834 = C840
    succ C840 = C849

    succ C850 = C854
    succ C854 = C858
    succ C858 = C860
    succ C860 = C862
    succ C862 = C872
    succ C872 = C876
    succ C876 = C882
    succ C882 = C886
    succ C887 = C890
    succ C891 = C894

    succ c
        | c == maxBound = error "succ(Numeric) tried to take `succ' of last tag in enumeration"
        | otherwise     = toEnum . (+) 1 $ fromEnum c

    pred C008 = C004
    pred C010 = C008
    pred C012 = C010
    pred C016 = C012
    pred C020 = C016
    pred C024 = C020
    pred C028 = C024
    pred C031 = C028
    pred C036 = C032
    pred C040 = C036
    pred C044 = C040
    pred C048 = C044

    pred C050 = C048
    pred C056 = C052
    pred C060 = C056
    pred C064 = C060
    pred C068 = C064
    pred C070 = C068
    pred C072 = C070
    pred C074 = C072
    pred C076 = C074
    pred C080 = C076
    pred C084 = C080
    pred C086 = C084
    pred C090 = C086
    pred C092 = C090
    pred C096 = C092

    pred C100 = C096
    pred C104 = C100
    pred C108 = C104
    pred C112 = C108
    pred C116 = C112
    pred C120 = C116
    pred C124 = C120
    pred C128 = C124
    pred C132 = C128
    pred C136 = C132
    pred C140 = C136
    pred C144 = C140
    pred C148 = C144

    pred C152 = C148
    pred C156 = C152
    pred C158 = C156
    pred C162 = C158
    pred C166 = C162
    pred C170 = C166
    pred C174 = C170
    pred C178 = C175
    pred C180 = C178
    pred C184 = C180
    pred C188 = C184
    pred C191 = C188
    pred C196 = C192

    pred C200 = C196
    pred C203 = C200
    pred C208 = C204
    pred C212 = C208
    pred C214 = C212
    pred C216 = C214
    pred C218 = C216
    pred C222 = C218
    pred C226 = C222
    pred C230 = C226
    pred C238 = C234
    pred C242 = C239
    pred C246 = C242
    pred C248 = C246

    pred C254 = C250
    pred C258 = C254
    pred C260 = C258
    pred C262 = C260
    pred C266 = C262
    pred C268 = C266
    pred C270 = C268
    pred C275 = C270
    pred C278 = C276
    pred C280 = C278
    pred C288 = C280
    pred C292 = C288
    pred C296 = C292

    pred C300 = C296
    pred C304 = C300
    pred C308 = C304
    pred C312 = C308
    pred C316 = C312
    pred C320 = C316
    pred C324 = C320
    pred C328 = C324
    pred C332 = C328
    pred C334 = C332
    pred C336 = C334
    pred C340 = C336
    pred C344 = C340
    pred C348 = C344

    pred C352 = C348
    pred C356 = C352
    pred C360 = C356
    pred C364 = C360
    pred C368 = C364
    pred C372 = C368
    pred C376 = C372
    pred C380 = C376
    pred C384 = C380
    pred C388 = C384
    pred C392 = C388
    pred C396 = C392
    pred C398 = C396

    pred C400 = C398
    pred C404 = C400
    pred C408 = C404
    pred C410 = C408
    pred C414 = C410
    pred C417 = C414
    pred C422 = C418
    pred C426 = C422
    pred C428 = C426
    pred C430 = C428
    pred C434 = C430
    pred C438 = C434
    pred C440 = C438
    pred C442 = C440
    pred C446 = C442

    pred C450 = C446
    pred C454 = C450
    pred C458 = C454
    pred C462 = C458
    pred C466 = C462
    pred C470 = C466
    pred C474 = C470
    pred C478 = C474
    pred C480 = C478
    pred C484 = C480
    pred C488 = C484
    pred C492 = C488
    pred C496 = C492
    pred C498 = C496

    pred C504 = C500
    pred C508 = C504
    pred C512 = C508
    pred C516 = C512
    pred C520 = C516
    pred C524 = C520
    pred C528 = C524
    pred C530 = C528
    pred C540 = C536
    pred C548 = C540

    pred C554 = C548
    pred C558 = C554
    pred C562 = C558
    pred C566 = C562
    pred C570 = C566
    pred C574 = C570
    pred C578 = C574
    pred C580 = C578
    pred C590 = C586
    pred C594 = C591
    pred C598 = C594

    pred C600 = C598
    pred C604 = C600
    pred C608 = C604
    pred C612 = C608
    pred C616 = C612
    pred C620 = C616
    pred C624 = C620
    pred C626 = C624
    pred C630 = C626
    pred C634 = C630
    pred C638 = C634
    pred C642 = C638
    pred C646 = C643

    pred C652 = C646
    pred C654 = C652
    pred C658 = C654
    pred C662 = C660
    pred C666 = C663
    pred C670 = C666
    pred C674 = C670
    pred C678 = C674
    pred C682 = C678
    pred C686 = C682
    pred C688 = C686
    pred C690 = C688
    pred C694 = C690
    pred C698 = C694

    pred C702 = C698
    pred C710 = C706
    pred C714 = C710
    pred C716 = C714
    pred C720 = C716
    pred C724 = C720
    pred C728 = C724
    pred C732 = C729
    pred C736 = C732
    pred C740 = C736
    pred C744 = C740
    pred C748 = C744

    pred C752 = C748
    pred C756 = C752
    pred C760 = C756
    pred C762 = C760
    pred C764 = C762
    pred C768 = C764
    pred C772 = C768
    pred C776 = C772
    pred C780 = C776
    pred C784 = C780
    pred C788 = C784
    pred C792 = C788
    pred C795 = C792
    pred C798 = C796

    pred C800 = C798
    pred C804 = C800
    pred C807 = C804
    pred C810 = C807
    pred C818 = C810
    pred C826 = C818
    pred C831 = C826
    pred C840 = C834
    pred C849 = C840

    pred C854 = C850
    pred C858 = C854
    pred C860 = C858
    pred C862 = C860
    pred C872 = C862
    pred C876 = C872
    pred C882 = C876
    pred C886 = C882
    pred C890 = C887
    pred C894 = C891

    pred c
        | c == minBound = error "pred(Numeric) tried to take `pred' of first tag in enumeration"
        | otherwise     = toEnum . subtract 1 $ fromEnum c

instance H.Hashable Numeric where
    hashWithSalt = H.hashUsing fromEnum

-- | The stability of any particular country code.
codeStatus :: Numeric -> Status
codeStatus C080 = Withdrawn
codeStatus C128 = Withdrawn
codeStatus C200 = Withdrawn
codeStatus C216 = Withdrawn
codeStatus C230 = Withdrawn
codeStatus C249 = Withdrawn
codeStatus C278 = Withdrawn
codeStatus C280 = Withdrawn
codeStatus C396 = Withdrawn
codeStatus C488 = Withdrawn
codeStatus C530 = Withdrawn
codeStatus C532 = Withdrawn
codeStatus C536 = Withdrawn
codeStatus C582 = Withdrawn
codeStatus C590 = Withdrawn
codeStatus C594 = Withdrawn
codeStatus C658 = Withdrawn
codeStatus C698 = Withdrawn
codeStatus C714 = Withdrawn
codeStatus C720 = Withdrawn
codeStatus C736 = Withdrawn
codeStatus C810 = Withdrawn
codeStatus C849 = Withdrawn
codeStatus C872 = Withdrawn
codeStatus C886 = Withdrawn
codeStatus C890 = Withdrawn
codeStatus C891 = Withdrawn
codeStatus _ = Official

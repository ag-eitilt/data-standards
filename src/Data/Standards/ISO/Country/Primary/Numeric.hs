{- | Module      : Data.Standards.ISO.Country.Primary.Numeric
 -   Description : The numeric country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Numeric
    ( Numeric ( .. )
    , Status ( .. )
    , codeStatus
    ) where

import Data.Standards.ISO.Country.Primary.Common

import qualified Data.Hashable as H

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
  deriving ( Eq, Show, Read, Enum, Bounded )
-- | Convert the country code into a unique 'Int' value.
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

{- | Module      : Data.Standards.ISO.Country.Primary.Alpha3
 -   Description : The three-character country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Alpha3
    ( Alpha3 ( .. )
    , Status ( .. )
    , codeStatus
    ) where

import Data.Standards.ISO.Country.Primary.Common

import qualified Data.Hashable as H

{-# DEPRECATED ANT "Divided into BES, CUW and SXM, or use ISO 3166-3 code ANHH" #-}
{-# DEPRECATED BUR "Use new code MMR or ISO 3166-3 code BUMM" #-}
{-# DEPRECATED BYS "Use new code BLR or ISO 3166-3 code BYAA" #-}
{-# DEPRECATED CSK "Divided into CZE and SVK, or use ISO 3166-3 code CSHH" #-}
{-# DEPRECATED NTZ "Divided between IRQ and SAU, or use ISO 3166-3 code NTHH" #-}
{-# DEPRECATED ROM "Use new code ROU" #-}
{-# DEPRECATED SCG "Divided into MNE and SRB, or use ISO 3166-3 code CSXX" #-}
{-# DEPRECATED TMP "Use new code TLS or ISO 3166-3 code TPTL" #-}
{-# DEPRECATED YUG "Use ISO 3166-3 code YUCS" #-}
{-# DEPRECATED ZAR "Use new code COD or ISO 3166-3 code ZRCD" #-}

{-# DEPRECATED ADN "Use new code YEM" #-}
{-# DEPRECATED BDS "Use official code BRB" #-}
{-# DEPRECATED BRU "Use official code BRN" #-}
{-# DEPRECATED CDN "Use official code CAN" #-}
{-# DEPRECATED EAK "Use official code KEN" #-}
{-# DEPRECATED EAT "Use code for entire country TZA" #-}
{-# DEPRECATED EAU "Use official code UGA" #-}
{-# DEPRECATED EAZ "Use code for entire country TZA" #-}
{-# DEPRECATED GBA "Use code for entire region GGY" #-}
{-# DEPRECATED GBG "Use official code GGY" #-}
{-# DEPRECATED GBJ "Use official code JEY" #-}
{-# DEPRECATED GBM "Use official code IMN" #-}
{-# DEPRECATED GBZ "Use official code GIB" #-}
{-# DEPRECATED GCA "Use official code GTM" #-}
{-# DEPRECATED HKJ "Use official code JOR" #-}
{-# DEPRECATED MAL "Use official code MYS" #-}
{-# DEPRECATED RCA "Use official code CAF" #-}
{-# DEPRECATED RCB "Use official code COG" #-}
{-# DEPRECATED RCH "Use official code CHL" #-}
{-# DEPRECATED RMM "Use official code MLI" #-}
{-# DEPRECATED RNR "Use official code ZMB" #-}
{-# DEPRECATED ROK "Use official code KOR" #-}
{-# DEPRECATED RSM "Use official code SMR" #-}
{-# DEPRECATED RSR "Use official code ZWE" #-}
{-# DEPRECATED SLO "Use official code SVN" #-}
{-# DEPRECATED SME "Use official code SUR" #-}
{-# DEPRECATED TMN "Use official code TKN" #-}
{-# DEPRECATED WAG "Use official code GMB" #-}
{-# DEPRECATED WAL "Use official code SLE" #-}
{-# DEPRECATED WAN "Use official code NGA" #-}
{-# DEPRECATED ZRE "Use official code ZAR, new code COD or ISO 3166-3 code ZRCD" #-}

-- | The official codes for countries with less collisions and therefore more
-- similar to the full name, translated into a type-safe representation.  Unless
-- otherwise specified, all codes are 'Official'.
data Alpha3
    = ABW  -- ^ Aruba
    | ADN  -- ^ 'IndeterminateReservation': Aden (road vehicles)
    | AFG  -- ^ Afghanistan
    | AGO  -- ^ Angola
    | AIA  -- ^ Anguilla
    | ALA  -- ^ Åland Islands
    | ALB  -- ^ Albania
    | AND  -- ^ Andorra
    | ANT  -- ^ 'TransitionalReservation': Netherlands Antilles
    | ARE  -- ^ United Arab Emirates (the)
    | ARG  -- ^ Argentina
    | ARM  -- ^ Armenia
    | ASC  -- ^ 'ExceptionalReservation': Ascension Island
    | ASM  -- ^ American Samoa
    | ATA  -- ^ Antarctica
    | ATF  -- ^ French Southern Territories (the)
    | ATG  -- ^ Antigua and Barbuda
    | AUS  -- ^ Australia
    | AUT  -- ^ Austria
    | AZE  -- ^ Azerbaijan

    | BDI  -- ^ Burundi
    | BDS  -- ^ 'IndeterminateReservation': Barbados (road vehicles)
    | BEL  -- ^ Belgium
    | BEN  -- ^ Benin
    | BES  -- ^ Bonaire, Sint Eustatius and Saba
    | BFA  -- ^ Burkina Faso
    | BGD  -- ^ Bangladesh
    | BGR  -- ^ Bulgaria
    | BHR  -- ^ Bahrain
    | BHS  -- ^ Bahamas (the)
    | BIH  -- ^ Bosnia and Herzegovina
    | BLM  -- ^ Saint Barthélemy
    | BLR  -- ^ Belarus
    | BLZ  -- ^ Belize
    | BMU  -- ^ Bermuda
    | BOL  -- ^ Bolivia (Plurinational State of)
    | BRA  -- ^ Brazil
    | BRB  -- ^ Barbados
    | BRN  -- ^ Brunei Darussalam
    | BRU  -- ^ 'IndeterminateReservation': Brunai (road vehicles)
    | BTN  -- ^ Bhutan
    | BUR  -- ^ 'TransitionalReservation': Burma
    | BVT  -- ^ Bouvet Island
    | BWA  -- ^ Botswana
    | BYS  -- ^ 'TransitionalReservation': Byelorussian SSR

    | CAF  -- ^ Central African Republic (the)
    | CAN  -- ^ Canada
    | CCK  -- ^ Cocos (Keeling) Islands (the)
    | CDN  -- ^ 'IndeterminateReservation': Canada (road vehicles)
    | CHE  -- ^ Switzerland
    | CHL  -- ^ Chile
    | CHN  -- ^ China
    | CIV  -- ^ Côte d'Ivoire
    | CMR  -- ^ Cameroon
    | COD  -- ^ Congo (the Democratic Republic of the)
    | COG  -- ^ Congo (the)
    | COK  -- ^ Cook Islands (the)
    | COL  -- ^ Colombia
    | COM  -- ^ Comoros (the)
    | CPT  -- ^ 'ExceptionalReservation': Clipperton Island
    | CPV  -- ^ Cabo Verde
    | CRI  -- ^ Costa Rica
    | CSK  -- ^ 'TransitionalReservation': Czechoslovakia
    | CUB  -- ^ Cuba
    | CUW  -- ^ Curaçao
    | CXR  -- ^ Christmas Island
    | CYM  -- ^ Cayman Islands (the)
    | CYP  -- ^ Cyprus
    | CZE  -- ^ Czechia

    | DEU  -- ^ Germany
    | DGA  -- ^ 'ExceptionalReservation': Diego Garcia
    | DJI  -- ^ Djibouti
    | DMA  -- ^ Dominica
    | DNK  -- ^ Denmark
    | DOM  -- ^ Dominican Republic (the)
    | DZA  -- ^ Algeria

    | EAK  -- ^ 'IndeterminateReservation': Kenya (road vehicles)
    | EAT  -- ^ 'IndeterminateReservation': Tanganyika (road vehicles)
    | EAU  -- ^ 'IndeterminateReservation': Uganda (road vehicles)
    | EAZ  -- ^ 'IndeterminateReservation': Zanzibar (road vehicles)
    | ECU  -- ^ Ecuador
    | EGY  -- ^ Egypt
    | ERI  -- ^ Eritrea
    | ESH  -- ^ Western Sahara
    | ESP  -- ^ Spain
    | EST  -- ^ Estonia
    | ETH  -- ^ Ethiopia

    | FIN  -- ^ Finland
    | FJI  -- ^ Fiji
    | FLK  -- ^ Falkland Islands (the) [Malvinas]
    | FRA  -- ^ France
    | FRO  -- ^ Faroe Islands (the)
    | FSM  -- ^ Micronesia (Federated States of)
    | FXX  -- ^ 'ExceptionalReservation': France, Metropolitan

    | GAB  -- ^ Gabon
    | GBA  -- ^ 'IndeterminateReservation': Alderney (road vehicles)
    | GBG  -- ^ 'IndeterminateReservation': Guernsey (road vehicles)
    | GBJ  -- ^ 'IndeterminateReservation': Jersey (road vehicles)
    | GBM  -- ^ 'IndeterminateReservation': Isle of Man (road vehicles)
    | GBR  -- ^ United Kingdom of Great Britain and Northern Ireland (the)
    | GBZ  -- ^ 'IndeterminateReservation': Gibraltar (road vehicles)
    | GCA  -- ^ 'IndeterminateReservation': Guatemala (road vehicles)
    | GEO  -- ^ Georgia
    | GGY  -- ^ Guernsey
    | GHA  -- ^ Ghana
    | GIB  -- ^ Gibraltar
    | GIN  -- ^ Guinea
    | GLP  -- ^ Guadeloupe
    | GMB  -- ^ Gambia (the)
    | GNB  -- ^ Guinea-Bissau
    | GNQ  -- ^ Equatorial Guinea
    | GRC  -- ^ Greece
    | GRD  -- ^ Grenada
    | GRL  -- ^ Greenland
    | GTM  -- ^ Guatemala
    | GUF  -- ^ French Guiana
    | GUM  -- ^ Guam
    | GUY  -- ^ Guyana

    | HKJ  -- ^ 'IndeterminateReservation': Jordan (road vehicles)
    | HKG  -- ^ Hong Kong
    | HMD  -- ^ Heard Island and McDonald Islands
    | HND  -- ^ Honduras
    | HRV  -- ^ Croatia
    | HTI  -- ^ Haiti
    | HUN  -- ^ Hungary

    | IDN  -- ^ Indonesia
    | IMN  -- ^ Isle of Man
    | IND  -- ^ India
    | IOT  -- ^ British Indian Ocean Territory (the)
    | IRL  -- ^ Ireland
    | IRN  -- ^ Iran (Islamic Republic of)
    | IRQ  -- ^ Iraq
    | ISL  -- ^ Iceland
    | ISR  -- ^ Israel
    | ITA  -- ^ Italy

    | JAM  -- ^ Jamaica
    | JEY  -- ^ Jersey
    | JOR  -- ^ Jordan
    | JPN  -- ^ Japan

    | KAZ  -- ^ Kazakhstan
    | KEN  -- ^ Kenya
    | KGZ  -- ^ Kyrgyzstan
    | KHM  -- ^ Cambodia
    | KIR  -- ^ Kiribati
    | KNA  -- ^ Saint Kitts and Nevis
    | KOR  -- ^ Korea (the Republic of)
    | KWT  -- ^ Kuwait

    | LAO  -- ^ Lao People's Democratic Republic (the)
    | LBN  -- ^ Lebanon
    | LBR  -- ^ Liberia
    | LBY  -- ^ Libya
    | LCA  -- ^ Saint Lucia
    | LIE  -- ^ Liechtenstein
    | LKA  -- ^ Sri Lanka
    | LSO  -- ^ Lesotho
    | LTU  -- ^ Lithuania
    | LUX  -- ^ Luxembourg
    | LVA  -- ^ Latvia

    | MAC  -- ^ Macao
    | MAF  -- ^ Saint Martin (French part)
    | MAL  -- ^ 'IndeterminateReservation': Malaysia (road vehicles)
    | MAR  -- ^ Morocco
    | MCO  -- ^ Monaco
    | MDA  -- ^ Moldova (the Republic of)
    | MDG  -- ^ Madagascar
    | MDV  -- ^ Maldives
    | MEX  -- ^ Mexico
    | MHL  -- ^ Marshall Islands (the)
    | MKD  -- ^ Macedonia (the former Yugoslav Republic of)
    | MLI  -- ^ Mali
    | MLT  -- ^ Malta
    | MMR  -- ^ Myanmar
    | MNE  -- ^ Montenegro
    | MNG  -- ^ Mongolia
    | MNP  -- ^ Northern Mariana Islands (the)
    | MOZ  -- ^ Mozambique
    | MRT  -- ^ Mauritania
    | MSR  -- ^ Montserrat
    | MTQ  -- ^ Martinique
    | MUS  -- ^ Mauritius
    | MWI  -- ^ Malawi
    | MYS  -- ^ Malaysia
    | MYT  -- ^ Mayotte

    | NAM  -- ^ Namibia
    | NCL  -- ^ New Caledonia
    | NER  -- ^ Niger (the)
    | NFK  -- ^ Norfolk Island
    | NGA  -- ^ Nigeria
    | NIC  -- ^ Nicaragua
    | NIU  -- ^ Niue
    | NLD  -- ^ Netherlands (the)
    | NOR  -- ^ Norway
    | NPL  -- ^ Nepal
    | NRU  -- ^ Nauru
    | NTZ  -- ^ 'TransitionalReservation': Saudi Arabian-Iraqi neutral zone (the)
    | NZL  -- ^ New Zealand

    | OMN  -- ^ Oman

    | PAK  -- ^ Pakistan
    | PAN  -- ^ Panama
    | PCN  -- ^ Pitcairn
    | PER  -- ^ Peru
    | PHL  -- ^ Philippines (the)
    | PLW  -- ^ Palau
    | PNG  -- ^ Papua New Guinea
    | POL  -- ^ Poland
    | PRI  -- ^ Puerto Rico
    | PRK  -- ^ Korea (the Democratic People's Republic of)
    | PRT  -- ^ Portugal
    | PRY  -- ^ Paraguay
    | PSE  -- ^ Palestine, State of
    | PYF  -- ^ French Polynesia

    | QAT  -- ^ Qatar

    | RCA  -- ^ 'IndeterminateReservation': Central African Republic (road vehicles)
    | RCB  -- ^ 'IndeterminateReservation': Congo, People's Republic of (road vehicles)
    | RCH  -- ^ 'IndeterminateReservation': Chile (road vehicles)
    | REU  -- ^ Réunion
    | RMM  -- ^ 'IndeterminateReservation': Mali (road vehicles)
    | RNR  -- ^ 'IndeterminateReservation': Zambia (road vehicles)
    | ROK  -- ^ 'IndeterminateReservation': Korea, Republic of (road vehicles)
    | ROM  -- ^ 'TransitionalReservation': Romania
    | ROU  -- ^ Romania
    | RSM  -- ^ 'IndeterminateReservation': San Marino (road vehicles)
    | RSR  -- ^ 'IndeterminateReservation': Zimbabwe (road vehicles)
    | RUS  -- ^ Russian Federation (the)
    | RWA  -- ^ Rwanda

    | SAU  -- ^ Saudi Arabia
    | SCG  -- ^ 'TransitionalReservation': Serbia and Montenegro
    | SDN  -- ^ Sudan (the)
    | SEN  -- ^ Senegal
    | SGP  -- ^ Singapore
    | SGS  -- ^ South Georgia and the South Sandwich Islands
    | SHN  -- ^ Saint Helena, Ascension and Tristan da Cunha
    | SJM  -- ^ Svalbard and Jan Mayen
    | SLB  -- ^ Solomon Islands
    | SLE  -- ^ Sierra Leone
    | SLO  -- ^ 'IndeterminateReservation': Slovenia (road vehicles)
    | SLV  -- ^ El Salvador
    | SME  -- ^ 'IndeterminateReservation': Suriname (road vehicles)
    | SMR  -- ^ San Marino
    | SOM  -- ^ Somalia
    | SPM  -- ^ Saint Pierre and Miquelon
    | SRB  -- ^ Serbia
    | SSD  -- ^ South Sudan
    | STP  -- ^ Sao Tome and Principe
    | SUN  -- ^ 'ExceptionalReservation': USSR (the)
    | SUR  -- ^ Suriname
    | SVK  -- ^ Slovakia
    | SVN  -- ^ Slovenia
    | SWE  -- ^ Sweden
    | SWZ  -- ^ Swaziland
    | SXM  -- ^ Sint Maarten (Dutch part)
    | SYC  -- ^ Seychelles
    | SYR  -- ^ Syrian Arab Republic

    | TAA  -- ^ 'ExceptionalReservation': Tristan da Cunha
    | TCA  -- ^ Turks and Caicos Islands (the)
    | TCD  -- ^ Chad
    | TGO  -- ^ Togo
    | THA  -- ^ Thailand
    | TJK  -- ^ Tajikistan
    | TKL  -- ^ Tokelau
    | TKM  -- ^ Turkmenistan
    | TLS  -- ^ Timor-Leste
    | TMN  -- ^ 'IndeterminateReservation': Turkmenistan (road vehicles)
    | TMP  -- ^ 'TransitionalReservation': East Timor
    | TON  -- ^ Tonga
    | TTO  -- ^ Trinidad and Tobago
    | TUN  -- ^ Tunisia
    | TUR  -- ^ Turkey
    | TUV  -- ^ Tuvalu
    | TWN  -- ^ Taiwan (Province of China)
    | TZA  -- ^ Tanzania, United Republic of

    | UGA  -- ^ Uganda
    | UKR  -- ^ Ukraine
    | UMI  -- ^ United States Minor Outlying Islands (the)
    | URY  -- ^ Uruguay
    | USA  -- ^ United States of America (the)
    | UZB  -- ^ Uzbekistan

    | VAT  -- ^ Holy See (the)
    | VCT  -- ^ Saint Vincent and the Grenadines
    | VEN  -- ^ Venezuela (Bolivarian Republic of)
    | VGB  -- ^ Virgin Islands (British)
    | VIR  -- ^ Virgin Islands (U.S.)
    | VNM  -- ^ Viet Nam
    | VUT  -- ^ Vanuatu

    | WAG  -- ^ 'IndeterminateReservation': Gambia (road vehicles)
    | WAL  -- ^ 'IndeterminateReservation': Sierra Leone (road vehicles)
    | WAN  -- ^ 'IndeterminateReservation': Nigeria (road vehicles)
    | WLF  -- ^ Wallis and Futuna
    | WSM  -- ^ Samoa

    | YEM  -- ^ Yemen
    | YUG  -- ^ 'TransitionalReservation': Yugoslavia

    | ZAF  -- ^ South Africa
    | ZAR  -- ^ 'TransitionalReservation': Zaire
    | ZMB  -- ^ Zambia
    | ZRE  -- ^ 'IndeterminateReservation': Zaire (road vehicles)
    | ZWE  -- ^ Zimbabwe
  deriving ( Eq, Show, Read, Enum, Bounded )
-- | Convert the country code into a unique 'Int' value.
instance H.Hashable Alpha3 where
    hashWithSalt = H.hashUsing fromEnum

-- | The stability of any particular country code.
codeStatus :: Alpha3 -> Status
codeStatus ADN = IndeterminateReservation
codeStatus ANT = TransitionalReservation
codeStatus ASC = ExceptionalReservation
codeStatus BDS = IndeterminateReservation
codeStatus BRU = IndeterminateReservation
codeStatus BUR = TransitionalReservation
codeStatus BYS = TransitionalReservation
codeStatus CDN = IndeterminateReservation
codeStatus CPT = ExceptionalReservation
codeStatus CSK = TransitionalReservation
codeStatus DGA = ExceptionalReservation
codeStatus EAK = IndeterminateReservation
codeStatus EAT = IndeterminateReservation
codeStatus EAU = IndeterminateReservation
codeStatus EAZ = IndeterminateReservation
codeStatus FXX = ExceptionalReservation
codeStatus GBA = IndeterminateReservation
codeStatus GBG = IndeterminateReservation
codeStatus GBJ = IndeterminateReservation
codeStatus GBM = IndeterminateReservation
codeStatus GBZ = IndeterminateReservation
codeStatus GCA = IndeterminateReservation
codeStatus HKJ = IndeterminateReservation
codeStatus MAL = IndeterminateReservation
codeStatus NTZ = TransitionalReservation
codeStatus RCA = IndeterminateReservation
codeStatus RCB = IndeterminateReservation
codeStatus RCH = IndeterminateReservation
codeStatus RMM = IndeterminateReservation
codeStatus RNR = IndeterminateReservation
codeStatus ROK = IndeterminateReservation
codeStatus ROM = TransitionalReservation
codeStatus RSM = IndeterminateReservation
codeStatus RSR = IndeterminateReservation
codeStatus SCG = TransitionalReservation
codeStatus SLO = IndeterminateReservation
codeStatus SME = IndeterminateReservation
codeStatus SUN = ExceptionalReservation
codeStatus TAA = ExceptionalReservation
codeStatus TMN = IndeterminateReservation
codeStatus TMP = TransitionalReservation
codeStatus WAG = IndeterminateReservation
codeStatus WAL = IndeterminateReservation
codeStatus WAN = IndeterminateReservation
codeStatus YUG = TransitionalReservation
codeStatus ZAR = TransitionalReservation
codeStatus ZRE = IndeterminateReservation
codeStatus _ = Official

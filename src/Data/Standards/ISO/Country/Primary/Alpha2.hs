{- | Module      : Data.Standards.ISO.Country.Primary.Alpha2
 -   Description : The two-character country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Alpha2
    ( Alpha2 ( .. )
    , Status ( .. )
    , codeStatus
    ) where

import Data.Standards.ISO.Country.Primary.Common

import qualified Data.Hashable as H

{-# WARNING UK "Official country code is GB" #-}

{-# DEPRECATED AN "Divided into BQ, CW and SX, or use ISO 3166-3 code ANHH" #-}
{-# DEPRECATED BU "Use new code MM or ISO 3166-3 code BUMM" #-}
{-# DEPRECATED CS "Divided into ME and RS, or use ISO 3166-3 code CSXX; previous usage divided into CZ and SK, or use ISO 3166-3 code CSHH" #-}
{-# DEPRECATED NT "Divided between IQ and SA, or use ISO 3166-3 code NTHH" #-}
{-# DEPRECATED TP "Use new code TL or ISO 3166-3 code TPTL" #-}
{-# DEPRECATED YU "Use ISO 3166-3 code YUCS" #-}
{-# DEPRECATED ZR "Use new code CD or ISO 3166-3 code ZRCD" #-}

-- | The official, recommended codes for countries, translated into a type-safe
-- representation.  Unless otherwise specified, all codes are 'Official'.
data Alpha2
    = AC  -- ^ 'ExceptionalReservation': Ascension Island
    | AD  -- ^ Andorra
    | AE  -- ^ United Arab Emirates (the)
    | AF  -- ^ Afghanistan
    | AG  -- ^ Antigua and Barbuda
    | AI  -- ^ Anguilla
    | AL  -- ^ Albania
    | AM  -- ^ Armenia
    | AN  -- ^ 'TransitionalReservation': Netherlands Antilles
    | AO  -- ^ Angola
    | AQ  -- ^ Antarctica
    | AR  -- ^ Argentina
    | AS  -- ^ American Samoa
    | AT  -- ^ Austria
    | AU  -- ^ Australia
    | AW  -- ^ Aruba
    | AX  -- ^ Åland Islands
    | AZ  -- ^ Azerbaijan

    | BA  -- ^ Bosnia and Herzegovina
    | BB  -- ^ Barbados
    | BD  -- ^ Bangladesh
    | BE  -- ^ Belgium
    | BF  -- ^ Burkina Faso
    | BG  -- ^ Bulgaria
    | BH  -- ^ Bahrain
    | BI  -- ^ Burundi
    | BJ  -- ^ Benin
    | BL  -- ^ Saint Barthélemy
    | BM  -- ^ Bermuda
    | BN  -- ^ Brunei Darussalam
    | BO  -- ^ Bolivia (Plurinational State of)
    | BQ  -- ^ Bonaire, Sint Eustatius and Saba
    | BR  -- ^ Brazil
    | BS  -- ^ Bahamas (the)
    | BT  -- ^ Bhutan
    | BU  -- ^ 'TransitionalReservation': Burma
    | BV  -- ^ Bouvet Island
    | BW  -- ^ Botswana
    | BY  -- ^ Belarus
    | BZ  -- ^ Belize

    | CA  -- ^ Canada
    | CC  -- ^ Cocos (Keeling) Islands (the)
    | CD  -- ^ Congo (the Democratic Republic of the)
    | CF  -- ^ Central African Republic (the)
    | CG  -- ^ Congo (the)
    | CH  -- ^ Switzerland
    | CI  -- ^ Côte d'Ivoire
    | CK  -- ^ Cook Islands (the)
    | CL  -- ^ Chile
    | CM  -- ^ Cameroon
    | CN  -- ^ China
    | CO  -- ^ Colombia
    | CP  -- ^ 'ExceptionalReservation': Clipperton Island
    | CR  -- ^ Costa Rica
    | CS  -- ^ 'TransitionalReservation': Serbia and Montenegro; previously Czechoslovakia
    | CU  -- ^ Cuba
    | CV  -- ^ Cabo Verde
    | CW  -- ^ Curaçao
    | CX  -- ^ Christmas Island
    | CY  -- ^ Cyprus
    | CZ  -- ^ Czechia

    | DE  -- ^ Germany
    | DG  -- ^ 'ExceptionalReservation': Diego Garcia
    | DJ  -- ^ Djibouti
    | DK  -- ^ Denmark
    | DM  -- ^ Dominica
    | DO  -- ^ Dominican Republic (the)
    | DZ  -- ^ Algeria

    | EA  -- ^ 'ExceptionalReservation': Ceuta & Melilla
    | EC  -- ^ Ecuador
    | EE  -- ^ Estonia
    | EG  -- ^ Egypt
    | EH  -- ^ Western Sahara
    | ER  -- ^ Eritrea
    | ES  -- ^ Spain
    | ET  -- ^ Ethiopia
    | EU  -- ^ 'ExceptionalReservation': European Union (the)
    | EZ  -- ^ 'ExceptionalReservation': Eurozone (the)

    | FI  -- ^ Finland
    | FJ  -- ^ Fiji
    | FK  -- ^ Falkland Islands (the) [Malvinas]
    | FM  -- ^ Micronesia (Federated States of)
    | FO  -- ^ Faroe Islands (the)
    | FR  -- ^ France
    | FX  -- ^ 'ExceptionalReservation': France, Metropolitan

    | GA  -- ^ Gabon
    | GB  -- ^ United Kingdom of Great Britain and Northern Ireland (the)
    | GD  -- ^ Grenada
    | GE  -- ^ Georgia
    | GF  -- ^ French Guiana
    | GG  -- ^ Guernsey
    | GH  -- ^ Ghana
    | GI  -- ^ Gibraltar
    | GL  -- ^ Greenland
    | GM  -- ^ Gambia (the)
    | GN  -- ^ Guinea
    | GP  -- ^ Guadeloupe
    | GQ  -- ^ Equatorial Guinea
    | GR  -- ^ Greece
    | GS  -- ^ South Georgia and the South Sandwich Islands
    | GT  -- ^ Guatemala
    | GU  -- ^ Guam
    | GW  -- ^ Guinea-Bissau
    | GY  -- ^ Guyana

    | HK  -- ^ Hong Kong
    | HM  -- ^ Heard Island and McDonald Islands
    | HN  -- ^ Honduras
    | HR  -- ^ Croatia
    | HT  -- ^ Haiti
    | HU  -- ^ Hungary

    | IC  -- ^ 'ExceptionalReservation': Canary Islands (the)
    | ID  -- ^ Indonesia
    | IE  -- ^ Ireland
    | IL  -- ^ Israel
    | IM  -- ^ Isle of Man
    | IN  -- ^ India
    | IO  -- ^ British Indian Ocean Territory (the)
    | IQ  -- ^ Iraq
    | IR  -- ^ Iran (Islamic Republic of)
    | IS  -- ^ Iceland
    | IT  -- ^ Italy
    | JE  -- ^ Jersey
    | JM  -- ^ Jamaica
    | JO  -- ^ Jordan
    | JP  -- ^ Japan

    | KE  -- ^ Kenya
    | KG  -- ^ Kyrgyzstan
    | KH  -- ^ Cambodia
    | KI  -- ^ Kiribati
    | KM  -- ^ Comoros (the)
    | KN  -- ^ Saint Kitts and Nevis
    | KP  -- ^ Korea (the Democratic People's Republic of)
    | KR  -- ^ Korea (the Republic of)
    | KW  -- ^ Kuwait
    | KY  -- ^ Cayman Islands (the)
    | KZ  -- ^ Kazakhstan

    | LA  -- ^ Lao People's Democratic Republic (the)
    | LB  -- ^ Lebanon
    | LC  -- ^ Saint Lucia
    | LI  -- ^ Liechtenstein
    | LK  -- ^ Sri Lanka
    | LR  -- ^ Liberia
    | LS  -- ^ Lesotho
    | LT  -- ^ Lithuania
    | LU  -- ^ Luxembourg
    | LV  -- ^ Latvia
    | LY  -- ^ Libya

    | MA  -- ^ Morocco
    | MC  -- ^ Monaco
    | MD  -- ^ Moldova (the Republic of)
    | ME  -- ^ Montenegro
    | MF  -- ^ Saint Martin (French part)
    | MG  -- ^ Madagascar
    | MH  -- ^ Marshall Islands (the)
    | MK  -- ^ Macedonia (the former Yugoslav Republic of)
    | ML  -- ^ Mali
    | MM  -- ^ Myanmar
    | MN  -- ^ Mongolia
    | MO  -- ^ Macao
    | MP  -- ^ Northern Mariana Islands (the)
    | MQ  -- ^ Martinique
    | MR  -- ^ Mauritania
    | MS  -- ^ Montserrat
    | MT  -- ^ Malta
    | MU  -- ^ Mauritius
    | MV  -- ^ Maldives
    | MW  -- ^ Malawi
    | MX  -- ^ Mexico
    | MY  -- ^ Malaysia
    | MZ  -- ^ Mozambique

    | NA  -- ^ Namibia
    | NC  -- ^ New Caledonia
    | NE  -- ^ Niger (the)
    | NF  -- ^ Norfolk Island
    | NG  -- ^ Nigeria
    | NI  -- ^ Nicaragua
    | NL  -- ^ Netherlands (the)
    | NO  -- ^ Norway
    | NP  -- ^ Nepal
    | NR  -- ^ Nauru
    | NT  -- ^ 'TransitionalReservation': Saudi Arabian-Iraqi neutral zone (the)
    | NU  -- ^ Niue
    | NZ  -- ^ New Zealand

    | OM  -- ^ Oman

    | PA  -- ^ Panama
    | PE  -- ^ Peru
    | PF  -- ^ French Polynesia
    | PG  -- ^ Papua New Guinea
    | PH  -- ^ Philippines (the)
    | PK  -- ^ Pakistan
    | PL  -- ^ Poland
    | PM  -- ^ Saint Pierre and Miquelon
    | PN  -- ^ Pitcairn
    | PR  -- ^ Puerto Rico
    | PS  -- ^ Palestine, State of
    | PT  -- ^ Portugal
    | PW  -- ^ Palau
    | PY  -- ^ Paraguay

    | QA  -- ^ Qatar

    | RE  -- ^ Réunion
    | RO  -- ^ Romania
    | RS  -- ^ Serbia
    | RU  -- ^ Russian Federation (the)
    | RW  -- ^ Rwanda

    | SA  -- ^ Saudi Arabia
    | SB  -- ^ Solomon Islands
    | SC  -- ^ Seychelles
    | SD  -- ^ Sudan (the)
    | SE  -- ^ Sweden
    | SG  -- ^ Singapore
    | SH  -- ^ Saint Helena, Ascension and Tristan da Cunha
    | SI  -- ^ Slovenia
    | SJ  -- ^ Svalbard and Jan Mayen
    | SK  -- ^ Slovakia
    | SL  -- ^ Sierra Leone
    | SM  -- ^ San Marino
    | SN  -- ^ Senegal
    | SO  -- ^ Somalia
    | SR  -- ^ Suriname
    | SS  -- ^ South Sudan
    | ST  -- ^ Sao Tome and Principe
    | SU  -- ^ 'ExceptionalReservation': USSR (the)
    | SV  -- ^ El Salvador
    | SX  -- ^ Sint Maarten (Dutch part)
    | SY  -- ^ Syrian Arab Republic
    | SZ  -- ^ Swaziland

    | TA  -- ^ 'ExceptionalReservation': Tristan da Cunha
    | TC  -- ^ Turks and Caicos Islands (the)
    | TD  -- ^ Chad
    | TF  -- ^ French Southern Territories (the)
    | TG  -- ^ Togo
    | TH  -- ^ Thailand
    | TJ  -- ^ Tajikistan
    | TK  -- ^ Tokelau
    | TL  -- ^ Timor-Leste
    | TM  -- ^ Turkmenistan
    | TN  -- ^ Tunisia
    | TO  -- ^ Tonga
    | TR  -- ^ Turkey
    | TP  -- ^ 'TransitionalReservation': East Timor
    | TT  -- ^ Trinidad and Tobago
    | TV  -- ^ Tuvalu
    | TW  -- ^ Taiwan (Province of China)
    | TZ  -- ^ Tanzania, United Republic of

    | UA  -- ^ Ukraine
    | UG  -- ^ Uganda
    | UK  -- ^ 'ExceptionalReservation': United Kingdom (the)
    | UM  -- ^ United States Minor Outlying Islands (the)
    | UN  -- ^ 'ExceptionalReservation': United Nations (the)
    | US  -- ^ United States of America (the)
    | UY  -- ^ Uruguay
    | UZ  -- ^ Uzbekistan

    | VA  -- ^ Holy See (the)
    | VC  -- ^ Saint Vincent and the Grenadines
    | VE  -- ^ Venezuela (Bolivarian Republic of)
    | VG  -- ^ Virgin Islands (British)
    | VI  -- ^ Virgin Islands (U.S.)
    | VN  -- ^ Viet Nam
    | VU  -- ^ Vanuatu

    | WF  -- ^ Wallis and Futuna
    | WS  -- ^ Samoa

    | YE  -- ^ Yemen
    | YT  -- ^ Mayotte
    | YU  -- ^ 'TransitionalReservation': Yugoslavia

    | ZA  -- ^ South Africa
    | ZM  -- ^ Zambia
    | ZR  -- ^ 'TransitionalReservation': Zaire
    | ZW  -- ^ Zimbabwe
  deriving ( Eq, Show, Read, Enum, Bounded )
-- | Convert the country code into a unique 'Int' value.
instance H.Hashable Alpha2 where
    hashWithSalt = H.hashUsing fromEnum

-- | The stability of any particular country code.
codeStatus :: Alpha2 -> Status
codeStatus AC = ExceptionalReservation
codeStatus AN = TransitionalReservation
codeStatus BU = TransitionalReservation
codeStatus CP = ExceptionalReservation
codeStatus CS = TransitionalReservation
codeStatus DG = ExceptionalReservation
codeStatus EA = ExceptionalReservation
codeStatus EU = ExceptionalReservation
codeStatus EZ = ExceptionalReservation
codeStatus FX = ExceptionalReservation
codeStatus IC = ExceptionalReservation
codeStatus NT = TransitionalReservation
codeStatus SU = ExceptionalReservation
codeStatus TA = ExceptionalReservation
codeStatus TP = TransitionalReservation
codeStatus UK = ExceptionalReservation
codeStatus UN = ExceptionalReservation
codeStatus YU = TransitionalReservation
codeStatus ZR = TransitionalReservation
codeStatus _ = Official

{- | Module      : Data.Standards.ISO.Country.Primary.Alpha2
 -   Description : The two-character country codes described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : provisional
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Alpha2
    ( Alpha2 ( .. )
    , Status ( .. )
    , codeStatus
    ) where

import Data.Standards.ISO.Country.Primary.Types

import qualified Data.Hashable as H

{-# WARNING UK "Official country code is GB" #-}

{-# DEPRECATED AN "Divided into BQ, CW and SX, or use ISO 3166-3 code ANHH" #-}
{-# DEPRECATED BU "Use new code MM or ISO 3166-3 code BUMM" #-}
{-# DEPRECATED CS "Divided into ME and RS, or use ISO 3166-3 code CSXX; previous usage divided into CZ and SK, or use ISO 3166-3 code CSHH" #-}
{-# DEPRECATED NT "Divided between IQ and SA, or use ISO 3166-3 code NTHH" #-}
{-# DEPRECATED TP "Use new code TL or ISO 3166-3 code TPTL" #-}
{-# DEPRECATED YU "Use ISO 3166-3 code YUCS" #-}
{-# DEPRECATED ZR "Use new code CD or ISO 3166-3 code ZRCD" #-}

{-# DEPRECATED DY "Use official code BJ" #-}
{-# DEPRECATED EW "Use official code EE" #-}
{-# DEPRECATED FL "Use official code LI" #-}
{-# DEPRECATED JA "Use official code JM" #-}
{-# DEPRECATED LF "Use code for entire country LY" #-}
{-# DEPRECATED PI "Use official code PH" #-}
{-# DEPRECATED RA "Use official code AR" #-}
{-# DEPRECATED RB "Use official code BO or BW, respectively" #-}
{-# DEPRECATED RC "Use official code CN" #-}
{-# DEPRECATED RH "Use official code HT" #-}
{-# DEPRECATED RI "Use official code ID" #-}
{-# DEPRECATED RM "Use official code MG" #-}
{-# DEPRECATED RN "Use official code NE" #-}
{-# DEPRECATED RP "Use official code PH" #-}
{-# DEPRECATED SF "Use official code FI" #-}
{-# DEPRECATED WG "Use official code GD" #-}
{-# DEPRECATED WL "Use official code LC" #-}
{-# DEPRECATED WV "Use official code VC" #-}
{-# DEPRECATED YV "Use official code VE" #-}

{-# WARNING AP,BX,EF,EM,EP,EV,GC,IB,OA,WO "Not an official ISO 3166-1 code" #-}

{-# DEPRECATED CT "Use new code KI or ISO 3166-3 code CTKI" #-}
{-# DEPRECATED DD "Use new code DE or ISO 3166-3 code DDDE" #-}
{-# DEPRECATED FQ "Divided into TF and part of AQ, or use ISO 3166-3 code FQHH" #-}
{-# DEPRECATED HV "Use new code BF or ISO 3166-3 code HVBF" #-}
{-# DEPRECATED JT "Use new code UM or ISO 3166-3 code JTUM" #-}
{-# DEPRECATED MI "Use new code UM or ISO 3166-3 code MIUM" #-}
{-# DEPRECATED NH "Use new code VU or ISO 3166-3 code NHVU" #-}
{-# DEPRECATED NQ "Use new code AQ or ISO 3166-3 code NQAQ" #-}
{-# DEPRECATED PC "Divided into FM, MH, MP and PW, or use ISO 3166-3 code PCHH" #-}
{-# DEPRECATED PU "Use new code UM or ISO 3166-3 code PUUM" #-}
{-# DEPRECATED PZ "Use new code PA or ISO 3166-3 code PZPA" #-}
{-# DEPRECATED VD "Use new code VN or ISO 3166-3 code VDVN" #-}
{-# DEPRECATED WK "Use new code UM or ISO 3166-3 code WKUM" #-}
{-# DEPRECATED YD "Use new code YE or ISO 3166-3 code YDYE" #-}

-- | The official, recommended codes for countries, translated into a type-safe
-- representation.  Unless otherwise specified, all codes are 'Official'.
data Alpha2
    = AC  -- ^ 'ExceptionalReservation': Ascension Island
    | AD  -- ^ Andorra
    | AE  -- ^ United Arab Emirates (the)
    | AF  -- ^ Afghanistan
    | AG  -- ^ Antigua and Barbuda
    | AI  -- ^ Anguilla; previously French Afar and Issas
    | AL  -- ^ Albania
    | AM  -- ^ Armenia
    | AN  -- ^ 'TransitionalReservation': Netherlands Antilles
    | AO  -- ^ Angola
    | AP  -- ^ 'NotInUse': African Regional Industrial Property Organization
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
    | BQ  -- ^ Bonaire, Sint Eustatius and Saba; previously British Antarctic Territory (the)
    | BR  -- ^ Brazil
    | BS  -- ^ Bahamas (the)
    | BT  -- ^ Bhutan
    | BU  -- ^ 'TransitionalReservation': Burma
    | BV  -- ^ Bouvet Island
    | BW  -- ^ Botswana
    | BX  -- ^ 'NotInUse': Benelux Trademarks and Design Offices
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
    | CT  -- ^ 'Withdrawn': Canton and Enderbury Islands (the)
    | CU  -- ^ Cuba
    | CV  -- ^ Cabo Verde
    | CW  -- ^ Curaçao
    | CX  -- ^ Christmas Island
    | CY  -- ^ Cyprus
    | CZ  -- ^ Czechia

    | DD  -- ^ 'Withdrawn': German Democratic Republic (the)
    | DE  -- ^ Germany
    | DG  -- ^ 'ExceptionalReservation': Diego Garcia
    | DJ  -- ^ Djibouti
    | DK  -- ^ Denmark
    | DM  -- ^ Dominica
    | DO  -- ^ Dominican Republic (the)
    | DY  -- ^ 'IndeterminateReservation': Benin (road vehicles); previously Dahomey
    | DZ  -- ^ Algeria

    | EA  -- ^ 'ExceptionalReservation': Ceuta & Melilla
    | EC  -- ^ Ecuador
    | EE  -- ^ Estonia
    | EF  -- ^ 'NotInUse': European Community Patent Convention
    | EM  -- ^ 'NotInUse': European Trademark Office
    | EP  -- ^ 'NotInUse': European Patent Organization
    | EV  -- ^ 'NotInUse': Eurasian Patent Organization
    | EW  -- ^ 'IndeterminateReservation': Estonia (road vehicles)
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
    | FL  -- ^ 'IndeterminateReservation': Liechtenstein (road vehicles)
    | FM  -- ^ Micronesia (Federated States of)
    | FO  -- ^ Faroe Islands (the)
    | FQ  -- ^ 'Withdrawn': French Southern and Antarctic Territories (the)
    | FR  -- ^ France
    | FX  -- ^ 'ExceptionalReservation': France, Metropolitan

    | GA  -- ^ Gabon
    | GB  -- ^ United Kingdom of Great Britain and Northern Ireland (the)
    | GC  -- ^ 'NotInUse': Patent Office of the Cooperation Council for the Arab States of the Gulf
    | GD  -- ^ Grenada
    | GE  -- ^ Georgia; previously Gilbert and Ellice Islands (the)
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
    | HV  -- ^ 'Withdrawn': Upper Volta

    | IB  -- ^ 'NotInUse': World Intellectual Property Organization, International Bureau
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

    | JA  -- ^ 'IndeterminateReservation': Jamaica (road vehicles)
    | JE  -- ^ Jersey
    | JM  -- ^ Jamaica
    | JO  -- ^ Jordan
    | JP  -- ^ Japan
    | JT  -- ^ 'Withdrawn': Johnston Island

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
    | LF  -- ^ 'IndeterminateReservation': Lybia Fezzan (road vehicles)
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
    | MI  -- ^ 'Withdrawn': Midway Islands (the)
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
    | NH  -- ^ 'Withdrawn': New Hebrides
    | NI  -- ^ Nicaragua
    | NL  -- ^ Netherlands (the)
    | NO  -- ^ Norway
    | NP  -- ^ Nepal
    | NQ  -- ^ 'Withdrawn': Dronning Maud Land
    | NR  -- ^ Nauru
    | NT  -- ^ 'TransitionalReservation': Saudi Arabian-Iraqi neutral zone (the)
    | NU  -- ^ Niue
    | NZ  -- ^ New Zealand

    | OA  -- ^ 'NotInUse': African Intellectual Property Organization
    | OM  -- ^ Oman

    | PA  -- ^ Panama
    | PC  -- ^ 'Withdrawn': Pacific Islands (Trust Territory of the)
    | PE  -- ^ Peru
    | PF  -- ^ French Polynesia
    | PG  -- ^ Papua New Guinea
    | PH  -- ^ Philippines (the)
    | PI  -- ^ 'IndeterminateReservation': Philippines (road vehicles)
    | PK  -- ^ Pakistan
    | PL  -- ^ Poland
    | PM  -- ^ Saint Pierre and Miquelon
    | PN  -- ^ Pitcairn
    | PR  -- ^ Puerto Rico
    | PS  -- ^ Palestine, State of
    | PT  -- ^ Portugal
    | PU  -- ^ 'Withdrawn': United States Miscellaneous Pacific Islands (the)
    | PW  -- ^ Palau
    | PY  -- ^ Paraguay
    | PZ  -- ^ 'Withdrawn': Panama Canal Zone (the)

    | QA  -- ^ Qatar

    | RA  -- ^ 'IndeterminateReservation': Argentina (road vehicles)
    | RB  -- ^ 'IndeterminateReservation': Bolivia & Botswana (road vehicles)
    | RC  -- ^ 'IndeterminateReservation': China (road vehicles)
    | RE  -- ^ Réunion
    | RH  -- ^ 'IndeterminateReservation': Haiti (road vehicles); previously Southern Rhodesia
    | RI  -- ^ 'IndeterminateReservation': Indonesia (road vehicles)
    | RM  -- ^ 'IndeterminateReservation': Madagascar (road vehicles)
    | RN  -- ^ 'IndeterminateReservation': Niger (road vehicles)
    | RO  -- ^ Romania
    | RP  -- ^ 'IndeterminateReservation': Philippines (road vehicles)
    | RS  -- ^ Serbia
    | RU  -- ^ Russian Federation (the)
    | RW  -- ^ Rwanda

    | SA  -- ^ Saudi Arabia
    | SB  -- ^ Solomon Islands
    | SC  -- ^ Seychelles
    | SD  -- ^ Sudan (the)
    | SE  -- ^ Sweden
    | SF  -- ^ 'IndeterminateReservation': Finland
    | SG  -- ^ Singapore
    | SH  -- ^ Saint Helena, Ascension and Tristan da Cunha
    | SI  -- ^ Slovenia
    | SJ  -- ^ Svalbard and Jan Mayen
    | SK  -- ^ Slovakia; previously Sikkim
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
    | SZ  -- ^ Eswatini

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
    | VD  -- ^ 'Withdrawn': Viet Nam (Democratic Republic of)
    | VE  -- ^ Venezuela (Bolivarian Republic of)
    | VG  -- ^ Virgin Islands (British)
    | VI  -- ^ Virgin Islands (U.S.)
    | VN  -- ^ Viet Nam
    | VU  -- ^ Vanuatu

    | WF  -- ^ Wallis and Futuna
    | WG  -- ^ 'IndeterminateReservation': Grenada (road vehicles)
    | WK  -- ^ 'Withdrawn': Wake Island
    | WL  -- ^ 'IndeterminateReservation': Saint Lucia (road vehicles)
    | WO  -- ^ 'NotInUse': World Intellectual Property Organization
    | WS  -- ^ Samoa
    | WV  -- ^ 'IndeterminateReservation': Saint Vincent (road vehicles)

    | YD  -- ^ 'Withdrawn': Yemen (Democratic)
    | YE  -- ^ Yemen
    | YT  -- ^ Mayotte
    | YU  -- ^ 'TransitionalReservation': Yugoslavia
    | YV  -- ^ 'IndeterminateReservation': Venezuela (road vehicles)

    | ZA  -- ^ South Africa
    | ZM  -- ^ Zambia
    | ZR  -- ^ 'TransitionalReservation': Zaire
    | ZW  -- ^ Zimbabwe
  deriving ( Eq, Show, Read, Ord, Enum, Bounded )

instance H.Hashable Alpha2 where
    hashWithSalt = H.hashUsing fromEnum

-- | The stability of any particular country code.
codeStatus :: Alpha2 -> Status
codeStatus AC = ExceptionalReservation
codeStatus AN = TransitionalReservation
codeStatus AP = NotInUse
codeStatus BU = TransitionalReservation
codeStatus BX = NotInUse
codeStatus CP = ExceptionalReservation
codeStatus CS = TransitionalReservation
codeStatus CT = Withdrawn
codeStatus DD = Withdrawn
codeStatus DG = ExceptionalReservation
codeStatus DY = IndeterminateReservation
codeStatus EA = ExceptionalReservation
codeStatus EF = NotInUse
codeStatus EM = NotInUse
codeStatus EP = NotInUse
codeStatus EU = ExceptionalReservation
codeStatus EV = NotInUse
codeStatus EW = IndeterminateReservation
codeStatus EZ = ExceptionalReservation
codeStatus FL = IndeterminateReservation
codeStatus FQ = Withdrawn
codeStatus FX = ExceptionalReservation
codeStatus GC = NotInUse
codeStatus HV = Withdrawn
codeStatus IB = NotInUse
codeStatus IC = ExceptionalReservation
codeStatus JA = IndeterminateReservation
codeStatus JT = Withdrawn
codeStatus LF = IndeterminateReservation
codeStatus NT = TransitionalReservation
codeStatus MI = Withdrawn
codeStatus NH = Withdrawn
codeStatus NQ = Withdrawn
codeStatus OA = NotInUse
codeStatus PC = Withdrawn
codeStatus PI = IndeterminateReservation
codeStatus PU = Withdrawn
codeStatus PZ = Withdrawn
codeStatus RA = IndeterminateReservation
codeStatus RB = IndeterminateReservation
codeStatus RC = IndeterminateReservation
codeStatus RH = IndeterminateReservation
codeStatus RI = IndeterminateReservation
codeStatus RM = IndeterminateReservation
codeStatus RN = IndeterminateReservation
codeStatus RP = IndeterminateReservation
codeStatus SF = IndeterminateReservation
codeStatus SU = ExceptionalReservation
codeStatus TA = ExceptionalReservation
codeStatus TP = TransitionalReservation
codeStatus UK = ExceptionalReservation
codeStatus UN = ExceptionalReservation
codeStatus VD = Withdrawn
codeStatus WG = IndeterminateReservation
codeStatus WK = Withdrawn
codeStatus WL = IndeterminateReservation
codeStatus WO = NotInUse
codeStatus WV = IndeterminateReservation
codeStatus YD = Withdrawn
codeStatus YU = TransitionalReservation
codeStatus YV = IndeterminateReservation
codeStatus ZR = TransitionalReservation
codeStatus _ = Official

{- | Module      : Data.Standards.ISO.Country.Primary.Translation
 -   Description : Convert between various code formats described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : unstable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Translation
    ( alpha2ToAlpha3
    , alpha2ToNumeric
    , alpha3ToAlpha2
    , alpha3ToNumeric
    , numericToAlpha2
    , numericToAlpha3
    ) where

import qualified Data.Standards.ISO.Country.Primary.Alpha2 as A2
import qualified Data.Standards.ISO.Country.Primary.Alpha3 as A3
import qualified Data.Standards.ISO.Country.Primary.Numeric as N

import qualified Data.HashMap.Lazy as M

-- | Convert an 'Alpha2' country code to an 'Alpha3'.
alpha2ToAlpha3 :: A2.Alpha2 -> Maybe A3.Alpha3
alpha2ToAlpha3 = flip M.lookup a2a3

-- | Convert an 'Alpha2' country code to a 'Numeric'.
alpha2ToNumeric :: A2.Alpha2 -> Maybe N.Numeric
alpha2ToNumeric = flip M.lookup a2n

-- | Convert an 'Alpha3' country code to an 'Alpha2'.
alpha3ToAlpha2 :: A3.Alpha3 -> Maybe A2.Alpha2
alpha3ToAlpha2 = flip M.lookup a3a2

-- | Convert an 'Alpha3' country code to a 'Numeric'.
alpha3ToNumeric :: A3.Alpha3 -> Maybe N.Numeric
alpha3ToNumeric = flip M.lookup a3n

-- | Convert a 'Numeric' country code to an 'Alpha2'.
numericToAlpha2 :: N.Numeric -> Maybe A2.Alpha2
numericToAlpha2 = flip M.lookup na2

-- | Convert a 'Numeric' country code to an 'Alpha3'.
numericToAlpha3 :: N.Numeric -> Maybe A3.Alpha3
numericToAlpha3 = flip M.lookup na3

-- | The mappings between countries which have a unique code in all systems of
-- representation.
tuples :: [(A2.Alpha2, A3.Alpha3, N.Numeric)]
tuples =
    [ (A2.AD, A3.AND, N.C020)  -- Andorra
    , (A2.AE, A3.ARE, N.C784)  -- United Arab Emirates (the)
    , (A2.AF, A3.AFG, N.C004)  -- Afghanistan
    , (A2.AG, A3.ATG, N.C028)  -- Antigua and Barbuda
    , (A2.AI, A3.AIA, N.C660)  -- Anguilla
    , (A2.AL, A3.ALB, N.C008)  -- Albania
    , (A2.AM, A3.ARM, N.C051)  -- Armenia
    , (A2.AO, A3.AGO, N.C024)  -- Angola
    , (A2.AQ, A3.ATA, N.C010)  -- Antarctica
    , (A2.AR, A3.ARG, N.C032)  -- Argentina
    , (A2.AS, A3.ASM, N.C016)  -- American Samoa
    , (A2.AT, A3.AUT, N.C040)  -- Austria
    , (A2.AU, A3.AUS, N.C036)  -- Australia
    , (A2.AW, A3.ABW, N.C533)  -- Aruba
    , (A2.AX, A3.ALA, N.C248)  -- Åland Islands
    , (A2.AZ, A3.AZE, N.C031)  -- Azerbaijan

    , (A2.BA, A3.BIH, N.C070)  -- Bosnia and Herzegovina
    , (A2.BB, A3.BRB, N.C052)  -- Barbados
    , (A2.BD, A3.BGD, N.C050)  -- Bangladesh
    , (A2.BE, A3.BEL, N.C056)  -- Belgium
    , (A2.BF, A3.BFA, N.C854)  -- Burkina Faso
    , (A2.BG, A3.BGR, N.C100)  -- Bulgaria
    , (A2.BH, A3.BHR, N.C048)  -- Bahrain
    , (A2.BI, A3.BDI, N.C108)  -- Burundi
    , (A2.BJ, A3.BEN, N.C204)  -- Benin
    , (A2.BL, A3.BLM, N.C652)  -- Saint Barthélemy
    , (A2.BM, A3.BMU, N.C060)  -- Bermuda
    , (A2.BN, A3.BRN, N.C096)  -- Brunei Darussalam
    , (A2.BO, A3.BOL, N.C068)  -- Bolivia (Plurinational State of)
    , (A2.BQ, A3.BES, N.C535)  -- Bonaire, Sint Eustatius and Saba
    , (A2.BR, A3.BRA, N.C076)  -- Brazil
    , (A2.BS, A3.BHS, N.C044)  -- Bahamas (the)
    , (A2.BT, A3.BTN, N.C064)  -- Bhutan
    , (A2.BV, A3.BVT, N.C074)  -- Bouvet Island
    , (A2.BW, A3.BWA, N.C072)  -- Botswana
    , (A2.BY, A3.BLR, N.C112)  -- Belarus
    , (A2.BZ, A3.BLZ, N.C084)  -- Belize

    , (A2.CA, A3.CAN, N.C124)  -- Canada
    , (A2.CC, A3.CCK, N.C166)  -- Cocos (Keeling) Islands (the)
    , (A2.CD, A3.COD, N.C180)  -- Congo (the Democratic Republic of the)
    , (A2.CF, A3.CAF, N.C140)  -- Central African Republic (the)
    , (A2.CG, A3.COG, N.C178)  -- Congo (the)
    , (A2.CH, A3.CHE, N.C756)  -- Switzerland
    , (A2.CI, A3.CIV, N.C384)  -- Côte d'Ivoire
    , (A2.CK, A3.COK, N.C184)  -- Cook Islands (the)
    , (A2.CL, A3.CHL, N.C152)  -- Chile
    , (A2.CM, A3.CMR, N.C120)  -- Cameroon
    , (A2.CN, A3.CHN, N.C156)  -- China
    , (A2.CO, A3.COL, N.C170)  -- Colombia
    , (A2.CR, A3.CRI, N.C188)  -- Costa Rica
    , (A2.CU, A3.CUB, N.C192)  -- Cuba
    , (A2.CV, A3.CPV, N.C132)  -- Cabo Verde
    , (A2.CW, A3.CUW, N.C531)  -- Curaçao
    , (A2.CX, A3.CXR, N.C162)  -- Christmas Island
    , (A2.CY, A3.CYP, N.C196)  -- Cyprus
    , (A2.CZ, A3.CZE, N.C203)  -- Czechia

    , (A2.DE, A3.DEU, N.C276)  -- Germany
    , (A2.DJ, A3.DJI, N.C262)  -- Djibouti
    , (A2.DK, A3.DNK, N.C208)  -- Denmark
    , (A2.DM, A3.DMA, N.C212)  -- Dominica
    , (A2.DO, A3.DOM, N.C214)  -- Dominican Republic (the)
    , (A2.DZ, A3.DZA, N.C012)  -- Algeria

    , (A2.EC, A3.ECU, N.C218)  -- Ecuador
    , (A2.EE, A3.EST, N.C233)  -- Estonia
    , (A2.EG, A3.EGY, N.C818)  -- Egypt
    , (A2.EH, A3.ESH, N.C732)  -- Western Sahara
    , (A2.ER, A3.ERI, N.C232)  -- Eritrea
    , (A2.ES, A3.ESP, N.C724)  -- Spain
    , (A2.ET, A3.ETH, N.C231)  -- Ethiopia

    , (A2.FI, A3.FIN, N.C246)  -- Finland
    , (A2.FJ, A3.FJI, N.C242)  -- Fiji
    , (A2.FK, A3.FLK, N.C238)  -- Falkland Islands (the) [Malvinas]
    , (A2.FM, A3.FSM, N.C583)  -- Micronesia (Federated States of)
    , (A2.FO, A3.FRO, N.C234)  -- Faroe Islands (the)
    , (A2.FR, A3.FRA, N.C250)  -- France
    , (A2.FX, A3.FXX, N.C249)  -- France, Metropolitan

    , (A2.GA, A3.GAB, N.C266)  -- Gabon
    , (A2.GB, A3.GBR, N.C826)  -- United Kingdom of Great Britain and Northern Ireland (the)
    , (A2.GD, A3.GRD, N.C308)  -- Grenada
    , (A2.GE, A3.GEO, N.C268)  -- Georgia
    , (A2.GF, A3.GUF, N.C254)  -- French Guiana
    , (A2.GG, A3.GGY, N.C831)  -- Guernsey
    , (A2.GH, A3.GHA, N.C288)  -- Ghana
    , (A2.GI, A3.GIB, N.C292)  -- Gibraltar
    , (A2.GL, A3.GRL, N.C304)  -- Greenland
    , (A2.GM, A3.GMB, N.C270)  -- Gambia (the)
    , (A2.GN, A3.GIN, N.C324)  -- Guinea
    , (A2.GP, A3.GLP, N.C312)  -- Guadeloupe
    , (A2.GQ, A3.GNQ, N.C226)  -- Equatorial Guinea
    , (A2.GR, A3.GRC, N.C300)  -- Greece
    , (A2.GS, A3.SGS, N.C239)  -- South Georgia and the South Sandwich Islands
    , (A2.GT, A3.GTM, N.C320)  -- Guatemala
    , (A2.GU, A3.GUM, N.C316)  -- Guam
    , (A2.GW, A3.GNB, N.C624)  -- Guinea-Bissau
    , (A2.GY, A3.GUY, N.C328)  -- Guyana

    , (A2.HK, A3.HKG, N.C344)  -- Hong Kong
    , (A2.HM, A3.HMD, N.C334)  -- Heard Island and McDonald Islands
    , (A2.HN, A3.HND, N.C340)  -- Honduras
    , (A2.HR, A3.HRV, N.C191)  -- Croatia
    , (A2.HT, A3.HTI, N.C332)  -- Haiti
    , (A2.HU, A3.HUN, N.C348)  -- Hungary

    , (A2.ID, A3.IDN, N.C360)  -- Indonesia
    , (A2.IE, A3.IRL, N.C372)  -- Ireland
    , (A2.IL, A3.ISR, N.C376)  -- Israel
    , (A2.IM, A3.IMN, N.C833)  -- Isle of Man
    , (A2.IN, A3.IND, N.C356)  -- India
    , (A2.IO, A3.IOT, N.C086)  -- British Indian Ocean Territory (the)
    , (A2.IQ, A3.IRQ, N.C368)  -- Iraq
    , (A2.IR, A3.IRN, N.C364)  -- Iran (Islamic Republic of)
    , (A2.IS, A3.ISL, N.C352)  -- Iceland
    , (A2.IT, A3.ITA, N.C380)  -- Italy

    , (A2.JE, A3.JEY, N.C832)  -- Jersey
    , (A2.JM, A3.JAM, N.C388)  -- Jamaica
    , (A2.JO, A3.JOR, N.C400)  -- Jordan
    , (A2.JP, A3.JPN, N.C392)  -- Japan

    , (A2.KE, A3.KEN, N.C404)  -- Kenya
    , (A2.KG, A3.KGZ, N.C417)  -- Kyrgyzstan
    , (A2.KH, A3.KHM, N.C116)  -- Cambodia
    , (A2.KI, A3.KIR, N.C296)  -- Kiribati
    , (A2.KM, A3.COM, N.C174)  -- Comoros (the)
    , (A2.KN, A3.KNA, N.C659)  -- Saint Kitts and Nevis
    , (A2.KP, A3.PRK, N.C408)  -- Korea (the Democratic People's Republic of)
    , (A2.KR, A3.KOR, N.C410)  -- Korea (the Republic of)
    , (A2.KW, A3.KWT, N.C414)  -- Kuwait
    , (A2.KY, A3.CYM, N.C136)  -- Cayman Islands (the)
    , (A2.KZ, A3.KAZ, N.C398)  -- Kazakhstan

    , (A2.LA, A3.LAO, N.C418)  -- Lao People's Democratic Republic (the)
    , (A2.LB, A3.LBN, N.C422)  -- Lebanon
    , (A2.LC, A3.LCA, N.C662)  -- Saint Lucia
    , (A2.LI, A3.LIE, N.C438)  -- Liechtenstein
    , (A2.LK, A3.LKA, N.C144)  -- Sri Lanka
    , (A2.LR, A3.LBR, N.C430)  -- Liberia
    , (A2.LS, A3.LSO, N.C426)  -- Lesotho
    , (A2.LT, A3.LTU, N.C440)  -- Lithuania
    , (A2.LU, A3.LUX, N.C442)  -- Luxembourg
    , (A2.LV, A3.LVA, N.C428)  -- Latvia
    , (A2.LY, A3.LBY, N.C434)  -- Libya

    , (A2.MA, A3.MAR, N.C504)  -- Morocco
    , (A2.MC, A3.MCO, N.C492)  -- Monaco
    , (A2.MD, A3.MDA, N.C498)  -- Moldova (the Republic of)
    , (A2.ME, A3.MNE, N.C499)  -- Montenegro
    , (A2.MF, A3.MAF, N.C663)  -- Saint Martin (French part)
    , (A2.MG, A3.MDG, N.C450)  -- Madagascar
    , (A2.MH, A3.MHL, N.C584)  -- Marshall Islands (the)
    , (A2.MK, A3.MKD, N.C807)  -- Macedonia (the former Yugoslav Republic of)
    , (A2.ML, A3.MLI, N.C466)  -- Mali
    , (A2.MM, A3.MMR, N.C104)  -- Myanmar
    , (A2.MN, A3.MNG, N.C496)  -- Mongolia
    , (A2.MO, A3.MAC, N.C446)  -- Macao
    , (A2.MP, A3.MNP, N.C580)  -- Northern Mariana Islands (the)
    , (A2.MQ, A3.MTQ, N.C474)  -- Martinique
    , (A2.MR, A3.MRT, N.C478)  -- Mauritania
    , (A2.MS, A3.MSR, N.C500)  -- Montserrat
    , (A2.MT, A3.MLT, N.C470)  -- Malta
    , (A2.MU, A3.MUS, N.C480)  -- Mauritius
    , (A2.MV, A3.MDV, N.C462)  -- Maldives
    , (A2.MW, A3.MWI, N.C454)  -- Malawi
    , (A2.MX, A3.MEX, N.C484)  -- Mexico
    , (A2.MY, A3.MYS, N.C458)  -- Malaysia
    , (A2.MZ, A3.MOZ, N.C508)  -- Mozambique

    , (A2.NA, A3.NAM, N.C516)  -- Namibia
    , (A2.NC, A3.NCL, N.C540)  -- New Caledonia
    , (A2.NE, A3.NER, N.C562)  -- Niger (the)
    , (A2.NF, A3.NFK, N.C574)  -- Norfolk Island
    , (A2.NG, A3.NGA, N.C566)  -- Nigeria
    , (A2.NI, A3.NIC, N.C558)  -- Nicaragua
    , (A2.NL, A3.NLD, N.C528)  -- Netherlands (the)
    , (A2.NO, A3.NOR, N.C578)  -- Norway
    , (A2.NP, A3.NPL, N.C524)  -- Nepal
    , (A2.NR, A3.NRU, N.C520)  -- Nauru
    , (A2.NU, A3.NIU, N.C570)  -- Niue
    , (A2.NZ, A3.NZL, N.C554)  -- New Zealand

    , (A2.OM, A3.OMN, N.C512)  -- Oman

    , (A2.PA, A3.PAN, N.C591)  -- Panama
    , (A2.PE, A3.PER, N.C604)  -- Peru
    , (A2.PF, A3.PYF, N.C258)  -- French Polynesia
    , (A2.PG, A3.PNG, N.C598)  -- Papua New Guinea
    , (A2.PH, A3.PHL, N.C608)  -- Philippines (the)
    , (A2.PK, A3.PAK, N.C586)  -- Pakistan
    , (A2.PL, A3.POL, N.C616)  -- Poland
    , (A2.PM, A3.SPM, N.C666)  -- Saint Pierre and Miquelon
    , (A2.PN, A3.PCN, N.C612)  -- Pitcairn
    , (A2.PR, A3.PRI, N.C630)  -- Puerto Rico
    , (A2.PS, A3.PSE, N.C275)  -- Palestine, State of
    , (A2.PT, A3.PRT, N.C620)  -- Portugal
    , (A2.PW, A3.PLW, N.C585)  -- Palau
    , (A2.PY, A3.PRY, N.C600)  -- Paraguay

    , (A2.QA, A3.QAT, N.C634)  -- Qatar

    , (A2.RE, A3.REU, N.C638)  -- Réunion
    , (A2.RO, A3.ROU, N.C642)  -- Romania
    , (A2.RS, A3.SRB, N.C688)  -- Serbia
    , (A2.RU, A3.RUS, N.C643)  -- Russian Federation (the)
    , (A2.RW, A3.RWA, N.C646)  -- Rwanda

    , (A2.SA, A3.SAU, N.C682)  -- Saudi Arabia
    , (A2.SB, A3.SLB, N.C090)  -- Solomon Islands
    , (A2.SC, A3.SYC, N.C690)  -- Seychelles
    , (A2.SD, A3.SDN, N.C729)  -- Sudan (the)
    , (A2.SE, A3.SWE, N.C752)  -- Sweden
    , (A2.SG, A3.SGP, N.C702)  -- Singapore
    , (A2.SH, A3.SHN, N.C654)  -- Saint Helena, Ascension and Tristan da Cunha
    , (A2.SI, A3.SVN, N.C705)  -- Slovenia
    , (A2.SJ, A3.SJM, N.C744)  -- Svalbard and Jan Mayen
    , (A2.SK, A3.SVK, N.C703)  -- Slovakia
    , (A2.SL, A3.SLE, N.C694)  -- Sierra Leone
    , (A2.SM, A3.SMR, N.C674)  -- San Marino
    , (A2.SN, A3.SEN, N.C686)  -- Senegal
    , (A2.SO, A3.SOM, N.C706)  -- Somalia
    , (A2.SR, A3.SUR, N.C740)  -- Suriname
    , (A2.SS, A3.SSD, N.C728)  -- South Sudan
    , (A2.ST, A3.STP, N.C678)  -- Sao Tome and Principe
    , (A2.SV, A3.SLV, N.C222)  -- El Salvador
    , (A2.SU, A3.SUN, N.C810)  -- USSR
    , (A2.SX, A3.SXM, N.C534)  -- Sint Maarten (Dutch part)
    , (A2.SY, A3.SYR, N.C760)  -- Syrian Arab Republic
    , (A2.SZ, A3.SWZ, N.C748)  -- Swaziland

    , (A2.TC, A3.TCA, N.C796)  -- Turks and Caicos Islands (the)
    , (A2.TD, A3.TCD, N.C148)  -- Chad
    , (A2.TF, A3.ATF, N.C260)  -- French Southern Territories (the)
    , (A2.TG, A3.TGO, N.C768)  -- Togo
    , (A2.TH, A3.THA, N.C764)  -- Thailand
    , (A2.TJ, A3.TJK, N.C762)  -- Tajikistan
    , (A2.TK, A3.TKL, N.C772)  -- Tokelau
    , (A2.TL, A3.TLS, N.C626)  -- Timor-Leste
    , (A2.TM, A3.TKM, N.C795)  -- Turkmenistan
    , (A2.TN, A3.TUN, N.C788)  -- Tunisia
    , (A2.TO, A3.TON, N.C776)  -- Tonga
    , (A2.TR, A3.TUR, N.C792)  -- Turkey
    , (A2.TT, A3.TTO, N.C780)  -- Trinidad and Tobago
    , (A2.TV, A3.TUV, N.C798)  -- Tuvalu
    , (A2.TW, A3.TWN, N.C158)  -- Taiwan (Province of China)
    , (A2.TZ, A3.TZA, N.C834)  -- Tanzania, United Republic of

    , (A2.UA, A3.UKR, N.C804)  -- Ukraine
    , (A2.UG, A3.UGA, N.C800)  -- Uganda
    , (A2.UM, A3.UMI, N.C581)  -- United States Minor Outlying Islands (the)
    , (A2.US, A3.USA, N.C840)  -- United States of America (the)
    , (A2.UY, A3.URY, N.C858)  -- Uruguay
    , (A2.UZ, A3.UZB, N.C860)  -- Uzbekistan

    , (A2.VA, A3.VAT, N.C336)  -- Holy See (the)
    , (A2.VC, A3.VCT, N.C670)  -- Saint Vincent and the Grenadines
    , (A2.VE, A3.VEN, N.C862)  -- Venezuela (Bolivarian Republic of)
    , (A2.VG, A3.VGB, N.C092)  -- Virgin Islands (British)
    , (A2.VI, A3.VIR, N.C850)  -- Virgin Islands (U.S.)
    , (A2.VN, A3.VNM, N.C704)  -- Viet Nam
    , (A2.VU, A3.VUT, N.C548)  -- Vanuatu

    , (A2.WF, A3.WLF, N.C876)  -- Wallis and Futuna
    , (A2.WS, A3.WSM, N.C882)  -- Samoa

    , (A2.YE, A3.YEM, N.C887)  -- Yemen
    , (A2.YT, A3.MYT, N.C175)  -- Mayotte

    , (A2.ZA, A3.ZAF, N.C710)  -- South Africa
    , (A2.ZM, A3.ZMB, N.C894)  -- Zambia
    , (A2.ZW, A3.ZWE, N.C716)  -- Zimbabwe
    ]

-- | Countries with codes in both the alpha-2 and alpha-3 representations, but
-- not the numeric.
pairA2A3 :: [(A2.Alpha2, A3.Alpha3)]
pairA2A3 =
    [ (A2.AC, A3.ASC)  -- Ascension Island
    , (A2.CP, A3.CPT)  -- Clipperton Island
    , (A2.DG, A3.DGA)  -- Diego Garcia
    , (A2.TA, A3.TAA)  -- Tristan da Cunha
    ]

-- | Cache 'Alpha2' -> 'Alpha3' conversions, and provide faster lookup.
a2a3 :: M.HashMap A2.Alpha2 A3.Alpha3
a2a3 = M.fromList $ pairA2A3 ++ map (\(a2, a3, _) -> (a2, a3)) tuples

-- | Cache 'Alpha2' -> 'Numeric' conversions, and provide faster lookup.
a2n :: M.HashMap A2.Alpha2 N.Numeric
a2n = M.fromList $ map (\(a2, _, n) -> (a2, n)) tuples

-- | Cache 'Alpha3' -> 'Alpha2' conversions, and provide faster lookup.
a3a2 :: M.HashMap A3.Alpha3 A2.Alpha2
a3a2 = M.fromList $ map (\(a2, a3, _) -> (a3, a2)) tuples

-- | Cache 'Alpha3' -> 'Numeric' conversions, and provide faster lookup.
a3n :: M.HashMap A3.Alpha3 N.Numeric
a3n = M.fromList $ map (\(_, a3, n) -> (a3, n)) tuples

-- | Cache 'Numeric' -> 'Alpha2' conversions, and provide faster lookup.
na2 :: M.HashMap N.Numeric A2.Alpha2
na2 = M.fromList $ map (\(a2, _, n) -> (n, a2)) tuples

-- | Cache 'Numeric' -> 'Alpha3' conversions, and provide faster lookup.
na3 :: M.HashMap N.Numeric A3.Alpha3
na3 = M.fromList $ map (\(_, a3, n) -> (n, a3)) tuples

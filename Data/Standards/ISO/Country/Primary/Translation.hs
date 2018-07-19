{-# OPTIONS -Wno-deprecations #-}

{- | Module      : Data.Standards.ISO.Country.Primary.Translation
 -   Description : Convert between various code formats described by ISO 3166-1.
 -   Copyright   : (c) 2018 Samuel May
 -   License     : MPL-2.0
 -   Maintainer  : ag.eitilt@gmail.com
 -   Stability   : stable
 -   Portability : portable
 -}
module Data.Standards.ISO.Country.Primary.Translation
    ( Country  -- without constructors
    , CountryCode ( .. )
      -- * Deprecated
    , alpha2ToAlpha3
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
import qualified Data.Maybe as Y
import qualified Data.String as S
import qualified Data.Text as T

-- | A generic representation of a country, without a specific type of code.
data Country
    = A2 A2.Alpha2
    | A3 A3.Alpha3
    | N  N.Numeric
  deriving ( Show, Read )
instance Eq Country where
    A2 l == A2 r = l == r
    A3 l == A3 r = l == r
    N l == N r   = l == r
    A2 l == r    = maybe False (== l) $ unpack r
    A3 l == r    = maybe False (== l) $ unpack r
    N l == r     = maybe False (== l) $ unpack r
--TODO: instance Show Country (with 'pack' rather than 'A2')

-- | Provide a common interface for converting to and from a 'Country'.
class CountryCode a where
    pack   :: a -> Country
    unpack :: Country -> Maybe a

-- | Only available when importing "Data.Standards.ISO.Country.Primary" or
-- "Data.Standards.ISO.Country.Primary.Translation".
instance CountryCode A2.Alpha2 where
    pack = A2
    unpack (A2 cc) = Just cc
    unpack (A3 cc) = M.lookup cc a3a2
    unpack (N cc)  = M.lookup cc na2

-- | Only available when importing "Data.Standards.ISO.Country.Primary" or
-- "Data.Standards.ISO.Country.Primary.Translation".
instance CountryCode A3.Alpha3 where
    pack = A3
    unpack (A2 cc) = M.lookup cc a2a3
    unpack (A3 cc) = Just cc
    unpack (N cc)  = M.lookup cc na3

-- | Only available when importing "Data.Standards.ISO.Country.Primary" or
-- "Data.Standards.ISO.Country.Primary.Translation".
instance CountryCode N.Numeric where
    pack = N
    unpack (A2 cc) = M.lookup cc a2n
    unpack (A3 cc) = M.lookup cc a3n
    unpack (N cc)  = Just cc

{-# DEPRECATED alpha2ToAlpha3,alpha2ToNumeric "Pack and unpack via CountryCode instead" #-}

-- | Convert an 'Alpha2' country code to an 'Alpha3'.
alpha2ToAlpha3 :: A2.Alpha2 -> Maybe A3.Alpha3
alpha2ToAlpha3 = unpack . pack

-- | Convert an 'Alpha2' country code to a 'Numeric'.
alpha2ToNumeric :: A2.Alpha2 -> Maybe N.Numeric
alpha2ToNumeric = unpack . pack

-- | Convert an 'Alpha3' country code to an 'Alpha2'.
alpha3ToAlpha2 :: A3.Alpha3 -> Maybe A2.Alpha2
alpha3ToAlpha2 = unpack . pack

-- | Convert an 'Alpha3' country code to a 'Numeric'.
alpha3ToNumeric :: A3.Alpha3 -> Maybe N.Numeric
alpha3ToNumeric = unpack . pack

-- | Convert a 'Numeric' country code to an 'Alpha2'.
numericToAlpha2 :: N.Numeric -> Maybe A2.Alpha2
numericToAlpha2 = unpack . pack

-- | Convert a 'Numeric' country code to an 'Alpha3'.
numericToAlpha3 :: N.Numeric -> Maybe A3.Alpha3
numericToAlpha3 = unpack . pack

-- | Shorthand for defining 'tuples'.
t :: String -> Maybe T.Text
t = Just . S.fromString

-- | The mappings between countries which have a unique code in all systems of
-- representation.
tuples :: [(Maybe A2.Alpha2, Maybe A3.Alpha3, Maybe N.Numeric, Maybe T.Text)]
tuples =
    --TODO: Allow multiple mappings /to/ 'Numeric' for historic names, but only
    -- a single one /from/ it (the current).
    [ (Just A2.AC, Just A3.ASC, Nothing    , t "Ascension Island")
    , (Just A2.AD, Just A3.AND, Just N.C020, t "Andorra")
    , (Just A2.AE, Just A3.ARE, Just N.C784, t "United Arab Emirates (the)")
    , (Just A2.AF, Just A3.AFG, Just N.C004, t "Afghanistan")
    , (Just A2.AG, Just A3.ATG, Just N.C028, t "Antigua and Barbuda")
    , (Just A2.AI, Just A3.AIA, Just N.C660, t "Anguilla")
    , (Just A2.AL, Just A3.ALB, Just N.C008, t "Albania")
    , (Just A2.AM, Just A3.ARM, Just N.C051, t "Armenia")
    , (Just A2.AN, Just A3.ANT, Just N.C530, t "Netherlands Antilles")
    , (Just A2.AO, Just A3.AGO, Just N.C024, t "Angola")
    , (Just A2.AQ, Just A3.ATA, Just N.C010, t "Antarctica")
    , (Just A2.AR, Just A3.ARG, Just N.C032, t "Argentina")
    , (Just A2.AS, Just A3.ASM, Just N.C016, t "American Samoa")
    , (Just A2.AT, Just A3.AUT, Just N.C040, t "Austria")
    , (Just A2.AU, Just A3.AUS, Just N.C036, t "Australia")
    , (Just A2.AW, Just A3.ABW, Just N.C533, t "Aruba")
    , (Just A2.AX, Just A3.ALA, Just N.C248, t "Åland Islands")
    , (Just A2.AZ, Just A3.AZE, Just N.C031, t "Azerbaijan")

    , (Just A2.BA, Just A3.BIH, Just N.C070, t "Bosnia and Herzegovina")
    , (Just A2.BB, Just A3.BRB, Just N.C052, t "Barbados")
    , (Just A2.BD, Just A3.BGD, Just N.C050, t "Bangladesh")
    , (Just A2.BE, Just A3.BEL, Just N.C056, t "Belgium")
    , (Just A2.BF, Just A3.BFA, Just N.C854, t "Burkina Faso")
    , (Just A2.BG, Just A3.BGR, Just N.C100, t "Bulgaria")
    , (Just A2.BH, Just A3.BHR, Just N.C048, t "Bahrain")
    , (Just A2.BI, Just A3.BDI, Just N.C108, t "Burundi")
    , (Just A2.BJ, Just A3.BEN, Just N.C204, t "Benin")
    , (Just A2.BL, Just A3.BLM, Just N.C652, t "Saint Barthélemy")
    , (Just A2.BM, Just A3.BMU, Just N.C060, t "Bermuda")
    , (Just A2.BN, Just A3.BRN, Just N.C096, t "Brunei Darussalam")
    , (Just A2.BO, Just A3.BOL, Just N.C068, t "Bolivia (Plurinational State of)")
    , (Nothing   , Just A3.ATB, Just N.C080, t "British Antarctic Territory (the)")
    , (Just A2.BQ, Just A3.BES, Just N.C535, t "Bonaire, Sint Eustatius and Saba")
    , (Just A2.BR, Just A3.BRA, Just N.C076, t "Brazil")
    , (Just A2.BS, Just A3.BHS, Just N.C044, t "Bahamas (the)")
    , (Just A2.BT, Just A3.BTN, Just N.C064, t "Bhutan")
    , (Just A2.BU, Just A3.BUR, Nothing    , t "Burma")
    , (Just A2.BV, Just A3.BVT, Just N.C074, t "Bouvet Island")
    , (Just A2.BW, Just A3.BWA, Just N.C072, t "Botswana")
    , (Just A2.BY, Just A3.BLR, Just N.C112, t "Belarus")
    , (Just A2.BZ, Just A3.BLZ, Just N.C084, t "Belize")

    , (Just A2.CA, Just A3.CAN, Just N.C124, t "Canada")
    , (Just A2.CC, Just A3.CCK, Just N.C166, t "Cocos (Keeling) Islands (the)")
    , (Just A2.CD, Just A3.COD, Just N.C180, t "Congo (the Democratic Republic of the)")
    , (Just A2.CF, Just A3.CAF, Just N.C140, t "Central African Republic (the)")
    , (Just A2.CG, Just A3.COG, Just N.C178, t "Congo (the)")
    , (Just A2.CH, Just A3.CHE, Just N.C756, t "Switzerland")
    , (Just A2.CI, Just A3.CIV, Just N.C384, t "Côte d'Ivoire")
    , (Just A2.CK, Just A3.COK, Just N.C184, t "Cook Islands (the)")
    , (Just A2.CL, Just A3.CHL, Just N.C152, t "Chile")
    , (Just A2.CM, Just A3.CMR, Just N.C120, t "Cameroon")
    , (Just A2.CN, Just A3.CHN, Just N.C156, t "China")
    , (Just A2.CO, Just A3.COL, Just N.C170, t "Colombia")
    , (Just A2.CP, Just A3.CPT, Nothing    , t "Clipperton Island")
    , (Just A2.CR, Just A3.CRI, Just N.C188, t "Costa Rica")
    , (Nothing   , Just A3.CSK, Just N.C200, t "Czechoslovakia")
    , (Just A2.CS, Just A3.SCG, Just N.C891, t "Serbia and Montenegro")
    , (Just A2.CT, Just A3.CTE, Just N.C128, t "Canton and Enderbury Islands (the)")
    , (Just A2.CU, Just A3.CUB, Just N.C192, t "Cuba")
    , (Just A2.CV, Just A3.CPV, Just N.C132, t "Cabo Verde")
    , (Just A2.CW, Just A3.CUW, Just N.C531, t "Curaçao")
    , (Just A2.CX, Just A3.CXR, Just N.C162, t "Christmas Island")
    , (Just A2.CY, Just A3.CYP, Just N.C196, t "Cyprus")
    , (Just A2.CZ, Just A3.CZE, Just N.C203, t "Czechia")

    , (Just A2.DD, Just A3.DDR, Just N.C278, t "German Democratic Republic (the)")
    , (Just A2.DE, Just A3.DEU, Just N.C276, t "Germany")
    , (Just A2.DG, Just A3.DGA, Nothing    , t "Diego Garcia")
    , (Just A2.DJ, Just A3.DJI, Just N.C262, t "Djibouti")
    , (Just A2.DK, Just A3.DNK, Just N.C208, t "Denmark")
    , (Just A2.DM, Just A3.DMA, Just N.C212, t "Dominica")
    , (Just A2.DO, Just A3.DOM, Just N.C214, t "Dominican Republic (the)")
    , (Just A2.DZ, Just A3.DZA, Just N.C012, t "Algeria")

    , (Just A2.EA, Nothing    , Nothing    , t "Ceuta & Melilla")
    , (Just A2.EC, Just A3.ECU, Just N.C218, t "Ecuador")
    , (Just A2.EE, Just A3.EST, Just N.C233, t "Estonia")
    , (Just A2.EG, Just A3.EGY, Just N.C818, t "Egypt")
    , (Just A2.EH, Just A3.ESH, Just N.C732, t "Western Sahara")
    , (Just A2.ER, Just A3.ERI, Just N.C232, t "Eritrea")
    , (Just A2.ES, Just A3.ESP, Just N.C724, t "Spain")
    , (Just A2.ET, Just A3.ETH, Just N.C231, t "Ethiopia")
    , (Just A2.EU, Nothing    , Nothing    , t "European Union")
    , (Just A2.EZ, Nothing    , Nothing    , t "Eurozone")

    , (Just A2.FI, Just A3.FIN, Just N.C246, t "Finland")
    , (Just A2.FJ, Just A3.FJI, Just N.C242, t "Fiji")
    , (Just A2.FK, Just A3.FLK, Just N.C238, t "Falkland Islands (the) [Malvinas]")
    , (Just A2.FM, Just A3.FSM, Just N.C583, t "Micronesia (Federated States of)")
    , (Just A2.FO, Just A3.FRO, Just N.C234, t "Faroe Islands (the)")
    , (Just A2.FR, Just A3.FRA, Just N.C250, t "France")
    , (Just A2.FX, Just A3.FXX, Just N.C249, t "France, Metropolitan")

    , (Just A2.GA, Just A3.GAB, Just N.C266, t "Gabon")
    , (Just A2.GB, Just A3.GBR, Just N.C826, t "United Kingdom of Great Britain and Northern Ireland (the)")
    , (Just A2.GD, Just A3.GRD, Just N.C308, t "Grenada")
    , (Just A2.GE, Just A3.GEO, Just N.C268, t "Georgia")
    , (Just A2.GF, Just A3.GUF, Just N.C254, t "French Guiana")
    , (Just A2.GG, Just A3.GGY, Just N.C831, t "Guernsey")
    , (Just A2.GH, Just A3.GHA, Just N.C288, t "Ghana")
    , (Just A2.GI, Just A3.GIB, Just N.C292, t "Gibraltar")
    , (Just A2.GL, Just A3.GRL, Just N.C304, t "Greenland")
    , (Just A2.GM, Just A3.GMB, Just N.C270, t "Gambia (the)")
    , (Just A2.GN, Just A3.GIN, Just N.C324, t "Guinea")
    , (Just A2.GP, Just A3.GLP, Just N.C312, t "Guadeloupe")
    , (Just A2.GQ, Just A3.GNQ, Just N.C226, t "Equatorial Guinea")
    , (Just A2.GR, Just A3.GRC, Just N.C300, t "Greece")
    , (Just A2.GS, Just A3.SGS, Just N.C239, t "South Georgia and the South Sandwich Islands")
    , (Just A2.GT, Just A3.GTM, Just N.C320, t "Guatemala")
    , (Just A2.GU, Just A3.GUM, Just N.C316, t "Guam")
    , (Just A2.GW, Just A3.GNB, Just N.C624, t "Guinea-Bissau")
    , (Just A2.GY, Just A3.GUY, Just N.C328, t "Guyana")

    , (Just A2.HK, Just A3.HKG, Just N.C344, t "Hong Kong")
    , (Just A2.HM, Just A3.HMD, Just N.C334, t "Heard Island and McDonald Islands")
    , (Just A2.HN, Just A3.HND, Just N.C340, t "Honduras")
    , (Just A2.HR, Just A3.HRV, Just N.C191, t "Croatia")
    , (Just A2.HT, Just A3.HTI, Just N.C332, t "Haiti")
    , (Just A2.HU, Just A3.HUN, Just N.C348, t "Hungary")
    , (Just A2.HV, Just A3.HVO, Nothing    , t "Upper Volta")

    , (Just A2.IC, Nothing    , Nothing    , t "Canary Islands")
    , (Just A2.ID, Just A3.IDN, Just N.C360, t "Indonesia")
    , (Just A2.IE, Just A3.IRL, Just N.C372, t "Ireland")
    , (Just A2.IL, Just A3.ISR, Just N.C376, t "Israel")
    , (Just A2.IM, Just A3.IMN, Just N.C833, t "Isle of Man")
    , (Just A2.IN, Just A3.IND, Just N.C356, t "India")
    , (Just A2.IO, Just A3.IOT, Just N.C086, t "British Indian Ocean Territory (the)")
    , (Just A2.IQ, Just A3.IRQ, Just N.C368, t "Iraq")
    , (Just A2.IR, Just A3.IRN, Just N.C364, t "Iran (Islamic Republic of)")
    , (Just A2.IS, Just A3.ISL, Just N.C352, t "Iceland")
    , (Just A2.IT, Just A3.ITA, Just N.C380, t "Italy")

    , (Just A2.JE, Just A3.JEY, Just N.C832, t "Jersey")
    , (Just A2.JM, Just A3.JAM, Just N.C388, t "Jamaica")
    , (Just A2.JO, Just A3.JOR, Just N.C400, t "Jordan")
    , (Just A2.JP, Just A3.JPN, Just N.C392, t "Japan")
    , (Just A2.JT, Just A3.JTN, Just N.C396, t "Johnston Island")

    , (Just A2.KE, Just A3.KEN, Just N.C404, t "Kenya")
    , (Just A2.KG, Just A3.KGZ, Just N.C417, t "Kyrgyzstan")
    , (Just A2.KH, Just A3.KHM, Just N.C116, t "Cambodia")
    , (Just A2.KI, Just A3.KIR, Just N.C296, t "Kiribati")
    , (Just A2.KM, Just A3.COM, Just N.C174, t "Comoros (the)")
    , (Just A2.KN, Just A3.KNA, Just N.C659, t "Saint Kitts and Nevis")
    , (Just A2.KP, Just A3.PRK, Just N.C408, t "Korea (the Democratic People's Republic of)")
    , (Just A2.KR, Just A3.KOR, Just N.C410, t "Korea (the Republic of)")
    , (Just A2.KW, Just A3.KWT, Just N.C414, t "Kuwait")
    , (Just A2.KY, Just A3.CYM, Just N.C136, t "Cayman Islands (the)")
    , (Just A2.KZ, Just A3.KAZ, Just N.C398, t "Kazakhstan")

    , (Just A2.LA, Just A3.LAO, Just N.C418, t "Lao People's Democratic Republic (the)")
    , (Just A2.LB, Just A3.LBN, Just N.C422, t "Lebanon")
    , (Just A2.LC, Just A3.LCA, Just N.C662, t "Saint Lucia")
    , (Just A2.LI, Just A3.LIE, Just N.C438, t "Liechtenstein")
    , (Just A2.LK, Just A3.LKA, Just N.C144, t "Sri Lanka")
    , (Just A2.LR, Just A3.LBR, Just N.C430, t "Liberia")
    , (Just A2.LS, Just A3.LSO, Just N.C426, t "Lesotho")
    , (Just A2.LT, Just A3.LTU, Just N.C440, t "Lithuania")
    , (Just A2.LU, Just A3.LUX, Just N.C442, t "Luxembourg")
    , (Just A2.LV, Just A3.LVA, Just N.C428, t "Latvia")
    , (Just A2.LY, Just A3.LBY, Just N.C434, t "Libya")

    , (Just A2.MA, Just A3.MAR, Just N.C504, t "Morocco")
    , (Just A2.MC, Just A3.MCO, Just N.C492, t "Monaco")
    , (Just A2.MD, Just A3.MDA, Just N.C498, t "Moldova (the Republic of)")
    , (Just A2.ME, Just A3.MNE, Just N.C499, t "Montenegro")
    , (Just A2.MF, Just A3.MAF, Just N.C663, t "Saint Martin (French part)")
    , (Just A2.MG, Just A3.MDG, Just N.C450, t "Madagascar")
    , (Just A2.MH, Just A3.MHL, Just N.C584, t "Marshall Islands (the)")
    , (Just A2.MI, Just A3.MID, Just N.C488, t "Midway Islands (the)")
    , (Just A2.MK, Just A3.MKD, Just N.C807, t "Macedonia (the former Yugoslav Republic of)")
    , (Just A2.ML, Just A3.MLI, Just N.C466, t "Mali")
    , (Just A2.MM, Just A3.MMR, Just N.C104, t "Myanmar")
    , (Just A2.MN, Just A3.MNG, Just N.C496, t "Mongolia")
    , (Just A2.MO, Just A3.MAC, Just N.C446, t "Macao")
    , (Just A2.MP, Just A3.MNP, Just N.C580, t "Northern Mariana Islands (the)")
    , (Just A2.MQ, Just A3.MTQ, Just N.C474, t "Martinique")
    , (Just A2.MR, Just A3.MRT, Just N.C478, t "Mauritania")
    , (Just A2.MS, Just A3.MSR, Just N.C500, t "Montserrat")
    , (Just A2.MT, Just A3.MLT, Just N.C470, t "Malta")
    , (Just A2.MU, Just A3.MUS, Just N.C480, t "Mauritius")
    , (Just A2.MV, Just A3.MDV, Just N.C462, t "Maldives")
    , (Just A2.MW, Just A3.MWI, Just N.C454, t "Malawi")
    , (Just A2.MX, Just A3.MEX, Just N.C484, t "Mexico")
    , (Just A2.MY, Just A3.MYS, Just N.C458, t "Malaysia")
    , (Just A2.MZ, Just A3.MOZ, Just N.C508, t "Mozambique")

    , (Just A2.NA, Just A3.NAM, Just N.C516, t "Namibia")
    , (Just A2.NC, Just A3.NCL, Just N.C540, t "New Caledonia")
    , (Just A2.NE, Just A3.NER, Just N.C562, t "Niger (the)")
    , (Just A2.NF, Just A3.NFK, Just N.C574, t "Norfolk Island")
    , (Just A2.NG, Just A3.NGA, Just N.C566, t "Nigeria")
    , (Just A2.NH, Just A3.NHB, Nothing    , t "New Hebrides")
    , (Just A2.NI, Just A3.NIC, Just N.C558, t "Nicaragua")
    , (Just A2.NL, Just A3.NLD, Just N.C528, t "Netherlands (the)")
    , (Just A2.NO, Just A3.NOR, Just N.C578, t "Norway")
    , (Just A2.NP, Just A3.NPL, Just N.C524, t "Nepal")
    , (Just A2.NQ, Just A3.ATN, Just N.C216, t "Dronning Maud Land")
    , (Just A2.NR, Just A3.NRU, Just N.C520, t "Nauru")
    , (Just A2.NT, Just A3.NTZ, Just N.C536, t "Saudi Arabian-Iraqi neutral zone (the)")
    , (Just A2.NU, Just A3.NIU, Just N.C570, t "Niue")
    , (Just A2.NZ, Just A3.NZL, Just N.C554, t "New Zealand")

    , (Just A2.OM, Just A3.OMN, Just N.C512, t "Oman")

    , (Just A2.PA, Just A3.PAN, Just N.C591, t "Panama")
    , (Just A2.PC, Just A3.PCI, Just N.C582, t "Pacific Islands (Trust Territory of the)")
    , (Just A2.PE, Just A3.PER, Just N.C604, t "Peru")
    , (Just A2.PF, Just A3.PYF, Just N.C258, t "French Polynesia")
    , (Just A2.PG, Just A3.PNG, Just N.C598, t "Papua New Guinea")
    , (Just A2.PH, Just A3.PHL, Just N.C608, t "Philippines (the)")
    , (Just A2.PK, Just A3.PAK, Just N.C586, t "Pakistan")
    , (Just A2.PL, Just A3.POL, Just N.C616, t "Poland")
    , (Just A2.PM, Just A3.SPM, Just N.C666, t "Saint Pierre and Miquelon")
    , (Just A2.PN, Just A3.PCN, Just N.C612, t "Pitcairn")
    , (Just A2.PR, Just A3.PRI, Just N.C630, t "Puerto Rico")
    , (Just A2.PS, Just A3.PSE, Just N.C275, t "Palestine, State of")
    , (Just A2.PT, Just A3.PRT, Just N.C620, t "Portugal")
    , (Just A2.PU, Just A3.PUS, Just N.C849, t "United States Miscellaneous Pacific Islands (the)")
    , (Just A2.PW, Just A3.PLW, Just N.C585, t "Palau")
    , (Just A2.PY, Just A3.PRY, Just N.C600, t "Paraguay")
    , (Just A2.PZ, Just A3.PCZ, Just N.C594, t "Panama Canal Zone (the)")

    , (Just A2.QA, Just A3.QAT, Just N.C634, t "Qatar")

    , (Just A2.RE, Just A3.REU, Just N.C638, t "Réunion")
    , (Just A2.RO, Just A3.ROU, Just N.C642, t "Romania")
    , (Just A2.RS, Just A3.SRB, Just N.C688, t "Serbia")
    , (Just A2.RU, Just A3.RUS, Just N.C643, t "Russian Federation (the)")
    , (Just A2.RW, Just A3.RWA, Just N.C646, t "Rwanda")

    , (Just A2.SA, Just A3.SAU, Just N.C682, t "Saudi Arabia")
    , (Just A2.SB, Just A3.SLB, Just N.C090, t "Solomon Islands")
    , (Just A2.SC, Just A3.SYC, Just N.C690, t "Seychelles")
    , (Just A2.SD, Just A3.SDN, Just N.C729, t "Sudan (the)")
    , (Just A2.SE, Just A3.SWE, Just N.C752, t "Sweden")
    , (Just A2.SG, Just A3.SGP, Just N.C702, t "Singapore")
    , (Just A2.SH, Just A3.SHN, Just N.C654, t "Saint Helena, Ascension and Tristan da Cunha")
    , (Just A2.SI, Just A3.SVN, Just N.C705, t "Slovenia")
    , (Just A2.SJ, Just A3.SJM, Just N.C744, t "Svalbard and Jan Mayen")
    , (Nothing   , Just A3.SKM, Just N.C698, t "Sikkim")
    , (Just A2.SK, Just A3.SVK, Just N.C703, t "Slovakia")
    , (Just A2.SL, Just A3.SLE, Just N.C694, t "Sierra Leone")
    , (Just A2.SM, Just A3.SMR, Just N.C674, t "San Marino")
    , (Just A2.SN, Just A3.SEN, Just N.C686, t "Senegal")
    , (Just A2.SO, Just A3.SOM, Just N.C706, t "Somalia")
    , (Just A2.SR, Just A3.SUR, Just N.C740, t "Suriname")
    , (Just A2.SS, Just A3.SSD, Just N.C728, t "South Sudan")
    , (Just A2.ST, Just A3.STP, Just N.C678, t "Sao Tome and Principe")
    , (Just A2.SV, Just A3.SLV, Just N.C222, t "El Salvador")
    , (Just A2.SU, Just A3.SUN, Just N.C810, t "USSR (the)")
    , (Just A2.SX, Just A3.SXM, Just N.C534, t "Sint Maarten (Dutch part)")
    , (Just A2.SY, Just A3.SYR, Just N.C760, t "Syrian Arab Republic")
    , (Just A2.SZ, Just A3.SWZ, Just N.C748, t "Swaziland")

    , (Just A2.TA, Just A3.TAA, Nothing    , t "Tristan da Cunha")
    , (Just A2.TC, Just A3.TCA, Just N.C796, t "Turks and Caicos Islands (the)")
    , (Just A2.TD, Just A3.TCD, Just N.C148, t "Chad")
    , (Just A2.TF, Just A3.ATF, Just N.C260, t "French Southern Territories (the)")
    , (Just A2.TG, Just A3.TGO, Just N.C768, t "Togo")
    , (Just A2.TH, Just A3.THA, Just N.C764, t "Thailand")
    , (Just A2.TJ, Just A3.TJK, Just N.C762, t "Tajikistan")
    , (Just A2.TK, Just A3.TKL, Just N.C772, t "Tokelau")
    , (Just A2.TL, Just A3.TLS, Just N.C626, t "Timor-Leste")
    , (Just A2.TM, Just A3.TKM, Just N.C795, t "Turkmenistan")
    , (Just A2.TN, Just A3.TUN, Just N.C788, t "Tunisia")
    , (Just A2.TO, Just A3.TON, Just N.C776, t "Tonga")
    , (Just A2.TR, Just A3.TUR, Just N.C792, t "Turkey")
    , (Just A2.TP, Just A3.TMP, Nothing    , t "East Timor")
    , (Just A2.TT, Just A3.TTO, Just N.C780, t "Trinidad and Tobago")
    , (Just A2.TV, Just A3.TUV, Just N.C798, t "Tuvalu")
    , (Just A2.TW, Just A3.TWN, Just N.C158, t "Taiwan (Province of China)")
    , (Just A2.TZ, Just A3.TZA, Just N.C834, t "Tanzania, United Republic of")

    , (Just A2.UA, Just A3.UKR, Just N.C804, t "Ukraine")
    , (Just A2.UG, Just A3.UGA, Just N.C800, t "Uganda")
    , (Just A2.UK, Nothing    , Nothing    , t "United Kingdom")
    , (Just A2.UM, Just A3.UMI, Just N.C581, t "United States Minor Outlying Islands (the)")
    , (Just A2.UN, Nothing    , Nothing    , t "United Nations")
    , (Just A2.US, Just A3.USA, Just N.C840, t "United States of America (the)")
    , (Just A2.UY, Just A3.URY, Just N.C858, t "Uruguay")
    , (Just A2.UZ, Just A3.UZB, Just N.C860, t "Uzbekistan")

    , (Just A2.VA, Just A3.VAT, Just N.C336, t "Holy See (the)")
    , (Just A2.VC, Just A3.VCT, Just N.C670, t "Saint Vincent and the Grenadines")
    , (Just A2.VD, Just A3.VDR, Just N.C714, t "Viet Nam (Democratic Republic of)")
    , (Just A2.VE, Just A3.VEN, Just N.C862, t "Venezuela (Bolivarian Republic of)")
    , (Just A2.VG, Just A3.VGB, Just N.C092, t "Virgin Islands (British)")
    , (Just A2.VI, Just A3.VIR, Just N.C850, t "Virgin Islands (U.S.)")
    , (Just A2.VN, Just A3.VNM, Just N.C704, t "Viet Nam")
    , (Just A2.VU, Just A3.VUT, Just N.C548, t "Vanuatu")

    , (Just A2.WF, Just A3.WLF, Just N.C876, t "Wallis and Futuna")
    , (Just A2.WK, Just A3.WAK, Just N.C872, t "Wake Island")
    , (Just A2.WS, Just A3.WSM, Just N.C882, t "Samoa")

    , (Just A2.YD, Just A3.YMD, Just N.C720, t "Yemen (Democratic)")
    , (Just A2.YE, Just A3.YEM, Just N.C887, t "Yemen")
    , (Just A2.YT, Just A3.MYT, Just N.C175, t "Mayotte")
    , (Just A2.YU, Just A3.YUG, Just N.C890, t "Yugoslavia")

    , (Just A2.ZA, Just A3.ZAF, Just N.C710, t "South Africa")
    , (Just A2.ZM, Just A3.ZMB, Just N.C894, t "Zambia")
    , (Just A2.ZW, Just A3.ZWE, Just N.C716, t "Zimbabwe")
    , (Just A2.ZR, Just A3.ZAR, Nothing    , t "Zaire")
    ]

-- | Cache 'Alpha2' -> 'Alpha3' conversions, and provide easier (but still
-- slightly slower) lookup.
a2a3 :: M.HashMap A2.Alpha2 A3.Alpha3
a2a3 = M.fromList . flip Y.mapMaybe tuples
     $ \(ma2, ma3, _, _) -> do
         a2 <- ma2
         a3 <- ma3
         return (a2, a3)

-- | Cache 'Alpha2' -> 'Numeric' conversions, and provide easier (but still
-- slightly slower) lookup.
a2n :: M.HashMap A2.Alpha2 N.Numeric
a2n = M.fromList . flip Y.mapMaybe tuples
    $ \(ma2, _, mn, _) -> do
        a2 <- ma2
        n <- mn
        return (a2, n)

-- | Cache 'Alpha3' -> 'Alpha2' conversions, and provide easier (but still
-- slightly slower) lookup.
a3a2 :: M.HashMap A3.Alpha3 A2.Alpha2
a3a2 = M.fromList . flip Y.mapMaybe tuples
     $ \(ma2, ma3, _, _) -> do
         a2 <- ma2
         a3 <- ma3
         return (a3, a2)

-- | Cache 'Alpha3' -> 'Numeric' conversions, and provide easier (but still
-- slightly slower) lookup.
a3n :: M.HashMap A3.Alpha3 N.Numeric
a3n = M.fromList . flip Y.mapMaybe tuples
    $ \(_, ma3, mn, _) -> do
        a3 <- ma3
        n <- mn
        return (a3, n)

-- | Cache 'Numeric' -> 'Alpha2' conversions, and provide easier (but still
-- slightly slower) lookup.
na2 :: M.HashMap N.Numeric A2.Alpha2
na2 = M.fromList . flip Y.mapMaybe tuples
    $ \(ma2, _, mn, _) -> do
        a2 <- ma2
        n <- mn
        return (n, a2)

-- | Cache 'Numeric' -> 'Alpha3' conversions, and provide easier (but still
-- slightly slower) lookup.
na3 :: M.HashMap N.Numeric A3.Alpha3
na3 = M.fromList . flip Y.mapMaybe tuples
    $ \(_, ma3, mn, _) -> do
        a3 <- ma3
        n <- mn
        return (n, a3)

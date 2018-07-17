# Revision history for data-standards

## 0.1.1.0

* Data.Standards.ISO.Country.Primary.Translation: Deprecate individual
  conversions in favor of an opaque collective class ('Country')

## 0.1.0.1

* "Data.Standards.ISO.Country.Primary" now reexports the types unqualified
* All ISO 3166-1 code types now have 'Ord' instances
* 'enumFromThen' and 'enumFromThenTo' over 'Numeric' no longer throw an error
  if the max bound was included in the sequence
* Updated doumentation to reflect new name for Eswatini (SZ/SWZ/748)

## 0.1.0.0  -- 2018-07-06

* ISO 3166-1, in all three variants (alpha-2, alpha-3, and numeric)
  * Methods of converting between all three
  * Historic or otherwise discouraged codes marked as such

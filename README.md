A number of the ISO standards are particularly attractive to being implemented
as static data libraries: they describe short strings that are prone to error
if left as 'Text', but are large enough to strongly discourage rewriting them
for each project.  It is no surprise, therefore, that this package is the
fourth on Hackage to implement ISO 3166-1:

    * "Data.ISO3166\_CountryCodes" from
      [iso3166-country-codes](https://hackage.haskell.org/package/iso3166-country-codes)
    * "Data.CountryCodes" from
      [country-codes](https://hackage.haskell.org/package/country-codes)
    * "Country" from [country](https://hackage.haskell.org/package/country)

However, none of those options truly seemed to provide an ideal, /complete/
experience.  Only one (@country@) provides all three code encodings, but it
doesn't expose constructors for pattern matching.  None of them implement the
other two parts of ISO 3166 (region codes, and historic countries), and the
only package that makes an effort to do so ("Data.StateCodes" from
[state-codes](https://hackage.haskell.org/package/state-codes)) doesn't cover
anything beyond the US.  Someone who only needs the alpha-2 codes ("CA") may
indeed find one of the existing packages sufficient, but for anyone who does
need the full power of ISO 3166-2, none of them would serve.

The lack of people who would actually find this useful is another matter.
(Eight packages using the simple alpha-2 codes from one or another of the
above, as of writing.)

This package is an attempt to package that standard -- and, eventually, others
like it -- in a consistent API based in standard Haskell; `fromEnum` rather
than `encodeNumeric`, for example.

The following standards are implemented:

    * ISO 3166-1 (`Data.Standards.ISO.Country.Primary`)

        * alpha-2 (`Data.Standards.ISO.Country.Primary.Alpha2`)
        * alpha-3 (`Data.Standards.ISO.Country.Primary.Alpha3`)
        * numeric (`Data.Standards.ISO.Country.Primary.Numeric`)

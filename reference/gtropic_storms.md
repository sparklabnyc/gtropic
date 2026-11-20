# List available tropical cyclones

Retrieves metadata for tropical cyclones represented in G-TROPIC,
including storm identifiers, names, and genesis dates. This function can
be used to identify storms and confirm temporal coverage before
retrieving exposure data with
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

Date filters are applied to `STORM_GENESIS_DATE_UTC`, the UTC date of
the first best-track observation. They do not select storms by landfall,
closest approach, or date of exposure.

When a requested `STORM_NAME` is not available, G-TROPIC checks the
published exclusion records and reports whether the storm is excluded or
absent from both lists.

`STORM_NAME` is a convenience lookup and can identify multiple storms.
If a name matches multiple unique `STORM_ID`s after all filters are
applied, this function returns every match and warns. Use
`gtropic_storms(STORM_NAME = ...)` to identify the intended `STORM_ID`,
then query
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
with `STORM_ID`, which is unique and preferred.

## Usage

``` r
gtropic_storms(
  date_range = NULL,
  year = NULL,
  STORM_NAME = NULL,
  version = ":latest",
  cache = NULL
)
```

## Arguments

- date_range:

  Optional vector of length two defining an inclusive range of storm
  genesis dates. Bare years, year-month values, complete dates, and
  `Date` or `POSIXct` values are accepted as described in
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

- year:

  Optional vector of storm genesis calendar years to retrieve. Ignored
  when `date_range` is supplied. Years outside the published collection
  are omitted.

- STORM_NAME:

  Optional character vector of published convenience labels in
  `Name-YYYY` form, such as `Katrina-2005`. Labels can duplicate across
  distinct `STORM_ID`s. Matching is case-insensitive; every supplied
  value must match a published name. The suffix is the storm season.
  Because Southern Hemisphere storms can form in the preceding calendar
  year, partitions for both `YYYY - 1` and `YYYY` are considered. This
  candidate set is combined with an explicit genesis `year` or
  `date_range` filter using AND.

- version:

  Dataset version to use. The default, `":latest"`, uses the most recent
  published version. See
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  for version formats.

- cache:

  Whether downloaded files may be cached. `NULL` uses
  `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
  setting for this call.

## Value

A tibble containing the published metadata records for storms that
satisfy the requested genesis period and name filters. If no metadata
files are available for the selected years, a warning is issued and an
empty tibble is returned. `STORM_GENESIS_DATE_UTC` is returned as a
`Date`.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md),
[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md),
[`gtropic_excluded_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_excluded_storms.md)

## Examples

``` r
# \donttest{
# Review storms that formed during calendar year 2005
gtropic_storms(year = 2005)
#> # A tibble: 103 × 4
#>    STORM_ID      STORM_NAME   USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>        <chr>       <date>                
#>  1 2005003S09177 Kerry-2005   SH082005    2005-01-03            
#>  2 2005006S09099 Sally-2005   SH092005    2005-01-06            
#>  3 2005007N04085 Unnamed-2005 IO012005    2005-01-07            
#>  4 2005012N04090 Unnamed-2005 IO022005    2005-01-11            
#>  5 2005013N05153 Kulap-2005   WP012005    2005-01-13            
#>  6 2005013S14124 Unnamed-2005 SH102005    2005-01-13            
#>  7 2005015S06089 Daren-2005   SH112005    2005-01-15            
#>  8 2005017S09061 Ernest-2005  SH122005    2005-01-16            
#>  9 2005022S14112 Tim-2005     SH132005    2005-01-22            
#> 10 2005029S11072 Gerard-2005  SH142005    2005-01-29            
#> # ℹ 93 more rows

# Retrieve the published identifiers and dates for Hurricane Katrina
gtropic_storms(STORM_NAME = "Katrina-2005")
#> # A tibble: 1 × 4
#>   STORM_ID      STORM_NAME   USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>   <chr>         <chr>        <chr>       <date>                
#> 1 2005236N23285 Katrina-2005 AL122005    2005-08-23            
# }
```

# List available tropical cyclones

Retrieves metadata for tropical cyclones represented in G-TROPIC,
including storm identifiers, names, and genesis dates. This function can
be used to identify storms and confirm temporal coverage before
retrieving exposure data with
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

Date filters are applied to `STORM_GENESIS_DATE_UTC`, the UTC date of
the first best-track observation. They do not select storms by landfall,
closest approach, or date of exposure.

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

  Optional vector of years to retrieve. Ignored when `date_range` is
  supplied. Years outside the published collection are omitted.

- STORM_NAME:

  Optional character vector of storm names. Matching is
  case-insensitive; every supplied value must match a published name.

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
empty tibble is returned.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md),
[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md)

## Examples

``` r
# \donttest{
# Review storms that formed during the 2005 season
gtropic_storms(year = 2005)
#> # A tibble: 75 × 4
#>    STORM_ID      STORM_NAME   USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>        <chr>       <date>                
#>  1 2005003S09177 Kerry-2005   SH082005    2005-01-03            
#>  2 2005007N04085 Unnamed-2005 IO012005    2005-01-07            
#>  3 2005013N05153 Kulap-2005   WP012005    2005-01-13            
#>  4 2005017S09061 Ernest-2005  SH122005    2005-01-16            
#>  5 2005029S11072 Gerard-2005  SH142005    2005-01-29            
#>  6 2005032S14195 Meena-2005   SH152005    2005-02-01            
#>  7 2005035S14136 Harvey-2005  SH162005    2005-02-03            
#>  8 2005041S13181 Olaf-2005    SH192005    2005-02-10            
#>  9 2005042S12190 Nancy-2005   SH182005    2005-02-10            
#> 10 2005054S09173 Percy-2005   SH202005    2005-02-23            
#> # ℹ 65 more rows

# Retrieve the published identifiers and dates for Hurricane Katrina
gtropic_storms(STORM_NAME = "Katrina-2005")
#> # A tibble: 1 × 4
#>   STORM_ID      STORM_NAME   USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>   <chr>         <chr>        <chr>       <date>                
#> 1 2005236N23285 Katrina-2005 AL122005    2005-08-23            
# }
```

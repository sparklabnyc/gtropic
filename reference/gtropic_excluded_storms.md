# List excluded tropical cyclones

Retrieves records for storms excluded from the published wind data.
These records identify storms that cannot be retrieved with
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).
Published `STORM_NAME` convenience labels can duplicate; the unique
`STORM_ID` values in the returned rows distinguish excluded records.

Date filters are applied to `STORM_GENESIS_DATE_UTC`, the UTC date of
the first best-track observation. They do not select storms by landfall,
closest approach, or date of exposure.

## Usage

``` r
gtropic_excluded_storms(
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
  `Name-YYYY` form. Labels can duplicate across distinct `STORM_ID`s.
  Matching is case-insensitive; every supplied value must match an
  excluded-storm record. The suffix is the storm season; partitions for
  genesis years `YYYY - 1` and `YYYY` are considered and combined with
  an explicit genesis `year` or `date_range` filter using AND.

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

A tibble with `STORM_ID`, `STORM_NAME`, and `STORM_GENESIS_DATE_UTC` for
published excluded storms satisfying the requested genesis period and
name filters. If no exclusion files are available for the selected
years, a warning is issued and an empty tibble is returned.
`STORM_GENESIS_DATE_UTC` is returned as a `Date`.

## See also

[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md),
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
# Review exclusion records for storms that formed during calendar year 2005
gtropic_excluded_storms(year = 2005)
#> # A tibble: 12 × 3
#>    STORM_ID      STORM_NAME   STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>        <date>                
#>  1 2005026S20040 Felapi-2005  2005-01-26            
#>  2 2005027S18168 Lola-2005    2005-01-27            
#>  3 2005103S08138 Unnamed-2005 2005-04-13            
#>  4 2005110S17180 Sheila-2005  2005-04-20            
#>  5 2005172N22069 Unnamed-2005 2005-06-21            
#>  6 2005179N22088 Unnamed-2005 2005-06-27            
#>  7 2005210N21088 Unnamed-2005 2005-07-29            
#>  8 2005255N14113 Unnamed-2005 2005-09-12            
#>  9 2005255N20088 Unnamed-2005 2005-09-12            
#> 10 2005257N20069 Unnamed-2005 2005-09-14            
#> 11 2005260N20091 Unnamed-2005 2005-09-17            
#> 12 2005324N08085 Unnamed-2005 2005-11-20            
# }
```

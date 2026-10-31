# List excluded tropical cyclones

Retrieves records for storms excluded from the published wind data.
These records identify storms that cannot be retrieved with
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

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

  Optional vector of years to retrieve. Ignored when `date_range` is
  supplied. Years outside the published collection are omitted.

- STORM_NAME:

  Optional character vector of published, disambiguated storm names in
  `Name-YYYY` form. Matching is case-insensitive; every supplied value
  must match an excluded-storm record. The suffix year limits the files
  scanned and is combined with an explicit `year` or `date_range` filter
  using AND.

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

## See also

[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md),
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
# Review exclusion records for storms that formed during the 2005 season
gtropic_excluded_storms(year = 2005)
#> Warning: No excluded-storm records found for the years requested.
#> # A tibble: 0 × 0
# }
```

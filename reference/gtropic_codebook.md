# Retrieve the G-TROPIC data codebook

Downloads the published G-TROPIC codebook. The codebook describes the
tables, variables, measurement units, and data provenance needed to
interpret wind, precipitation, flood, population, and storm metadata in
an exposure analysis.

## Usage

``` r
gtropic_codebook(version = ":latest", cache = NULL)
```

## Arguments

- version:

  Dataset version to use. The default, `":latest"`, uses the most recent
  published version. See
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  for version formats.

- cache:

  Whether downloaded source files may be cached. `NULL` uses
  `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
  setting for this call.

## Value

A nested list preserving the structure of the published JSON codebook.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
codebook <- gtropic_codebook()
names(codebook)
#> [1] "codebook_version" "format"           "created_for"      "read_in_r"       
#> [5] "notes"            "raw_sources"      "datasets"        
# }
```

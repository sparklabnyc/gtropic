# Print a G-TROPIC data result

Displays a concise overview of a G-TROPIC retrieval, including source
dataset versions, resolved genesis period, numbers of storms and ADM2
units, and the dimensions of each returned table. Printing does not
modify or materialize the result.

## Usage

``` r
# S3 method for class 'gtropic_data'
print(x, ...)
```

## Arguments

- x:

  A `gtropic_data` object.

- ...:

  Additional arguments passed to the print method. Currently ignored.

## Value

`x`, invisibly.

## Examples

``` r
# \donttest{
gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 8 files (133.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading metadata/codebook.json.
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────
#> • Historical dataset: version "3.0"
#> • Current-year dataset: version "4.0"
#> • Genesis dates: "2005-01-01" to "2005-12-31"
#> • Storms: 75
#> • ADM2 units: 827
#> 
#> ── Tables 
#> • wind: 17828 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • pop: 827 rows x 6 columns
#> • storm_metadata: 75 rows x 4 columns
#> • adm2: 827 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 61198 rows x 11 columns
#> 
#> ℹ Use `summary()` for detail.
# }
```

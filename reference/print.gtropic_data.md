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
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 10 files (143.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading 02_wind/storm_tracks/2005.parquet.
#> Downloading metadata/codebook.json.
#> Downloading metadata/adm2_geometry_simplified.parquet.
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────
#> • Historical dataset: doi:10.70122/FK2/SWEYST (version 4.0)
#> • Genesis dates: "2005-01-01" to "2005-12-31"
#> • Storms: 5
#> • ADM2 units: 827
#> 
#> ── Tables 
#> • wind: 3719 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • zero_pairs: 43370 rows x 5 columns
#> • tracks: 376 rows x 10 columns
#> • pop: 827 rows x 6 columns
#> • storm_metadata: 5 rows x 4 columns
#> • adm2: 827 rows x 5 columns
#> • geometry: 827 rows x 2 columns
#> • codebook: 7 entries (list)
#> • links: 3719 rows x 12 columns
#> 
#> ℹ Use `summary()` for detail.
# }
```

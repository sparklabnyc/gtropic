# Summarize a G-TROPIC data result

Displays an extended overview of a G-TROPIC retrieval. The output
includes dataset provenance, retrieval time and size, returned table
dimensions, resolved geographic and storm filters, hazard-specific
temporal and spatial windows, and any diagnostics retained with the
result.

## Usage

``` r
# S3 method for class 'gtropic_data'
summary(object, ...)
```

## Arguments

- object:

  A `gtropic_data` object.

- ...:

  Additional arguments passed to the summary method. Currently ignored.

## Value

`object`, invisibly.

## Examples

``` r
# \donttest{
res <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
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
summary(res)
#> 
#> ── G-TROPIC data summary ───────────────────────────────────────────────────────
#> 
#> ── Sources ──
#> 
#> • Server: "demo.dataverse.org"
#> • Historical dataset: doi:10.70122/FK2/SWEYST v4.0
#> • Files retrieved: 10 (143.0 MB)
#> • Retrieved at: 2026-10-31 02:43:06 UTC
#> 
#> ── Tables ──
#> 
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
#> ── Resolved filters ──
#> 
#> • ADM2 units: 827
#> • Storms: 5
#> • Genesis window: 2005-01-01 to 2005-12-31
#> • Wind radius: 500 km
#> • Precipitation window: -2/ +1 days within 500 km
#> • Flood window: -2/+1 days within 500 km (match: "start")
# }
```

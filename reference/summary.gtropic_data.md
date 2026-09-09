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
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 8 files (133.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading metadata/codebook.json.
summary(res)
#> 
#> ── G-TROPIC data summary ───────────────────────────────────────────────────────
#> 
#> ── Sources ──
#> 
#> • Server: "demo.dataverse.org"
#> • Historical: "doi:10.70122/FK2/SWEYST" v3.0
#> • Current-year: "doi:10.70122/FK2/DDSYYD" v4.0
#> • Files retrieved: 8 (133.0 MB)
#> • Retrieved at: 2026-09-09 15:50:20 UTC
#> 
#> ── Tables ──
#> 
#> • wind: 17828 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • pop: 827 rows x 6 columns
#> • storm_metadata: 75 rows x 4 columns
#> • adm2: 827 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 61198 rows x 11 columns
#> 
#> ── Resolved filters ──
#> 
#> • ADM2 units: 827
#> • Storms: 75
#> • Genesis window: 2005-01-01 to 2005-12-31
#> • Precipitation window: -2/ +1 days within 500 km
#> • Flood window: -2/+1 days within 500 km (match: "start")
# }
```

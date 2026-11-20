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
#> Retrieving 10 files (165.5 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Warning: No non-missing precipitation values were returned for 146 selected ADM2 units.
#> ! "35162164B10440863111199", "35162164B11758798186333",
#>   "35162164B13235352626667", "35162164B13736079230506",
#>   "35162164B14656063603224", "35162164B14734200252425",
#>   "35162164B15393502878039", "35162164B15608303769076",
#>   "35162164B15658417820468", "35162164B15710553395599", and "... and 136 more"
#> ℹ A known current source-data limitation can affect small or coastal ADM2 units
#>   when no ERA5-Land grid cell is assigned.
#> ℹ A future G-TROPIC dataset release is planned to fill these gaps using
#>   finer-resolution ERA5 precipitation data.
#> ℹ See `?gtropic_known_limitations`; complete identifiers are available from
#>   `gtropic_warnings()`.
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
#> • Server: "dataverse.harvard.edu"
#> • Historical dataset: doi:10.7910/DVN/EAVWC3 v1.0
#> • Files retrieved: 10 (165.5 MB)
#> • Retrieved at: 2026-11-20 02:06:47 UTC
#> 
#> ── Tables ──
#> 
#> • wind: 3719 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • zero_pairs: 64083 rows x 5 columns
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
#> 
#> ── Warnings ──
#> 
#> • precip_all_missing_adm2_ids: 35162164B10440863111199,
#> 35162164B11758798186333, 35162164B13235352626667, 35162164B13736079230506,
#> 35162164B14656063603224, and ... and 141 more
# }
```

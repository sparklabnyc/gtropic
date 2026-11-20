# Print a G-TROPIC version manifest

Displays the dataset server, DOI and version information for datasets
actually used, file count and total size, retrieval time, and package
version recorded for a G-TROPIC retrieval. A manifest can be supplied to
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
to request the same published source files in a subsequent analysis.

## Usage

``` r
# S3 method for class 'gtropic_manifest'
print(x, ...)
```

## Arguments

- x:

  A `gtropic_manifest` object returned by
  [`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md).

- ...:

  Additional arguments passed to the print method. Currently ignored.

## Value

`x`, invisibly.

## See also

[`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md),
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
result <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
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
gtropic_manifest(result)
#> 
#> ── gtropic version manifest ────────────────────────────────────────────────────
#> • Server: "dataverse.harvard.edu"
#> • Historical dataset: doi:10.7910/DVN/EAVWC3 (version 1.0)
#> • Files recorded: 10
#> • Total size: 165.5 MB
#> • Retrieved: 2026-11-20 02:06:16 UTC
#> • Package version: "0.0.0.9002"
#> 
#> ℹ Pass this object as `manifest` to `gtropic_data()` to
#> reproduce the same retrieval.
# }
```

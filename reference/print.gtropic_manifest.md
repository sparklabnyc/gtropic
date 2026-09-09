# Print a G-TROPIC version manifest

Displays the dataset server, DOI and version information, file count and
total size, retrieval time, and package version recorded for a G-TROPIC
retrieval. A manifest can be supplied to
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
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 8 files (133.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading metadata/codebook.json.
gtropic_manifest(result)
#> 
#> ── gtropic version manifest ────────────────────────────────────────────────────
#> • Server: "demo.dataverse.org"
#> • Historical: "doi:10.70122/FK2/SWEYST" (version "3.0")
#> • Current-year: "doi:10.70122/FK2/DDSYYD" (version "4.0")
#> • Files recorded: 8
#> • Total size: 133.0 MB
#> • Retrieved: 2026-09-09 15:50:13 UTC
#> • Package version: "0.0.0.9000"
#> 
#> ℹ Pass this object as `manifest` to `gtropic_data()` to
#> reproduce the same retrieval.
# }
```

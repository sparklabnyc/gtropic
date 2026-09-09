# Extract metadata from a G-TROPIC result

These accessor functions retrieve provenance, resolved query settings,
and recorded diagnostic information from an object returned by
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

`gtropic_manifest()` identifies the source datasets and files used in
the retrieval, including their versions and checksums. Supplying the
manifest to a later
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
call requests those same source files, provided they remain available.

`gtropic_filters()` reports the filters as resolved by the package,
including selected ADM2 and storm identifiers, genesis-date bounds,
years represented by the selected storms, and precipitation and flood
window settings. These values support transparent reporting and quality
assurance in downstream analyses.

`gtropic_warnings()` returns diagnostic details retained with the
result. This can include complete values that were abbreviated in a
console warning, such as ADM2 units whose polygon membership varied over
time. It is not a log of every warning that may have been emitted during
retrieval.

## Usage

``` r
gtropic_manifest(x)

gtropic_filters(x)

gtropic_warnings(x)
```

## Arguments

- x:

  A `gtropic_data` object.

## Value

`gtropic_manifest()` returns an object of class `gtropic_manifest`.
`gtropic_filters()` returns a named list of resolved query settings.
`gtropic_warnings()` returns a named list of recorded diagnostics, which
is empty when none were retained.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
res <- gtropic_data(WHO_ENTITY = "Philippines", date_range = c(2015, 2016))
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Downloading 02_wind/storm_metadata/2016.parquet.
#> Retrieving 14 files (256.5 MB) ; .
#> Downloading 02_wind/exposures/2015.parquet.
#> Downloading 02_wind/exposures/2016.parquet.
#> Downloading 02_wind/zero_pairs/2015.parquet.
#> Downloading 02_wind/zero_pairs/2016.parquet.
#> Downloading 04_precip/2015.parquet.
#> Downloading 04_precip/2016.parquet.
#> Downloading 03_flood/2015.parquet.
#> Downloading 03_flood/2016.parquet.
#> Downloading 01_pop/2015.parquet.
#> Downloading 01_pop/2016.parquet.
#> Downloading metadata/codebook.json.
manifest <- gtropic_manifest(res)
filters <- gtropic_filters(res)
diagnostics <- gtropic_warnings(res)
# }
```

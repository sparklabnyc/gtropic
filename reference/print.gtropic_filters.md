# Print resolved G-TROPIC filters

Displays each resolved filter with a compact value preview. Long
vectors, such as selected ADM2 identifiers, show at most `n` values and
state how many values were omitted. The complete values remain available
through ordinary list access.

## Usage

``` r
# S3 method for class 'gtropic_filters'
print(x, n = 10L, ...)
```

## Arguments

- x:

  A `gtropic_filters` object returned by
  [`gtropic_filters()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md).

- n:

  Maximum number of values to display for each filter.

- ...:

  Additional arguments passed to the print method. Currently ignored.

## Value

`x`, invisibly.

## Examples

``` r
# \donttest{
res <- gtropic_data(WHO_ENTITY = "Philippines", date_range = c(2015, 2016))
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Downloading 02_wind/storm_metadata/2016.parquet.
#> Retrieving 17 files (266.5 MB) ; .
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
#> Downloading 02_wind/storm_tracks/2015.parquet.
#> Downloading 02_wind/storm_tracks/2016.parquet.
#> Downloading metadata/codebook.json.
#> Downloading metadata/adm2_geometry_simplified.parquet.
print(gtropic_filters(res))
#> 
#> ── G-TROPIC resolved filters ───────────────────────────────────────────────────
#> • adm2_ids (82 values): ["2640588B11276327116073", "2640588B11290582279728",
#> "2640588B11393510524278", "2640588B12869096786612", "2640588B14186532473464",
#> "2640588B17055181338626", "2640588B17162343229623", "2640588B17428979589921",
#> "2640588B19040020708567", "2640588B20221108717018"] (72 values omitted)
#> • storm_ids (25 values): ["2015012N09146", "2015085N06162", "2015122N07144",
#> "2015183N13130", "2015203N16126", "2015273N12130", "2015285N14151",
#> "2015344N07145", "2016211N11128", "2016287N12131"] (15 values omitted)
#> • date_start (1 value): [2015-01-01]
#> • date_end (1 value): [2016-12-31]
#> • date_order (1 value): ["ymd"]
#> • years_scanned (2 values): [2015, 2016]
#> • wind_radius_km (1 value): [500]
#> • precip_days_before (1 value): [2]
#> • precip_days_after (1 value): [1]
#> • precip_radius_km (1 value): [500]
#> • flood_days_before (1 value): [2]
#> • flood_days_after (1 value): [1]
#> • flood_radius_km (1 value): [500]
#> • flood_match (1 value): ["start"]
#> • inconsistent_adm2_ids (0 values): <empty>
# }
```

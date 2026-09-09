# List available second-level administrative units

Retrieves the G-TROPIC lookup table for second-level administrative
units (ADM2), such as counties, districts, or provinces. The table
provides the geographic identifiers and names used to define study areas
in
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).

Use this function to identify valid geographic values before requesting
exposure data. Filters that do not match a published value produce an
error rather than an empty result.

## Usage

``` r
gtropic_adm2(
  ADM2_NAME = NULL,
  ADM2_GROUP = NULL,
  WHO_ENTITY = NULL,
  WHO_REGION = NULL,
  version = ":latest",
  cache = NULL
)
```

## Arguments

- ADM2_NAME, ADM2_GROUP, WHO_ENTITY, WHO_REGION:

  Optional character vectors used to restrict the lookup table. Values
  are matched case-insensitively after normalizing whitespace. Multiple
  values within an argument are combined with OR; filters supplied
  through different arguments are combined with AND.

- version:

  Dataset version to use. The default, `":latest"`, uses the most recent
  published version. See
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  for version formats.

- cache:

  Whether downloaded files may be cached. `NULL` uses
  `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
  setting for this call.

## Value

A tibble containing the published ADM2 lookup. Each row represents an
administrative unit and includes its stable `ADM2_ID` and available
name, country or territory, and WHO regional classifications.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md),
[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md)

## Examples

``` r
# \donttest{
# Review all available administrative units
gtropic_adm2()
#> # A tibble: 49,349 × 5
#>    ADM2_ID                 ADM2_NAME    ADM2_GROUP WHO_ENTITY  WHO_REGION       
#>    <chr>                   <chr>        <chr>      <chr>       <chr>            
#>  1 17698898B67359070524975 Deh Bala     AFG        Afghanistan Eastern Mediterr…
#>  2 17698898B98443198567384 Gulran       AFG        Afghanistan Eastern Mediterr…
#>  3 17698898B82675281335003 Koshk        AFG        Afghanistan Eastern Mediterr…
#>  4 17698898B74585757664988 Chaparhar    AFG        Afghanistan Eastern Mediterr…
#>  5 17698898B84066352785355 Koshki Kohna AFG        Afghanistan Eastern Mediterr…
#>  6 17698898B31168234973124 Pachier Agam AFG        Afghanistan Eastern Mediterr…
#>  7 17698898B88962028797570 Kohsan       AFG        Afghanistan Eastern Mediterr…
#>  8 17698898B58246966451205 Khogayani    AFG        Afghanistan Eastern Mediterr…
#>  9 17698898B9118080572878  Shirzad      AFG        Afghanistan Eastern Mediterr…
#> 10 17698898B30811011988841 Ghoryan      AFG        Afghanistan Eastern Mediterr…
#> # ℹ 49,339 more rows

# Find the identifiers used for units in the Philippines
gtropic_adm2(WHO_ENTITY = "Philippines")
#> # A tibble: 86 × 5
#>    ADM2_ID                ADM2_NAME        ADM2_GROUP WHO_ENTITY  WHO_REGION    
#>    <chr>                  <chr>            <chr>      <chr>       <chr>         
#>  1 2640588B11393510524278 Abra             PHL        Philippines Western Pacif…
#>  2 2640588B29392713909073 Agusan del Norte PHL        Philippines Western Pacif…
#>  3 2640588B20221108717018 Agusan del Sur   PHL        Philippines Western Pacif…
#>  4 2640588B21218366338371 Aklan            PHL        Philippines Western Pacif…
#>  5 2640588B82697045094820 Albay            PHL        Philippines Western Pacif…
#>  6 2640588B32523752073662 Antique          PHL        Philippines Western Pacif…
#>  7 2640588B24165862747597 Apayao           PHL        Philippines Western Pacif…
#>  8 2640588B82822310999177 Aurora           PHL        Philippines Western Pacif…
#>  9 2640588B29921835743372 Basilan          PHL        Philippines Western Pacif…
#> 10 2640588B97763445928490 Bataan           PHL        Philippines Western Pacif…
#> # ℹ 76 more rows
# }
```

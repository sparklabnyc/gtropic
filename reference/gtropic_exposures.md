# List G-TROPIC exposure metrics

Summarizes the published wind, precipitation, and flooding exposure
metrics available in G-TROPIC. The returned aggregation is the default
used when a metric is summarized across its published records; it does
not recompute a published exposure value.

## Usage

``` r
gtropic_exposures()
```

## Value

A named list with `wind`, `precip`, and `flood` components. Each
component is a tibble with columns `metric`, `label`, `unit`,
`aggregate`, and `plot_supported`.

## Details

`plot_supported` identifies metrics currently supported by
[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).
`FLOODED_POP_CENTROID` is included because it documents a published
flood exposure, although it is not currently available for plotting.

## Examples

``` r
exposures <- gtropic_exposures()
exposures$flood
#> # A tibble: 3 × 5
#>   metric               label                      unit  aggregate plot_supported
#>   <chr>                <chr>                      <chr> <chr>     <lgl>         
#> 1 PROP_FLOODED_POP     Flooded population (propo… prop… max       TRUE          
#> 2 PROP_FLOODED_AREA    Flooded area (proportion,… prop… max       TRUE          
#> 3 FLOODED_POP_CENTROID Flooded population centro… logi… any       FALSE         
```

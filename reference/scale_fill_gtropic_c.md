# Color-blind-safe fill scales for G-TROPIC plots

Native ggplot2 viridis scales: inferno for wind, mako for precipitation,
and viridis for flooding when a metric is supplied. Missing values are
grey.

## Usage

``` r
scale_fill_gtropic_c(
  ...,
  metric = NULL,
  palette = NULL,
  direction = 1,
  name = NULL,
  na.value = "grey90"
)

scale_fill_gtropic_d(
  ...,
  metric = NULL,
  palette = NULL,
  direction = 1,
  name = NULL,
  na.value = "grey90"
)
```

## Arguments

- ...:

  Additional arguments passed to the ggplot2 viridis fill scale.

- metric:

  Optional published metric name, used for palette and title.

- palette:

  Viridis palette name; NULL chooses a hazard-specific palette.

- direction:

  Palette direction, either 1 or -1.

- name:

  Legend title; NULL uses the metric label and unit when available.

- na.value:

  Color for missing values.

## Value

A continuous or discrete ggplot2 fill scale.

## Examples

``` r
# \donttest{
scale_fill_gtropic_c(metric = "VMAX_SUST_MS")
#> <ScaleContinuous>
#>  Range:  
#>  Limits:    0 --    1
scale_fill_gtropic_d(name = "Exposed")
#> <ggproto object: Class ScaleDiscrete, Scale, gg>
#>     aesthetics: fill
#>     axis_order: function
#>     break_info: function
#>     break_positions: function
#>     breaks: waiver
#>     call: call
#>     clone: function
#>     dimension: function
#>     drop: TRUE
#>     expand: waiver
#>     fallback_palette: function
#>     get_breaks: function
#>     get_breaks_minor: function
#>     get_labels: function
#>     get_limits: function
#>     get_transformation: function
#>     guide: legend
#>     is_discrete: function
#>     is_empty: function
#>     labels: waiver
#>     limits: NULL
#>     make_sec_title: function
#>     make_title: function
#>     map: function
#>     map_df: function
#>     minor_breaks: waiver
#>     n.breaks.cache: NULL
#>     na.translate: TRUE
#>     na.value: grey90
#>     name: Exposed
#>     palette: function
#>     palette.cache: NULL
#>     position: left
#>     range: environment
#>     rescale: function
#>     reset: function
#>     train: function
#>     train_df: function
#>     transform: function
#>     transform_df: function
#>     super:  <ggproto object: Class ScaleDiscrete, Scale, gg>
# }
```

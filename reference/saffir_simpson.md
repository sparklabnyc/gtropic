# Saffir-Simpson tropical-cyclone wind categories

Provides the category thresholds and conventional display colours for
the Saffir-Simpson Hurricane Wind Scale. Thresholds are sustained wind
speeds in metres per second. Each threshold is the upper bound of its
category, except for `"Cat5"`, which has no upper bound.

## Usage

``` r
saffir_simpson()
```

## Value

A tibble with one row per category and columns `category`, `threshold`,
`unit`, and `color`. `threshold` is numeric and `"Cat5"` has an infinite
threshold.

## Examples

``` r
saffir_simpson()
#> # A tibble: 7 × 4
#>   category threshold unit  color  
#>   <chr>        <dbl> <chr> <chr>  
#> 1 TD              17 m/s   #6EC1EA
#> 2 TS              32 m/s   #5FF   
#> 3 Cat 1           42 m/s   #FFFFD9
#> 4 Cat 2           49 m/s   #FBDA8C
#> 5 Cat 3           58 m/s   #F89F59
#> 6 Cat 4           70 m/s   #F6738A
#> 7 Cat 5          Inf m/s   #A188FC
```

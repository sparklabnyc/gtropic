# Report the temporal coverage of G-TROPIC

Reports the calendar years represented in the historical and
current-year G-TROPIC datasets. This information can be used to define
feasible study periods before retrieving storm exposure data.

The historical collection contains completed years. When available, the
separate current-year collection is updated as the tropical cyclone
season progresses and may therefore represent provisional temporal
coverage.

## Usage

``` r
gtropic_years(version = ":latest")
```

## Arguments

- version:

  Dataset version or versions to inspect. The default, `":latest"`, uses
  the most recent published versions. See
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  for supported version formats.

## Value

A list of integer vectors with components `historical`, `current`, and
`all`. `all` is the sorted union of the two collections; `current` is
empty when no separate current-year dataset is available.

## See also

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)

## Examples

``` r
# \donttest{
coverage <- gtropic_years()
range(coverage$all)
#> [1] 1980 2026
# }
```

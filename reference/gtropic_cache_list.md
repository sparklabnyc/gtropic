# Inspect cached G-TROPIC files

`gtropic_cache_list()` reports the dataset file references recorded in
the cache index. `gtropic_cache_size()` reports the physical disk space
occupied by cached file content.

Cached content is identified by checksum and may be shared by more than
one dataset version. Consequently, multiple index entries can refer to
one file on disk.

## Usage

``` r
gtropic_cache_list()

gtropic_cache_size()
```

## Value

`gtropic_cache_list()` returns a tibble with one row per indexed dataset
file reference and columns `server`, `doi`, `version`,
`directory_label`, `label`, `md5`, `size_bytes`, and `fetched_at`.
`gtropic_cache_size()` returns a numeric scalar giving the physical
cache size in bytes, or `0` when the cache is empty.

## Examples

``` r
gtropic_cache_list()
#> # A tibble: 0 × 8
#> # ℹ 8 variables: server <chr>, doi <chr>, version <chr>, directory_label <chr>,
#> #   label <chr>, md5 <chr>, size_bytes <dbl>, fetched_at <dttm>
gtropic_cache_size()
#> [1] 0
```

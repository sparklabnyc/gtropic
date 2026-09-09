# Locate the G-TROPIC cache

Returns the directory used to store downloaded G-TROPIC source files
when persistent caching is enabled. Reusing these files can reduce data
transfer and retrieval time for repeated or overlapping analyses.

## Usage

``` r
gtropic_cache_dir()
```

## Value

A character scalar containing the cache directory path.

## Details

Unless changed through `options(gtropic.cache_dir = ...)` or
[`gtropic_cache_enable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md),
the path is the platform-specific user cache location returned by
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). Calling
this function reports the path but does not create it.

## See also

[`gtropic_cache_enable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md),
[`gtropic_cache_clear()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_clear.md)

## Examples

``` r
gtropic_cache_dir()
#> [1] "/home/runner/.cache/R/gtropic"
```

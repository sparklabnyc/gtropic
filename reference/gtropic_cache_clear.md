# Delete all cached G-TROPIC files

Removes all downloaded file content from the configured cache and clears
its index. This operation does not change whether caching is enabled.

## Usage

``` r
gtropic_cache_clear(confirm = TRUE)
```

## Arguments

- confirm:

  Logical. If `TRUE` (the default), confirmation is requested before
  deletion in an interactive R session. No prompt is issued in a
  non-interactive session. Set to `FALSE` to proceed without prompting.

## Value

Invisibly, the number of cached files selected for removal. Returns `0`
if the cache is empty or interactive confirmation is declined.

## Examples

``` r
# \donttest{
gtropic_cache_clear(confirm = FALSE)
#> ℹ Cache is already empty.
# }
```

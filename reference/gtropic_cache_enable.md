# Enable or disable persistent caching

`gtropic_cache_enable()` stores downloaded G-TROPIC source files for
reuse across R sessions. `gtropic_cache_disable()` stops using
persistent caching for subsequent retrievals but leaves existing files
unchanged.

## Usage

``` r
gtropic_cache_enable(dir = NULL)

gtropic_cache_disable()
```

## Arguments

- dir:

  Optional path for the persistent cache. If `NULL`, the current value
  returned by
  [`gtropic_cache_dir()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_dir.md)
  is used. A custom location may be useful when exposure files must be
  stored on a larger disk.

## Value

Invisibly, a character scalar containing the cache directory path.

## Details

Persistent caching is disabled by default. Enabling it sets the
`gtropic.cache_enabled` option for the current R session and creates the
cache directory if needed. Disabling it sets that option to `FALSE`; use
[`gtropic_cache_clear()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_clear.md)
to delete stored files.

## See also

[`gtropic_cache_dir()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_dir.md),
[`gtropic_cache_prune()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_prune.md)

## Examples

``` r
# \donttest{
# Use a temporary cache location for this session
gtropic_cache_enable(dir = file.path(tempdir(), "gtropic-cache"))
#> ✔ Cache enabled at /tmp/RtmplEywYK/gtropic-cache.
gtropic_cache_disable()
#> ℹ Cache disabled. Existing files are kept; use
#> `gtropic_cache_clear()` to remove them.
# }
```

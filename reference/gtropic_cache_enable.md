# Enable or disable caching

`gtropic_cache_enable()` enables storage of downloaded G-TROPIC source
files for reuse. `gtropic_cache_disable()` stops using the cache for
subsequent retrievals but leaves existing files unchanged.

## Usage

``` r
gtropic_cache_enable(dir = NULL, persist = FALSE)

gtropic_cache_disable(persist = FALSE)
```

## Arguments

- dir:

  Optional path for the cache. If `NULL`, the current value returned by
  [`gtropic_cache_dir()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_dir.md)
  is used. A custom location may be useful when exposure files must be
  stored on a larger disk.

- persist:

  Logical. Should this enabled or disabled state be used in future R
  sessions? Defaults to `FALSE`, so the change affects only the current
  session.

## Value

Invisibly, a character scalar containing the cache directory path.

## Details

Caching is enabled by default for each R session. These functions change
the `gtropic.cache_enabled` option for the current session. Set
`persist = TRUE` to use the same choice in future sessions; this writes
the setting under `tools::R_user_dir("gtropic", "config")`. The package
never edits `.Rprofile`. Enabling creates the cache directory if needed;
disabling does not delete cached files. Use
[`gtropic_cache_clear()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_clear.md)
to delete them.

## See also

[`gtropic_cache_dir()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_dir.md),
[`gtropic_cache_prune()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_prune.md)

## Examples

``` r
# \donttest{
# Bypass the cache for the current session
gtropic_cache_disable()

# Keep that choice for future sessions
gtropic_cache_disable(persist = TRUE)
# }
```

# Prune cached G-TROPIC files by size or age

Removes cache entries according to their original retrieval time.
Entries older than the specified age are removed, and the oldest
remaining entries are removed as needed to satisfy the size limit. When
both criteria are supplied, an entry is removed if required by either
criterion. With no arguments, nothing is removed.

If identical file content is referenced by multiple dataset versions,
the physical file is retained until no remaining cache entry references
it.

## Usage

``` r
gtropic_cache_prune(max_size_gb = NULL, older_than_days = NULL)
```

## Arguments

- max_size_gb:

  Optional maximum indexed cache size in gigabytes. The oldest entries
  are removed until the indexed size is at or below this value.

- older_than_days:

  Optional age threshold in days. Entries fetched before the resulting
  cutoff are removed.

## Value

Invisibly, a tibble containing the removed cache-index entries. An empty
tibble is returned when no entries are removed.

## Examples

``` r
# \donttest{
gtropic_cache_prune(max_size_gb = 5, older_than_days = 90)
#> ℹ Cache is empty; nothing to prune.
# }
```

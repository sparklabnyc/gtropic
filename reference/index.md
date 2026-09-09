# Package index

## Retrieval

The cornerstone data retrieval function and its associated helpers
accessing query metadata.

- [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  : Retrieve tropical cyclone exposure data from G-TROPIC
- [`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
  [`gtropic_filters()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
  [`gtropic_warnings()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
  : Extract metadata from a G-TROPIC result
- [`print(`*`<gtropic_data>`*`)`](https://sparklabnyc.github.io/gtropic/reference/print.gtropic_data.md)
  : Print a G-TROPIC data result
- [`summary(`*`<gtropic_data>`*`)`](https://sparklabnyc.github.io/gtropic/reference/summary.gtropic_data.md)
  : Summarize a G-TROPIC data result
- [`print(`*`<gtropic_manifest>`*`)`](https://sparklabnyc.github.io/gtropic/reference/print.gtropic_manifest.md)
  : Print a G-TROPIC version manifest

## Discovery

Functions to explore the available administrative units, storms, years
and column definitions.

- [`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md)
  : List available second-level administrative units
- [`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md)
  : List available tropical cyclones
- [`gtropic_years()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_years.md)
  : Report the temporal coverage of G-TROPIC
- [`gtropic_codebook()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_codebook.md)
  : Retrieve the G-TROPIC data codebook

## Cache

Optional, opt-in storage of downloaded files between R sessions.

- [`gtropic_cache_dir()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_dir.md)
  : Locate the G-TROPIC cache
- [`gtropic_cache_enable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md)
  [`gtropic_cache_disable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md)
  : Enable or disable persistent caching
- [`gtropic_cache_list()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_list.md)
  [`gtropic_cache_size()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_list.md)
  : Inspect cached G-TROPIC files
- [`gtropic_cache_clear()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_clear.md)
  : Delete all cached G-TROPIC files
- [`gtropic_cache_prune()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_prune.md)
  : Prune cached G-TROPIC files by size or age

## Configuration

Options controlling the server, dataset identifiers, timeouts and the
large query download guardrail.

- [`gtropic_options`](https://sparklabnyc.github.io/gtropic/reference/gtropic_options.md)
  : Options used by gtropic

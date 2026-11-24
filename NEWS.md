# gtropic 0.0.0.9002

## Breaking changes

* `summary()` of a `gtropic_data` result now returns an object of class
  `summary.gtropic_data`, which prints the same overview as before. It used
  to print and return the result itself, so `y <- summary(x)` now gives the
  summary, not the data.

* Renamed exported functions, with no aliases:
  * `gtropic_warnings()` is now `gtropic_diagnostics()`. It returns retained
    diagnostics, not warnings, and the old name was easy to confuse with
    `base::warnings()`.
  * `saffir_simpson()` is now `gtropic_saffir_simpson()`, so every exported
    function except the ggplot2-style `theme_` and `scale_` helpers starts
    with `gtropic_`.
  * `gtropic_exposures()` is now `gtropic_metrics()`. It lists the metric
    names accepted by `gtropic_plot(metric = )`.
* `gtropic_basemap()` takes `what` first: `gtropic_basemap(what = "countries",
  scale = "medium")`.

* `gtropic_storms()` and `gtropic_excluded_storms()` no longer have a `year`
  argument; use `date_range`, which now also accepts a single value
  (`date_range = 2005` selects storms that formed in 2005). When both were
  supplied, `date_range` silently won.
* `confirm` in `gtropic_data()` is a plain logical with default `TRUE`
  (`NULL` meant the same as `TRUE` and is no longer accepted).
* Logical arguments (`quiet`, `reproject`, `offline`, `make_valid`, `confirm`,
  `persist`, and `cache`, which may also be `NULL`) must be a single `TRUE` or
  `FALSE`. Previously `quiet = "yes"` was silently treated as `FALSE` and
  `reproject` was not checked.

* `gtropic.*` options are checked when they are read. A value of the wrong
  type or out of range (for example `options(gtropic.max_tries = 0)` or
  `options(gtropic.cache_enabled = "yes")`) is now an error that names the
  option, instead of being coerced or silently treated as `FALSE`. The
  defaults, `?gtropic_options` and the package's load-time settings now come
  from one definition.

* `gtropic_data()` no longer has an `as` argument. `as = "arrow"` never kept
  data on disk: every table was collected into memory and returned as a
  tibble either way. Results are always tibbles; convert a table with
  `arrow::as_arrow_table()` if you want Arrow.

* gtropic now requires R 4.2.0 or later. Current releases of the Arrow R
  package need R 4.2, so an R 4.1 installation could not install gtropic's
  dependencies anyway; the declared minimum now matches what is tested.

* The cache index, the offline discovery snapshot and the persisted cache
  setting are now stored as JSON (`index.json`, `discovery.json` and
  `config.json`). Files written by earlier development versions
  (`index.rds`, `discovery.rds`, `cache.rds`) are ignored and never read.
  Testers can delete their old cache directory (`gtropic_cache_dir()`) and
  the old `cache.rds` under `tools::R_user_dir("gtropic", "config")`.

## New features

* `gtropic_manifest_write()` and `gtropic_manifest_read()` save a manifest as
  a JSON file and read it back. JSON is the safe way to share a manifest:
  reading it cannot run code, unlike `readRDS()` on an `.rds` file from
  someone else, and it is readable and diffs cleanly in version control.
  Reading checks every field and then applies the replay validation, so an
  edited or foreign file is rejected with a `gtropic_error_manifest`.

* The cache now manages itself (CRAN policy on user cache files). The first
  file cached in a session prints where the cache is. After each retrieval
  that adds to the cache, files downloaded more than
  `getOption("gtropic.cache_max_age_days")` (new option, default 180) days ago
  and files superseded by a newer version retrieved with `version = ":latest"`
  are removed, then the least recently downloaded files until the cache is at
  most `getOption("gtropic.cache_max_gb")` (new option, default 10) GB. Files
  used by that retrieval are never removed; `Inf` switches a limit off.

* New option `gtropic.quiet` (default `FALSE`): `options(gtropic.quiet = TRUE)`
  silences progress and informational messages from every call in a session.
  It is the default of `quiet` in `gtropic_data()` and `gtropic_plot()`, and
  it also silences the messages of `gtropic_cache_clear()` and
  `gtropic_cache_prune()`.

* Errors and warnings that scripts may want to handle now have classes, all
  inheriting from `gtropic_error` or `gtropic_warning`: network failures
  (`gtropic_error_network`), checksum mismatches, filters that match nothing,
  unconfirmed large downloads, invalid manifests, and the availability,
  polygon, missing-precipitation and ambiguous-storm-name warnings. See
  `?gtropic_conditions`.
* Accessors such as `gtropic_manifest()` give an informative error, not a
  `stopifnot()` message, when passed something other than a `gtropic_data`
  object.

* `gtropic_example()` returns a small synthetic `gtropic_data` object, so
  examples and the plotting functions can be tried offline. Its values are
  invented.

## Improvements

* Queries use much less memory. Rows are filtered to the requested storms and
  ADM2 units, and files are read only for the columns a query uses, before
  anything is converted to R. Each file is filtered one row group at a time,
  so a whole year-file of global precipitation is never decoded at once. The
  returned tables are unchanged, row order included.

* Data is downloaded with `httr2` directly; the `dataverse` package is no
  longer a dependency (`httr2 (>= 1.2.0)` is now imported). Each file is
  streamed to disk as it arrives and then checked and read from disk, so a
  download is never held in memory whole. Fewer requests are made: the
  `dataverse` client looked up the dataset and the file's size and type before
  every download, and reading a dataset's version number no longer transfers
  its whole file list.

* Only transient failures are retried: a lost or refused connection, a
  timeout, and HTTP 408, 429 and 5xx responses, up to
  `getOption("gtropic.max_tries")` attempts, waiting as a server's
  `Retry-After` asks (at most 60 seconds). A permanent error, such as a
  mistyped DOI or a version that does not exist, is reported at once instead
  of after two retries and three seconds of waiting.

* Fewer dependencies: `stringr` is no longer imported, and the unused
  suggested packages `curl`, `memoise` and `scales` are gone. User-supplied
  plot captions are now wrapped with base `strwrap()`, which fills lines
  greedily, so their line breaks can fall in different places, and blank-line
  paragraph breaks are kept.

* Data-availability warnings for recent periods are shown once per session
  for each kind of warning; the full detail is still kept in
  `gtropic_diagnostics()`.
* A multi-year `polygon` query now reports that membership is year-dependent
  as a message (silenced by `quiet`). It warns only when an ADM2 unit actually
  moves in or out of the polygon across the years evaluated.
* The large-download guardrail and `gtropic_cache_clear()` ask for
  confirmation the same way.

* `format()` methods for results, summaries, manifests and filters return
  the lines their `print()` methods display, as a character vector.

* A `gtropic_data()` call that does not name `tables` now works with only the
  required packages installed. When `sf` or `sfarrow` is missing, the
  `geometry` table is left out and a message says so once per session.
  Naming `"geometry"` in `tables` still requires both packages.

## Bug fixes

* When a published file lacks a column the query asks for, the warning that
  says so is now shown. Before, building the warning failed with a
  pluralisation error that stopped the query.

* A failed request is reported once. When automatic dataset discovery fails,
  gtropic still falls back to the configured DOI options, but warns only if
  the fallback works. If it fails too, for example with no network
  connection, the result is a single `gtropic_error_network` error instead of
  a warning followed by an error.

* `options(gtropic.timeout = )` (default 600 seconds) now takes effect; it
  was documented but never applied. A metadata request that takes longer is
  abandoned and retried, and a download is abandoned when no data arrives for
  that long, so a large file on a slow connection is not cut off while it is
  still arriving. `Inf` waits indefinitely.

* Manifest download times are always stored in UTC. On R 4.2 they kept the
  session's time zone attribute (the instants were the same).

* The warning for ADM2 units that move in or out of a `polygon` across years
  no longer fails with "Cannot pluralize without a quantity" instead of
  warning.

* `gtropic_saffir_simpson()` writes the tropical-storm colour as `"#55FFFF"`
  instead of the shorthand `"#5FF"` (the same colour). R before 4.3.0 does not
  accept three-digit colour codes, so every map drawn with the default key
  failed on R 4.2.

* A pinned `version` (for example `gtropic_data(version = "2.0")`) now reads
  the files of that version. Earlier development versions always read the
  files of the latest version and recorded them in the manifest under the
  pinned version number, so results obtained with a pinned version before
  this fix should be re-run.

## Security

* `arrow (>= 17.0.0)` is now required. Earlier versions of the Arrow R
  package could run code embedded in the metadata of an untrusted Parquet
  file (CVE-2024-52338), and gtropic reads Parquet files fetched from the
  network.

* Printing or summarising a result or a manifest no longer evaluates code
  contained in its dataset DOI, version or server strings. These strings come
  from the Dataverse server or from a shared manifest, and `cli` treated them
  as templates, so a crafted manifest could run R code when it was printed. A
  DOI or version containing `{` or `}` now prints literally instead of causing
  an error. Error messages that list unmatched filter values and suggested
  names are protected in the same way.

* Checksums are validated before they are used to name files in the cache.
  A checksum that is not a hexadecimal MD5, SHA-1, SHA-256 or SHA-512 digest
  is treated as "no checksum published" when it comes from a file listing,
  and a cache index entry with such a checksum can no longer make the cache
  read or delete a file outside its own directory.

* Downloads are bounded while they transfer. A server that sends more than a
  file's listed size (plus 1 MB or 1 %, whichever is more), or more than 8 GB
  for a file listed without a size, is stopped; the partial file is removed
  and an error of class `gtropic_error_download_size` is raised. The
  large-download confirmation relies on the listed sizes, which are now
  enforced.

* When Dataverse answers a download with a redirect to its storage (Harvard
  Dataverse redirects to Amazon S3), the redirect is followed without
  `DATAVERSE_KEY`, which belongs to the Dataverse host only. A redirect from
  HTTPS to plain HTTP is refused.

* `DATAVERSE_KEY` is now sent only to the configured server
  (`getOption("gtropic.server")`). Requests to any other host, for example
  when replaying a manifest that names another server, are made without the
  token, and a warning explains how to authenticate against that host.
* `options(gtropic.server = )` must be a bare host name with an optional
  port. Paths, queries, user information and spaces are rejected with an
  error that names the option. A leading `https://` and trailing slashes are
  still stripped.
* The fallback server, used when `gtropic.server` is unset, is now
  `dataverse.harvard.edu`, the same as the default set at load time (it was
  `demo.dataverse.org`). Transport errors name the host that was actually
  contacted.

* Replaying a manifest (`gtropic_data(manifest = )`) now takes only the
  data-shaping query from it. `cache`, `as`, `confirm`, `quiet` and `offline`
  are always the caller's choice, so a manifest can no longer switch off the
  large-download confirmation or silence messages.
* Manifests are now checked for content as well as shape before replay: the
  format version, a valid server host name, positive whole file ids, plain
  relative file and directory names, hexadecimal checksums, Dataverse-style
  DOIs and versions, and a resolved query with the same types a direct call
  produces. Manifests made by `gtropic_manifest()` always pass; hand-edited
  ones with out-of-contract values are rejected with an error of class
  `gtropic_error_manifest`.
* Replaying a manifest that names a server other than
  `getOption("gtropic.server")` warns once, naming both hosts, before any
  request is made.

* Cache metadata is no longer read with `readRDS()`. Reading an R
  serialisation file that someone else can write may run their code
  (CVE-2024-27322 on R < 4.4.0), and a cache can sit on a shared disk. The
  JSON files are checked field by field when read; an invalid file is ignored
  with a warning (the index then counts as empty, and an invalid discovery
  snapshot cannot be used offline).
* Cache and configuration directories are created readable and writable by
  the user only. On Unix, a cache directory that other users can write to is
  reported once per session.

* Cache files are written through temporary files with unpredictable names
  and checked renames. A file planted at a predictable temporary name can no
  longer redirect a cache write, a failed write is reported instead of being
  silently ignored, and temporary files left by interrupted writes are
  removed after a day.

* Interactive maps (`gtropic_plot(interactive = TRUE)`) now HTML-escape
  legend labels, which can come from user-supplied `wind_categories` or
  `track_map_key` category names, and the wind unit in track popups. Labels
  that intentionally contained HTML now show literally.

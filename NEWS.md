# gtropic 0.0.0.9002

## Breaking changes

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

* `gtropic_example()` returns a small synthetic `gtropic_data` object, so
  examples and the plotting functions can be tried offline. Its values are
  invented.

## Bug fixes

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

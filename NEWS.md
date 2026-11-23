# gtropic 0.0.0.9002

## New features

* `gtropic_example()` returns a small synthetic `gtropic_data` object, so
  examples and the plotting functions can be tried offline. Its values are
  invented.

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

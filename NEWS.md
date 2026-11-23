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

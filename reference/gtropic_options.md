# Options used by gtropic

`gtropic` is configured entirely through
[`options()`](https://rdrr.io/r/base/options.html). Setting these is how
you point the package at a different Dataverse installation – for
example moving from the demonstration server to a production repository
– without needing a package update.

## Value

These are options, not a function; nothing is returned. This topic
exists for documentation only.

## Details

|  |  |  |
|----|----|----|
| Option | Default | Meaning |
| `gtropic.server` | `"demo.dataverse.org"` | Dataverse host (HTTPS only). |
| `gtropic.doi_historical` | `"doi:10.70122/FK2/SWEYST"` | DOI of the historical dataset. |
| `gtropic.doi_current` | `"doi:10.70122/FK2/DDSYYD"` | DOI of the current-year dataset. Used only as a fallback if automatic discovery fails. |
| `gtropic.collection` | `"gtropic"` | Dataverse collection (alias) searched during discovery. |
| `gtropic.cache_enabled` | `FALSE` | Whether downloaded files are kept on disk between sessions. |
| `gtropic.cache_dir` | `tools::R_user_dir("gtropic", "cache")` | Where cached files live. |
| `gtropic.confirm_threshold_gb` | `1` | Download size, in gigabytes, above which you are asked to confirm. |
| `gtropic.timeout` | `600` | Per-request timeout in seconds. |
| `gtropic.max_tries` | `3` | Maximum attempts per request before giving up. |

A "cache" here means a folder of previously downloaded files on your own
computer. It is switched off by default, and turning it on writes only
to a standard per-user location chosen by R (see
[`gtropic_cache_enable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md)).

## Examples

``` r
# Point the package at a different installation:
if (FALSE) { # \dontrun{
options(
  gtropic.server = "dataverse.harvard.edu",
  gtropic.doi_historical = "doi:10.7910/DVN/EXAMPLE"
)
} # }

# Inspect the current settings:
getOption("gtropic.server")
#> [1] "demo.dataverse.org"
```

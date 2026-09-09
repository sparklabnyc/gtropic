# gtropic

## Overview

The R package `gtropic` provides a retrieval, filtering, and plotting
client for the live G-TROPIC collection of tropical cyclone exposure
datasets, published and updated weekly on Harvard Dataverse. Exposure
datasets include wind, precipitation, and flooding data at second-level
administrative unit (ADM2) resolution for every ADM2 and International
Best Track Archive for Climate Stewardship (IBTrACS) recorded storm
globally, from 1980 to the present. Key features include:

- A cornerstone query function:
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
- Discovery helpers for exploring the available vocabulary of
  administrative units and storms
- Version manifests for reproducible re-retrieval
- An optional opt-in on-disk cache

## Installation

The development version of `gtropic` can be installed from GitHub with:

``` r

# install.packages("pak")
pak::pak("sparklabnyc/gtropic")
```

`gtropic` can then be loaded and attached in your current R session as
usual with:

``` r

library(gtropic)
```

## Usage

``` r

library(gtropic)

# Wind, precipitation, flooding and population for the USA, for storms that formed
# during 2005.
usa_05 <- gtropic_data(WHO_ENTITY = "United States", date_range = c(2005, 2005))
#> Discovering datasets in collection "gtropic".
#> Downloading 'metadata/adm2.parquet'.
#> Downloading '02_wind/storm_metadata/2005.parquet'.
#> Retrieving 8 files (133.0 MB) ; .
#> Downloading '02_wind/exposures/2005.parquet'.
#> Downloading '02_wind/zero_pairs/2005.parquet'.
#> Downloading '04_precip/2005.parquet'.
#> Downloading '03_flood/2005.parquet'.
#> Downloading '01_pop/2005.parquet'.
#> Downloading 'metadata/codebook.json'.

usa_05
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────
#> • Historical dataset: version "3.0"
#> • Current-year dataset: version "4.0"
#> • Genesis dates: "2005-01-01" to "2005-12-31"
#> • Storms: 75
#> • ADM2 units: 3231
#> 
#> ── Tables 
#> • wind: 45899 rows x 12 columns
#> • precip: 32739 rows x 3 columns
#> • flood: 421 rows x 7 columns
#> • pop: 3231 rows x 6 columns
#> • storm_metadata: 75 rows x 4 columns
#> • adm2: 3231 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 239094 rows x 11 columns
#> 
#> ℹ Use `summary()` for detail.
```

The
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function returns a named list of tables that by default includes:

- **`wind`** – one row per administrative unit per storm, for pairs
  where wind exposure was non-zero. This is the core exposure table.
- **`precip`** – one row per administrative unit per local calendar
  date, for dates falling near a storm’s passage for that ADM2.
- **`flood`** – one row per observed flood event.
- **`pop`** – population and population-weighted centroid data, per ADM2
  per year.
- **`adm2`** – the lookup table of administrative units.
- **`codebook`** – the dataset’s own documentation, as a nested list.
- **`links`** – see below.

Three further tables are available on request via the `tables` argument:
`zero_pairs` (unit-storm pairs with no measurable wind), `tracks` (raw
storm track observations, for plotting purposes), and `geometry`
(administrative boundaries, again for plotting purposes, and requiring
the `sf` package).

Every result also carries a **manifest** providing a record of which
published files went into the result, including their paths, dataset
versions, sizes and checksums. Saving it lets you reproduce the same
retrieval months later, even if the dataset has since been revised.

``` r

saveRDS(gtropic_manifest(usa_05), "usa-2005-manifest.rds")
```

## Getting help

Extensive documentation is available on our pkgdown website as well as
offline within R. You can see the `gtropic` reference manual in R with:

``` r

help("gtropic")
```

A number of vignettes are available and can be browsed in R using:

``` r

browseVignettes("gtropic")
```

We recommend reading the vignettes in the following order:

- [`vignette("getting-started")`](https://sparklabnyc.github.io/gtropic/articles/getting-started.md)
  introduces the cornerstone
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  function, running your first query, and how to navigate the resulting
  data.
- [`vignette("filtering")`](https://sparklabnyc.github.io/gtropic/articles/filtering.md)
  covers how all the spatio-temporal/storm filters in
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  combine, with a worked example.
- [`vignette("linkage")`](https://sparklabnyc.github.io/gtropic/articles/linkage.md)
  documents how to link precipitation/flood data to storm data and using
  the `links` table to perform joins without double-counting exposure
  rows.
- [`vignette("versions-and-caching")`](https://sparklabnyc.github.io/gtropic/articles/versions-and-caching.md)
  walks through reproducibility and download management best practices
  in `gtropic`.

## Citation

Please cite both the package and the underlying datasets. The dataset
DOIs and versions used by any given result are recorded in its manifest:

``` r

gtropic_manifest(usa_05)
#> 
#> ── gtropic version manifest ────────────────────────────────────────────────────
#> • Server: "demo.dataverse.org"
#> • Historical: "doi:10.70122/FK2/SWEYST" (version "3.0")
#> • Current-year: "doi:10.70122/FK2/DDSYYD" (version "4.0")
#> • Files recorded: 8
#> • Total size: 133.0 MB
#> • Retrieved: 2026-09-08 16:57:17 EDT
#> • Package version: "0.0.0.9000"
#> 
#> ℹ Pass this object as `manifest` to `gtropic_data()` to
#> reproduce the same retrieval.
```

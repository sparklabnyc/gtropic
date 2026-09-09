# Getting started with gtropic

This vignette walks through installing the `gtropic` package, retrieving
your first G-TROPIC dataset, and understanding what comes back.

## What is G-TROPIC?

G-TROPIC is a multi-exposure, live (updated weekly) dataset spanning
global tropical storms from 1980 onward, combining IBTrACS storm track
data, GHSL population rasters, geoBoundaries ADM2 administrative units,
Groundsource flood polygons, and ERA5-Land precipitation. G-TROPIC
adheres to FAIR data principles and is publicly available via Harvard
Dataverse, with versioned updates that incorporate recent storm and
precipitation data once a week. At ADM2 resolution worldwide for
compatibility with health outcome and other data, G-TROPIC provides
population totals and population-weighted centroids, storm exposure
metrics from both best-track and wind-radii methods, flood exposure
metrics, and daily local precipitation totals, along with ADM2 metadata
for region and WHO classifications.

For enhanced accessibility, we developed this open-source R package,
`gtropic`, for easy retrieval, exploration, and mapping of G-TROPIC data
directly in R.

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

## Data availablity

First, let’s get oriented by running some of the `gtropic` helper
functions that let us know what G-TROPIC data is available in the first
place. Doing so will help us define the specific query we later send to
the G-TROPIC database, so we can retrieve only the data we need, and
nothing more (to save on memory and disk space).

We can examine the available years of data using:

``` r

gtropic_years()
#> $historical
#>  [1] 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994
#> [16] 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
#> [31] 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
#> [46] 2025
#> 
#> $current
#> [1] 2026
#> 
#> $all
#>  [1] 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994
#> [16] 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
#> [31] 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
#> [46] 2025 2026
```

The available administrative units in the dataset with:

``` r

adm2s <- gtropic_adm2()
adm2s
#> # A tibble: 49,349 × 5
#>    ADM2_ID                 ADM2_NAME    ADM2_GROUP WHO_ENTITY  WHO_REGION       
#>    <chr>                   <chr>        <chr>      <chr>       <chr>            
#>  1 17698898B67359070524975 Deh Bala     AFG        Afghanistan Eastern Mediterr…
#>  2 17698898B98443198567384 Gulran       AFG        Afghanistan Eastern Mediterr…
#>  3 17698898B82675281335003 Koshk        AFG        Afghanistan Eastern Mediterr…
#>  4 17698898B74585757664988 Chaparhar    AFG        Afghanistan Eastern Mediterr…
#>  5 17698898B84066352785355 Koshki Kohna AFG        Afghanistan Eastern Mediterr…
#>  6 17698898B31168234973124 Pachier Agam AFG        Afghanistan Eastern Mediterr…
#>  7 17698898B88962028797570 Kohsan       AFG        Afghanistan Eastern Mediterr…
#>  8 17698898B58246966451205 Khogayani    AFG        Afghanistan Eastern Mediterr…
#>  9 17698898B9118080572878  Shirzad      AFG        Afghanistan Eastern Mediterr…
#> 10 17698898B30811011988841 Ghoryan      AFG        Afghanistan Eastern Mediterr…
#> # ℹ 49,339 more rows
```

Or, for example, the available administrative units in a specific
country:

``` r

adm2s_usa <- gtropic_adm2(WHO_ENTITY = "United States")
adm2s_usa
#> # A tibble: 3,231 × 5
#>    ADM2_ID                 ADM2_NAME  ADM2_GROUP WHO_ENTITY    WHO_REGION
#>    <chr>                   <chr>      <chr>      <chr>         <chr>     
#>  1 52423323B51867153498623 Highland   USA        United States Americas  
#>  2 52423323B67588574079428 Alpine     USA        United States Americas  
#>  3 52423323B97123089850170 Escambia   USA        United States Americas  
#>  4 52423323B54167037237770 Lawrence   USA        United States Americas  
#>  5 52423323B18579387327768 Wayne      USA        United States Americas  
#>  6 52423323B71362276548625 Tishomingo USA        United States Americas  
#>  7 52423323B19966710463863 Sanders    USA        United States Americas  
#>  8 52423323B67077901806593 Albany     USA        United States Americas  
#>  9 52423323B68632918387185 Potter     USA        United States Americas  
#> 10 52423323B68605261841500 Greene     USA        United States Americas  
#> # ℹ 3,221 more rows
```

Similarly, we can also get a list of all the available storms in the
dataset. By default the function returns all storms from 1980 onward,
but if we pass in a `year`, then we can get the list of storms for that
given `year`:

``` r

storms_25 <- gtropic_storms(year = 2025)
storms_25
#> # A tibble: 77 × 4
#>    STORM_ID      STORM_NAME   USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>        <chr>       <date>                
#>  1 2025017S18122 Sean-2025    SH102025    2025-01-17            
#>  2 2025024S11082 Faida-2025   SH112025    2025-01-23            
#>  3 2025029S25043 Elvis-2025   SH122025    2025-01-28            
#>  4 2025034S19166 Unnamed-2025 SH152025    2025-02-02            
#>  5 2025039S14125 Zelia-2025   SH172025    2025-02-08            
#>  6 2025052S14148 Alfred-2025  SH182025    2025-02-21            
#>  7 2025052S23038 Honde-2025   SH232025    2025-02-21            
#>  8 2025054S13182 Rae-2025     SH192025    2025-02-22            
#>  9 2025054S17051 Garance-2025 SH222025    2025-02-23            
#> 10 2025056S16171 Seru-2025    SH212025    2025-02-24            
#> # ℹ 67 more rows
```

The `STORM_GENESIS_DATE_UTC` column in that last result is the date used
by every temporal filter in the package – see the “Selecting storms by
date” section below, and the `filtering` vignette for the full
treatment.

One of the most important pieces of metadata is the G-TROPIC codebook,
which can be obtained with:

``` r

cb <- gtropic_codebook()
names(cb)
#> [1] "codebook_version" "format"           "created_for"      "read_in_r"       
#> [5] "notes"            "raw_sources"      "datasets"
```

The returned codebook object contains all the information needed to
understand the retrieved datasets, including but not limited to their
columns, what those columns measure, and the original provenance of the
data, both in terms of the raw data sources, as well as the G-TROPIC
backend pipeline scripts that generated that data.

## A first query

The cornerstone function of the `gtropic` package is the
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function. Let’s start by using it to retrieve all available G-TROPIC
data associated with storms that began in calendar year 2025, for only
those ADM2s in the USA:

``` r

usa_25 <- gtropic_data(
  WHO_ENTITY = "United States",
  date_range = c(2025, 2025)
)
#> Downloading 'metadata/adm2.parquet'.
#> Downloading '02_wind/storm_metadata/2025.parquet'.
#> Retrieving 8 files (156.9 MB) ; .
#> Downloading '02_wind/exposures/2025.parquet'.
#> Downloading '02_wind/zero_pairs/2025.parquet'.
#> Downloading '04_precip/2025.parquet'.
#> Downloading '03_flood/2025.parquet'.
#> Downloading '01_pop/2025.parquet'.
#> Downloading '01_pop/2026.parquet'.
#> Downloading 'metadata/codebook.json'.

usa_25
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • Historical dataset: version "3.0"
#> • Current-year dataset: version "4.0"
#> • Genesis dates: "2025-01-01" to "2025-12-31"
#> • Storms: 77
#> • ADM2 units: 3231
#> 
#> ── Tables 
#> • wind: 23605 rows x 12 columns
#> • precip: 4272 rows x 3 columns
#> • flood: 661 rows x 7 columns
#> • pop: 6462 rows x 6 columns
#> • storm_metadata: 77 rows x 4 columns
#> • adm2: 3231 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 248787 rows x 11 columns
#> 
#> ℹ Use `summary()` for detail.
```

Printing the result gives a compact summary: which version of the data
you retrieved, the span of storm genesis dates, how many storms and
administrative units were selected, and the size of each table returned.

But, for more detail, including resolved filter settings and any
warnings raised, we can use:

``` r

summary(usa_25)
#> 
#> ── G-TROPIC data summary ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Sources ──
#> 
#> • Server: "demo.dataverse.org"
#> • Historical: "doi:10.70122/FK2/SWEYST" v3.0
#> • Current-year: "doi:10.70122/FK2/DDSYYD" v4.0
#> • Files retrieved: 9 (158.5 MB)
#> • Retrieved at: 2026-09-08 16:40:15 EDT
#> 
#> ── Tables ──
#> 
#> • wind: 23605 rows x 12 columns
#> • precip: 4272 rows x 3 columns
#> • flood: 661 rows x 7 columns
#> • pop: 6462 rows x 6 columns
#> • storm_metadata: 77 rows x 4 columns
#> • adm2: 3231 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 248787 rows x 11 columns
#> 
#> ── Resolved filters ──
#> 
#> • ADM2 units: 3231
#> • Storms: 77
#> • Genesis window: 2025-01-01 to 2025-12-31
#> • Precipitation window: -2/ +1 days within 500 km
#> • Flood window: -2/+1 days within 500 km (match: "start")
```

## The `gtropic_data` object

A `gtropic_data` object is just a named list of data frames, behaving
like any other named list:

``` r

names(usa_25)
#> [1] "wind"           "precip"         "flood"          "pop"           
#> [5] "storm_metadata" "adm2"           "codebook"       "links"
```

``` r

usa_25$wind
#> # A tibble: 23,605 × 12
#>    ADM2_ID  STORM_ID LOCAL_DATETIME_STORM…¹ LOCAL_DATETIME_MAX_W…² STORM_DIST_KM
#>    <chr>    <chr>    <chr>                  <chr>                          <dbl>
#>  1 5242332… 2025180… 2025-06-28T14:00:00-0… 2025-06-29T20:15:00-0…         2158.
#>  2 5242332… 2025185… 2025-07-06T14:00:00-0… 2025-07-06T05:00:00-0…          152.
#>  3 5242332… 2025215… 2025-08-02T14:00:00-0… 2025-08-02T14:00:00-0…          506.
#>  4 5242332… 2025223… 2025-08-20T22:30:00-0… 2025-08-21T00:30:00-0…          745.
#>  5 5242332… 2025261… 2025-09-22T11:00:00-0… 2025-09-23T14:00:00-0…         1771.
#>  6 5242332… 2025270… 2025-09-30T09:45:00-0… 2025-09-30T06:15:00-0…          743.
#>  7 5242332… 2025280… 2025-10-11T08:00:00-0… 2025-10-10T20:30:00-0…         1985.
#>  8 5242332… 2025294… 2025-10-30T14:00:00-0… 2025-10-31T00:45:00-0…         1135.
#>  9 5242332… 2025185… 2025-07-04T02:00:00-0… 2025-07-05T22:00:00-0…         1943.
#> 10 5242332… 2025215… 2025-08-03T17:00:00-0… 2025-08-03T06:30:00-0…         1789.
#> # ℹ 23,595 more rows
#> # ℹ abbreviated names: ¹​LOCAL_DATETIME_STORM_CLOSEST, ²​LOCAL_DATETIME_MAX_WIND
#> # ℹ 7 more variables: VMAX_SUST_MS <dbl>, SUST_DUR_MINUTES <dbl>,
#> #   VMAX_GUST_MS <dbl>, GUST_DUR_MINUTES <dbl>, WIND_RADII_VMAX_SUST_MS <dbl>,
#> #   WIND_RADII_SUST_DUR_MINUTES <dbl>, LOCAL_DATE_STORM_CLOSEST <chr>
```

By default, the
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function returns:

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

## The links table

`links` is returned whenever any storm-linked table is. It has one row
per ADM2-storm pair:

``` r

usa_25$links
#> # A tibble: 248,787 × 11
#>    STORM_ID      ADM2_ID      LOCAL_DATE_STORM_CLO…¹ STORM_DIST_KM WIND_EXPOSURE
#>    <chr>         <chr>        <date>                         <dbl> <lgl>        
#>  1 2025017S18122 52423323B10… 2025-01-17                    17418. FALSE        
#>  2 2025024S11082 52423323B10… 2025-02-04                    14774. FALSE        
#>  3 2025029S25043 52423323B10… 2025-01-29                    14506. FALSE        
#>  4 2025034S19166 52423323B10… 2025-02-04                    12678. FALSE        
#>  5 2025039S14125 52423323B10… 2025-02-11                    17173. FALSE        
#>  6 2025052S14148 52423323B10… 2025-02-25                    14013. FALSE        
#>  7 2025052S23038 52423323B10… 2025-02-25                    13996. FALSE        
#>  8 2025054S13182 52423323B10… 2025-02-22                    11485. FALSE        
#>  9 2025054S17051 52423323B10… 2025-02-25                    15020. FALSE        
#> 10 2025056S16171 52423323B10… 2025-02-24                    12713. FALSE        
#> # ℹ 248,777 more rows
#> # ℹ abbreviated name: ¹​LOCAL_DATE_STORM_CLOSEST
#> # ℹ 6 more variables: PRECIP_IN_FIELD <lgl>, PRECIP_WINDOW_START <date>,
#> #   PRECIP_WINDOW_END <date>, FLOOD_IN_FIELD <lgl>, FLOOD_WINDOW_START <date>,
#> #   FLOOD_WINDOW_END <date>
```

A few important columns include:

- `LOCAL_DATE_STORM_CLOSEST`: the local calendar date on which the storm
  passed closest to that unit. This is used to anchor both precipitation
  and flooding windows (e.g. if you request precipitation data for an
  ADM2 on the 2 days before and 2 days after a given storm, then the
  temporal window is calculated with respect to this date).
- `STORM_DIST_KM`: how close the corresponding storm came to the
  corresponding ADM2 (in kilometres).
- `WIND_EXPOSURE`: whether the pair had measurable wind (`TRUE`) or not
  (`FALSE`).
- `PRECIP_IN_FIELD` and `FLOOD_IN_FIELD`: whether the ADM2 was close
  enough to the storm track to be considered for precipitation and
  flooding respectively.

The `links` table helps avoid duplicating precipitation/flooding rows
for every storm returned in a query. For instance, a single rainy day in
one ADM2 may land inside two storms’ spatio-temporal windows; returning
that day twice would cause problems if the `PRECIP_MM` exposure were to
be summed later in an analysis. Instead, tidy tables are returned along
with the `links` table, and we leave the join operations necessary for
your analysis to you, the user. The `linkage` vignette details this in
full.

## How to inspect your query’s metadata

Three functions provide access to the metadata pertaining to your
`gtropic_data` object:

``` r

# Exactly which files were retrieved, at which version
gtropic_manifest(usa_25)
#> 
#> ── gtropic version manifest ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • Server: "demo.dataverse.org"
#> • Historical: "doi:10.70122/FK2/SWEYST" (version "3.0")
#> • Current-year: "doi:10.70122/FK2/DDSYYD" (version "4.0")
#> • Files recorded: 9
#> • Total size: 158.5 MB
#> • Retrieved: 2026-09-08 16:40:15 EDT
#> • Package version: "0.0.0.9000"
#> 
#> ℹ Pass this object as `manifest` to `gtropic_data()` to
#> reproduce the same retrieval.
```

``` r

# The filters as the package resolved them
str(gtropic_filters(usa_25), max.level = 1)
#> List of 14
#>  $ adm2_ids             : chr [1:3231] "52423323B51867153498623" "52423323B67588574079428" "52423323B97123089850170" "52423323B54167037237770" ...
#>  $ storm_ids            : chr [1:77] "2025017S18122" "2025024S11082" "2025029S25043" "2025034S19166" ...
#>  $ date_start           : Date[1:1], format: "2025-01-01"
#>  $ date_end             : Date[1:1], format: "2025-12-31"
#>  $ date_order           : chr "ymd"
#>  $ years_scanned        : int 2025
#>  $ precip_days_before   : num 2
#>  $ precip_days_after    : num 1
#>  $ precip_radius_km     : num 500
#>  $ flood_days_before    : num 2
#>  $ flood_days_after     : num 1
#>  $ flood_radius_km      : num 500
#>  $ flood_match          : chr "start"
#>  $ inconsistent_adm2_ids: chr(0)
```

``` r

# Anything the query warned about
gtropic_warnings(usa_25)
#> list()
```

The **manifest** provides a record of which published files went into
the result, including their paths, dataset versions, sizes and
checksums. Saving it lets you reproduce the same retrieval months later,
even if the dataset has since been revised. The `versions-and-caching`
vignette illustrates this in more detail.

## Larger queries, caching, and size guardrails

Before scaling up your queries or relaxing your filters to download
larger portions of the G-TROPIC dataset, it’s worth getting to know how
file caching and size guardrails work in `gtropic`:

*The file cache*: To save repeated downloads of the same queried data,
downloads can be kept between sessions using the file cache. By default,
caching is disabled, but you can enable caching for the current R
session using:

``` r

gtropic_cache_enable()
```

Or simply by passing `cache = TRUE` when making queries.

*The size guardrail*: Calling
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
with no arguments means (as of writing) retrieving roughly 47 years
worth of global storm exposure data (~6GB). Before downloading anything
estimated to be larger than 1GB,
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
will prompt you to confirm going ahead with the download. In a
non-interactive session, for instance a scheduled/scripted job, it will
raise an error instead, unless you pass the `confirm = FALSE` argument.

Both topics are covered extensively in
[`vignette("versions-and-caching")`](https://sparklabnyc.github.io/gtropic/articles/versions-and-caching.md).

## Where to go next

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

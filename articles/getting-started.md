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
precipitation data once a week. At second-level administrative unit
(ADM2) resolution worldwide for compatibility with health outcome and
other data, G-TROPIC provides population totals and population-weighted
centroids, storm exposure metrics from both best-track and wind-radii
methods, flood exposure metrics, and daily local precipitation totals,
along with ADM2 metadata for region and WHO classifications.

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

## Data availability

Let’s get oriented by running some of the `gtropic` helper functions
that let us know what G-TROPIC data is available in the first place.
Doing so will help us define the specific query we later send to the
G-TROPIC database, so we can retrieve only the data we need, and nothing
more (to save on memory and disk space).

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

For example, let’s look at a few available `WHO_ENTITY`s shipped in
G-TROPIC:

``` r

head(unique(adm2s$WHO_ENTITY))
#> [1] "Afghanistan"    "United Kingdom" "Albania"        "Algeria"
#> [5] "United States"  NA
```

Or, for instance, the available administrative units in a specific
country:

``` r

gtropic_adm2(WHO_ENTITY = "United States")
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
dataset. By default,
[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md)
returns all storms from 1980 onward, but if we pass in a `year`, then we
can get the list of storms for that given `year`:

``` r

gtropic_storms(year = 2025)
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
by every temporal filter in the package. So, for example, if you inquire
after storms from 2025, the package is designed to return storms whose
genesis date (UTC) occurs within the 2025 calendar year. For more in
depth discussion, see the “Selecting storms by date” section below, and
the `filtering` vignette.

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

Related to the codebook is the
[`gtropic_exposures()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_exposures.md)
function, which lists the variable names associated with exposures
available in G-TROPIC, their units of measurement, their default
aggregation methods (for plotting exposure maps), and whether or not the
[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md)
function supports plotting the exposure.

``` r

gtropic_exposures()
#> $wind
#> # A tibble: 6 × 5
#>   metric                      label               unit  aggregate plot_supported
#>   <chr>                       <chr>               <chr> <chr>     <lgl>
#> 1 VMAX_SUST_MS                Maximum sustained … m/s   max       TRUE
#> 2 VMAX_GUST_MS                Maximum wind gust … m/s   max       TRUE
#> 3 SUST_DUR_MINUTES            Sustained wind dur… minu… max       TRUE
#> 4 GUST_DUR_MINUTES            Wind gust duration… minu… max       TRUE
#> 5 WIND_RADII_VMAX_SUST_MS     Wind radii sustain… m/s   max       TRUE
#> 6 WIND_RADII_SUST_DUR_MINUTES Wind radii duratio… minu… max       TRUE
#>
#> $precip
#> # A tibble: 1 × 5
#>   metric    label              unit  aggregate plot_supported
#>   <chr>     <chr>              <chr> <chr>     <lgl>
#> 1 PRECIP_MM Precipitation (mm) mm    sum       TRUE
#>
#> $flood
#> # A tibble: 3 × 5
#>   metric               label                      unit  aggregate plot_supported
#>   <chr>                <chr>                      <chr> <chr>     <lgl>
#> 1 PROP_FLOODED_POP     Flooded population (propo… prop… max       TRUE
#> 2 PROP_FLOODED_AREA    Flooded area (proportion,… prop… max       TRUE
#> 3 FLOODED_POP_CENTROID Flooded population centro… logi… any       FALSE
```

## A first query

The cornerstone function of the `gtropic` package is the
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function. Let’s start by using it to retrieve all available G-TROPIC
data impacting ADM2s inside the United States, for those storms forming
in calendar year 2025:

``` r

usa_25 <- gtropic_data(
  WHO_ENTITY = "United States",
  date_range = c(2025, 2025)
)
#> Discovering datasets in collection "gtropic".
#> Retrieving 7 files (166.0 MB) ; 3 already cached.
#> Downloading '02_wind/exposures/2025.parquet'.
#> Downloading '02_wind/zero_pairs/2025.parquet'.
#> Downloading '04_precip/2025.parquet'.
#> Downloading '03_flood/2025.parquet'.
#> Downloading '01_pop/2025.parquet'.
#> Downloading '02_wind/storm_tracks/2025.parquet'.
#> Downloading 'metadata/adm2_geometry_simplified.parquet'.

usa_25
#>
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • Historical dataset: doi:10.70122/FK2/SWEYST (version 4.0)
#> • Genesis dates: "2025-01-01" to "2025-12-31"
#> • Storms: 10
#> • ADM2 units: 803
#>
#> ── Tables
#> • wind: 1068 rows x 12 columns
#> • precip: 4272 rows x 3 columns
#> • flood: 661 rows x 7 columns
#> • zero_pairs: 225182 rows x 5 columns
#> • tracks: 682 rows x 10 columns
#> • pop: 803 rows x 6 columns
#> • storm_metadata: 10 rows x 4 columns
#> • adm2: 803 rows x 5 columns
#> • geometry: 803 rows x 2 columns
#> • codebook: 7 entries (list)
#> • links: 1068 rows x 12 columns
#>
#> ℹ Use `summary()` for detail.
```

Printing the result gives a compact summary: which version of the data
you retrieved, the span of storm genesis dates, how many storms and
administrative units were selected based on your filters, and the size
of each table returned.

But, for more detail, including resolved filter settings and any
warnings raised, we can use:

``` r

summary(usa_25)
#>
#> ── G-TROPIC data summary ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#>
#> ── Sources ──
#>
#> • Server: "demo.dataverse.org"
#> • Historical dataset: doi:10.70122/FK2/SWEYST v4.0
#> • Files retrieved: 10 (166.8 MB)
#> • Retrieved at: 2026-10-30 22:50:44 UTC
#>
#> ── Tables ──
#>
#> • wind: 1068 rows x 12 columns
#> • precip: 4272 rows x 3 columns
#> • flood: 661 rows x 7 columns
#> • zero_pairs: 225182 rows x 5 columns
#> • tracks: 682 rows x 10 columns
#> • pop: 803 rows x 6 columns
#> • storm_metadata: 10 rows x 4 columns
#> • adm2: 803 rows x 5 columns
#> • geometry: 803 rows x 2 columns
#> • codebook: 7 entries (list)
#> • links: 1068 rows x 12 columns
#>
#> ── Resolved filters ──
#>
#> • ADM2 units: 803
#> • Storms: 10
#> • Genesis window: 2025-01-01 to 2025-12-31
#> • Wind radius: 500 km
#> • Precipitation window: -2/ +1 days within 500 km
#> • Flood window: -2/+1 days within 500 km (match: "start")
```

The [`summary()`](https://rdrr.io/r/base/summary.html) function here
shows the important resolved filters used by the data query we just
sent, revealing some default argument values used by the
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function:

1.  The `wind_radius_km` argument, which by default is 500km, defining a
    spatial wind field of interest. This restricts the function to
    return wind exposure data associated with storm tracks that come
    within 500km of any ADM2. Combined with our passed in values for
    `WHO_ENTITY` and `date_range`, this means we are asking for wind
    exposure data for storms formed in the calendar year 2025 that come
    within 500km of any ADM2 in the United States.

2.  Similarly, separate `precip_radius_km` and `flood_radius_km`
    arguments define *spatial* precipitation and flood fields, in order
    to link precipitation and flood data to storm tracks. By default,
    these are both also 500km.

3.  The `precip_days_before` and `precip_days_after` arguments (along
    with `flood_days_before` and `flood_days_after`) define the
    *temporal* windows linking precipitation and flood data to storm
    tracks. By default, G-TROPIC will assume you want daily
    precipitation and flood data starting 2 days before and ending up to
    1 day after a storm track makes its closest approach with an ADM2.

Note that the `radius_km` arguments can be any integer in the range (0,
“Inf”\], or `NULL`. Passing in “Inf” or `NULL` does not impose a spatial
field on any storm track, and returns all available wind/precip/flood
data.

The `precip/flood_days_before/after` arguments can be any whole number
in the range \[0, “Inf”).

## The `gtropic_data` object

A `gtropic_data` object is just a named list of data frames, behaving
like any other named list:

``` r

names(usa_25)
#>  [1] "wind"           "precip"         "flood"          "zero_pairs"
#>  [5] "tracks"         "pop"            "storm_metadata" "adm2"
#>  [9] "geometry"       "codebook"       "links"
```

``` r

usa_25$wind
#> # A tibble: 1,068 × 12
#>    ADM2_ID  STORM_ID LOCAL_DATETIME_STORM…¹ LOCAL_DATETIME_MAX_W…² STORM_DIST_KM
#>    <chr>    <chr>    <chr>                  <chr>                          <dbl>
#>  1 5242332… 2025185… 2025-07-06T14:00:00-0… 2025-07-06T05:00:00-0…         152.
#>  2 5242332… 2025223… 2025-08-16T22:00:00-0… 2025-08-16T22:15:00-0…         196.
#>  3 5242332… 2025280… 2025-10-10T07:15:00-0… 2025-10-10T09:45:00-0…         277.
#>  4 5242332… 2025185… 2025-07-07T09:45:00-0… 2025-07-07T13:45:00-0…          11.4
#>  5 5242332… 2025215… 2025-08-02T14:00:00-0… 2025-08-02T15:00:00-0…         471.
#>  6 5242332… 2025243… 2025-09-09T19:30:00-1… 2025-09-09T20:45:00-1…         311.
#>  7 5242332… 2025185… 2025-07-07T14:00:00-0… 2025-07-07T12:45:00-0…          75.1
#>  8 5242332… 2025185… 2025-07-07T14:00:00-0… 2025-07-07T12:30:00-0…         108.
#>  9 5242332… 2025185… 2025-07-04T08:15:00-0… 2025-07-06T04:00:00-0…         358.
#> 10 5242332… 2025185… 2025-07-07T00:15:00-0… 2025-07-07T03:00:00-0…          77.9
#> # ℹ 1,058 more rows
#> # ℹ abbreviated names: ¹​LOCAL_DATETIME_STORM_CLOSEST, ²​LOCAL_DATETIME_MAX_WIND
#> # ℹ 7 more variables: VMAX_SUST_MS <dbl>, SUST_DUR_MINUTES <dbl>,
#> #   VMAX_GUST_MS <dbl>, GUST_DUR_MINUTES <dbl>, WIND_RADII_VMAX_SUST_MS <dbl>,
#> #   WIND_RADII_SUST_DUR_MINUTES <dbl>, LOCAL_DATE_STORM_CLOSEST <chr>
```

By default, the
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
function returns:

- **`wind`**: one row per ADM2 per storm, for those ADM2s within the
  specified `wind_radius_km` of a storm track, for those ADM2-storm
  pairs where wind exposure was non-zero. This is the core exposure
  table.
- **`precip`** – one row per ADM2 per local calendar date, for those
  ADM2s within the specified `precip_radius_km` of a storm track, and
  for dates falling within `precip_days_before` and `precip_days_after`
  the local date of a storm track’s closest approach to that ADM2 (i.e.,
  for the temporal window defined by `LOCAL_DATETIME_STORM_CLOSEST` -
  `precip_days_before` through `LOCAL_DATETIME_STORM_CLOSEST` +
  `precip_days_after`).
- **`flood`** – one row per observed flood event, for flood events whose
  `FLOOD_START_DATE` falls within `flood_days_before` and
  `flood_days_after` the `LOCAL_DATETIME_STORM_CLOSEST`, for those ADM2s
  within `flood_radius_km` of a storm track.
- **`pop`** – population and population-weighted centroid data, per ADM2
  per year.
- **`adm2`** – the lookup table of ADM2s.
- **`codebook`** – the dataset’s own documentation, as a nested list.
- **`zero_pairs`** – ADM2-storm pairs with no measurable wind.
- **`tracks`** – raw storm-track observations for those storms coming
  within `max(c(wind_radius_km, precip_radius_km, flood_radius_km))` of
  any of the requested ADM2s.
- **`geometry`** – ADM2 boundaries, as an `sf` object.
- **`storm_metadata`** – storm names, identifiers, and genesis dates.
- **`links`** – see below.

If you are on a tighter computational budget and would like to reduce
file transfer and storage requirements, pass only the tables required
for your analysis through the `tables` argument.

## The links table

`links` is returned whenever any storm-linked table is. It has one row
per ADM2-storm pair retained by the union of the fields for the
requested hazard tables (or all selected pairs when every requested
field is unbounded):

``` r

usa_25$links
#> # A tibble: 1,068 × 12
#>    STORM_ID      ADM2_ID      LOCAL_DATE_STORM_CLO…¹ STORM_DIST_KM WIND_EXPOSURE
#>    <chr>         <chr>        <date>                         <dbl> <lgl>
#>  1 2025185N30280 52423323B10… 2025-07-06                     152.  TRUE
#>  2 2025223N17337 52423323B10… 2025-08-16                     196.  TRUE
#>  3 2025280N11319 52423323B10… 2025-10-10                     277.  TRUE
#>  4 2025185N30280 52423323B10… 2025-07-07                      11.4 TRUE
#>  5 2025215N33284 52423323B10… 2025-08-02                     471.  TRUE
#>  6 2025243N14239 52423323B10… 2025-09-09                     311.  TRUE
#>  7 2025185N30280 52423323B10… 2025-07-07                      75.1 TRUE
#>  8 2025185N30280 52423323B10… 2025-07-07                     108.  TRUE
#>  9 2025185N30280 52423323B10… 2025-07-04                     358.  TRUE
#> 10 2025185N30280 52423323B10… 2025-07-07                      77.9 TRUE
#> # ℹ 1,058 more rows
#> # ℹ abbreviated name: ¹​LOCAL_DATE_STORM_CLOSEST
#> # ℹ 7 more variables: WIND_IN_FIELD <lgl>, PRECIP_IN_FIELD <lgl>,
#> #   PRECIP_WINDOW_START <date>, PRECIP_WINDOW_END <date>, FLOOD_IN_FIELD <lgl>,
#> #   FLOOD_WINDOW_START <date>, FLOOD_WINDOW_END <date>
```

A few important columns include:

- `LOCAL_DATE_STORM_CLOSEST`: the local calendar date on which the storm
  passed closest to that unit. This is used to anchor both precipitation
  and flooding windows (e.g. if you request precipitation data for an
  ADM2 on the 2 days before and 2 days after a given storm, then the
  temporal window is calculated with respect to this date).
- `STORM_DIST_KM`: how close the corresponding storm came to the
  corresponding ADM2 (in kilometers).
- `WIND_EXPOSURE`: whether the pair had measurable wind (`TRUE`) or not
  (`FALSE`).
- `WIND_IN_FIELD`, `PRECIP_IN_FIELD`, and `FLOOD_IN_FIELD`: whether the
  ADM2 was close enough to the storm track to be considered for wind,
  precipitation and flooding data inclusion, respectively.

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
#> ── gtropic version manifest ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • Server: "demo.dataverse.org"
#> • Historical dataset: doi:10.70122/FK2/SWEYST (version 4.0)
#> • Files recorded: 10
#> • Total size: 166.8 MB
#> • Retrieved: 2026-10-30 22:50:44 UTC
#> • Package version: "0.0.0.9000"
#>
#> ℹ Pass this object as `manifest` to `gtropic_data()` to
#> reproduce the same retrieval.
```

``` r

# The filters as the package resolved them
gtropic_filters(usa_25)
#>
#> ── G-TROPIC resolved filters ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • adm2_ids (803 values): ["52423323B10055621117527", "52423323B10065184809144", "52423323B10302998803932", "52423323B10350916691509", "52423323B10442929932087", "52423323B10584672826222",
#> "52423323B10770912620871", "52423323B10780727675908", "52423323B11034045930208", "52423323B11153152222342"] (793 values omitted)
#> • storm_ids (10 values): ["2025185N30280", "2025223N17337", "2025280N11319", "2025215N33284", "2025243N14239", "2025270N21285", "2025180N19266", "2025205N13145", "2025218N19149",
#> "2025308N09145"]
#> • date_start (1 value): [2025-01-01]
#> • date_end (1 value): [2025-12-31]
#> • date_order (1 value): ["ymd"]
#> • years_scanned (1 value): [2025]
#> • wind_radius_km (1 value): [500]
#> • precip_days_before (1 value): [2]
#> • precip_days_after (1 value): [1]
#> • precip_radius_km (1 value): [500]
#> • flood_days_before (1 value): [2]
#> • flood_days_after (1 value): [1]
#> • flood_radius_km (1 value): [500]
#> • flood_match (1 value): ["start"]
#> • inconsistent_adm2_ids (0 values): <empty>
```

``` r

# Anything the query warned about
gtropic_warnings(usa_25)
#> list()
```

``` r

# Structured schedules, known source coverage, and the assessed request period
gtropic_availability(usa_25)
```

The **manifest** records the published files actually used, including
paths, concrete dataset versions, checksums, and each file’s original
download time. Saving it lets you reproduce the same retrieval with
`gtropic_data(manifest = mf)`, even if `:latest` has since changed. The
`versions-and-caching` vignette explains canonical replay and cache
behavior.

[`gtropic_availability()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
records source update schedules, known coverage, and the requested
period used for diagnostics. Availability warnings are conditional: for
example, a recent wind request can warn about provisional IBTrACS data,
a recent precipitation request about the typical ERA5-Land lag, and a
flood request reaching or extending beyond **2026-01-22** about the
known Groundsource flood endpoint. Inspect
[`gtropic_warnings()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
to retain these diagnostics with an analysis; availability is metadata,
not a guarantee that every record exists.

## Larger queries, caching, and size guardrails

Before scaling up your queries or relaxing your filters to download
larger portions of the G-TROPIC dataset, it’s worth getting to know how
file caching and size guardrails work in `gtropic`:

*The file cache*: To save repeated downloads of the same queried data,
downloads are kept between sessions using the file cache by default. You
can disable caching for the current R session using:

``` r

gtropic_cache_disable()
```

Or bypass it for one query by passing `cache = FALSE`. To retain a
disabled choice across R sessions, use
`gtropic_cache_disable(persist = TRUE)`.

*The size guardrail*: Calling
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
with no arguments requests all available tables for the full global
record. This can require a substantial download. Before downloading
anything estimated to be larger than 1GB,
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
will prompt you to confirm going ahead with the download. In a
non-interactive session, for instance a scheduled/scripted job, it will
raise an error instead, unless you pass the `confirm = FALSE` argument.

Both topics are covered extensively in
[`vignette("versions-and-caching")`](https://sparklabnyc.github.io/gtropic/articles/versions-and-caching.md).

## Summary

- Use the discovery helpers to identify available years, places, storms,
  and exposure metrics before making a query.
- [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  returns a named collection of exposure tables plus metadata that
  documents the selected records and source files.
- Use the `links` table to connect precipitation and flooding records to
  storms without duplicating observations, and retain the manifest for
  reproducibility.

## Where to go next

- [`vignette("filtering")`](https://sparklabnyc.github.io/gtropic/articles/filtering.md)
  covers how all the spatio-temporal/storm filters in
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
  combine.
- [`vignette("linkage")`](https://sparklabnyc.github.io/gtropic/articles/linkage.md)
  documents how to link precipitation/flood data to storm data and using
  the `links` table to perform joins without double-counting exposure
  rows.
- [`vignette("versions-and-caching")`](https://sparklabnyc.github.io/gtropic/articles/versions-and-caching.md)
  walks through reproducibility and download management best practices
  in `gtropic`.
- [`vignette("plotting")`](https://sparklabnyc.github.io/gtropic/articles/plotting.md)
  examines how to quickly visualize exposure maps of G-TROPIC data using
  the
  [`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md)
  plotting function.

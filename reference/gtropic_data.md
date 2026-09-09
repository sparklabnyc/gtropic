# Retrieve tropical cyclone exposure data from G-TROPIC

Retrieves and filters published G-TROPIC data for assessing tropical
cyclone exposures at the second-level administrative unit (ADM2).
Available data include wind exposure, daily precipitation, flood events,
population, administrative-unit characteristics, storm tracks, and storm
metadata.

The function subsets published values; it does not recalculate hazard or
exposure measures. This preserves the definitions and provenance of the
source data for environmental health and epidemiologic analyses.

## Usage

``` r
gtropic_data(
  ADM2_ID = NULL,
  ADM2_NAME = NULL,
  ADM2_GROUP = NULL,
  WHO_ENTITY = NULL,
  WHO_REGION = NULL,
  polygon = NULL,
  reproject = TRUE,
  date_range = NULL,
  date_list = NULL,
  date_format = NULL,
  STORM_ID = NULL,
  STORM_NAME = NULL,
  USA_ATCF_ID = NULL,
  tables = c("wind", "precip", "flood", "pop", "adm2", "codebook"),
  precip_days_before = 2,
  precip_days_after = 1,
  precip_radius_km = 500,
  flood_days_before = 2,
  flood_days_after = 1,
  flood_radius_km = 500,
  flood_match = c("start", "overlap"),
  geometry_resolution = c("simplified", "full"),
  version = ":latest",
  manifest = NULL,
  cache = NULL,
  as = c("tibble", "arrow"),
  confirm = NULL,
  quiet = FALSE
)
```

## Arguments

- ADM2_ID:

  Optional character vector of published ADM2 identifiers. Matching is
  exact and case-sensitive.

- ADM2_NAME:

  Optional character vector of ADM2 names, matched case-insensitively.

- ADM2_GROUP:

  Optional character vector of published ADM2 groupings, typically
  country or territory codes, matched case-insensitively.

- WHO_ENTITY:

  Optional character vector of WHO entity names, matched
  case-insensitively.

- WHO_REGION:

  Optional character vector of WHO region names or codes, matched
  case-insensitively.

- polygon:

  An `sf` or `sfc` object defining a study area. ADM2 units are selected
  using their published population-weighted centroids. The object must
  have a coordinate reference system; use requires the `sf` and
  `sfarrow` packages.

- reproject:

  Logical. If `TRUE` (the default), a `polygon` not already in EPSG:4326
  is transformed before evaluation. If `FALSE`, a non-EPSG:4326 polygon
  produces an error.

- date_range:

  Optional vector of length two defining an inclusive period of storm
  genesis dates. Cannot be combined with `date_list`.

- date_list:

  Optional vector of complete genesis dates to match exactly. Cannot be
  combined with `date_range`.

- date_format:

  Optional character scalar specifying the order of non-ISO dates, such
  as `"ymd"`, `"mdy"`, or `"dmy"`. When supplied, automatic date-order
  inference is not used.

- STORM_ID:

  Optional character vector of published G-TROPIC storm identifiers.
  Matching is exact and case-sensitive.

- STORM_NAME:

  Optional character vector of storm names, matched case-insensitively.

- USA_ATCF_ID:

  Optional character vector of US Automated Tropical Cyclone Forecasting
  System identifiers. Matching is exact and case-sensitive.

- tables:

  Character vector specifying tables to return. Available values are
  `"wind"`, `"precip"`, `"flood"`, `"pop"`, `"adm2"`, `"codebook"`,
  `"zero_pairs"`, `"tracks"`, `"geometry"`, and `"storm_metadata"`.
  `storm_metadata` is added whenever a storm-linked table is requested.
  A `links` table describing evaluated ADM2-storm pairs is also
  returned.

- precip_days_before, precip_days_after:

  Non-negative whole numbers of days before and after local storm
  closest approach to include in each precipitation window.

- precip_radius_km:

  Positive numeric value giving the maximum storm-track distance, in
  kilometres, for inclusion in precipitation linkages.

- flood_days_before, flood_days_after:

  Non-negative whole numbers of days before and after local storm
  closest approach to include in each flood window.

- flood_radius_km:

  Positive numeric value giving the maximum storm-track distance, in
  kilometres, for inclusion in flood linkages.

- flood_match:

  Method for matching flood intervals to storm-relative windows.
  `"start"` includes floods whose start date is within the window.
  `"overlap"` includes any flood interval that overlaps the window,
  including events already in progress at the beginning of the window.

- geometry_resolution:

  Resolution of boundaries returned when `tables` includes `"geometry"`:
  either `"simplified"` (the default) or `"full"`.

- version:

  Dataset version specification. Use `":latest"` (the default), a single
  version applied to both datasets, or a named character vector such as
  `c(historical = "2.0", current = "3.0")`.

- manifest:

  Optional `gtropic_manifest` from
  [`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md).
  When supplied, the recorded source files are used and `version` and
  configured DOI options are ignored.

- cache:

  Whether downloaded source files may be cached. `NULL` uses
  `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
  setting for this call.

- as:

  Requested representation for tabular output: `"tibble"` (the default)
  or `"arrow"`. Source tables are currently collected while filters are
  applied, so tables may be returned as tibbles for either setting.
  Non-tabular elements, including the codebook, retain their natural
  representation.

- confirm:

  Controls the large-download guardrail. `NULL` or `TRUE` applies the
  configured threshold. `FALSE` permits the transfer without interactive
  confirmation.

- quiet:

  Logical. If `TRUE`, progress and informational messages are
  suppressed. Warnings and errors are still reported.

## Value

An object of class `gtropic_data`, implemented as a named list. It
contains the requested data tables and a `links` tibble with one row per
evaluated ADM2-storm pair. `links` includes `ADM2_ID`, `STORM_ID`, the
published local date and distance at closest approach, logical
`WIND_EXPOSURE` status (`TRUE` for nonzero wind exposure and `FALSE` for
zero), indicators for whether the pair falls within the precipitation
and flood radii, and the start and end of each hazard-specific temporal
window. The `pop` table includes a `YEAR` column derived from each
yearly source filename. By default, tabular elements are tibbles,
`codebook` is a nested list, and `geometry` is an `sf` object.

The object also stores the source-file manifest, original query,
resolved filters, recorded diagnostics, retrieval time, and package
version as attributes. Use
[`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md),
[`gtropic_filters()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md),
and
[`gtropic_warnings()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md)
to retrieve the principal metadata attributes.

## Details

### Combining filters

Multiple values supplied to one filter are combined with OR, while
different filters are combined with AND. For example,
`WHO_ENTITY = c("United States", "Mexico")` includes units in either
country, whereas `ADM2_NAME = "San Jose", ADM2_GROUP = "CRI"` requires
both conditions to be met.

`ADM2_ID`, `STORM_ID`, and `USA_ATCF_ID` are exact, case-sensitive
identifiers. Names and geographic classifications are matched
case-insensitively after whitespace normalization. Every supplied value
must match the published metadata, and the intersection of all filters
must be nonempty. Use
[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md)
and
[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md)
to review valid values.

A spatial filter supplied through `polygon` is combined with other
geographic filters using AND. An ADM2 unit is selected when its
published population-weighted centroid falls within the polygon in at
least one year evaluated by the query. For multiyear queries, the
function reports units whose membership changes over time.

### Temporal definition of storm selection

`date_range` and `date_list` select storms according to
`STORM_GENESIS_DATE_UTC`, the UTC calendar date of the first best-track
observation. These arguments do not filter by landfall date, date of
closest approach, or date of local exposure.

Once a storm is selected, its requested records may extend beyond the
specified genesis period. Conversely, a storm that formed before the
period is excluded even if it affected the study area during the period.
To define an analysis by local timing, retrieve an appropriate genesis
period and use `LOCAL_DATE_STORM_CLOSEST` in the returned `links` table.

`date_range` accepts the following endpoint formats:

|                     |                |               |
|---------------------|----------------|---------------|
| Input               | Start endpoint | End endpoint  |
| `"2001"` or `2001`  | `2001-01-01`   | `2001-12-31`  |
| `"2001-03"`         | `2001-03-01`   | `2001-03-31`  |
| `"2001-03-04"`      | unchanged      | unchanged     |
| `Date` or `POSIXct` | calendar date  | calendar date |

Values in `date_list` must be complete dates. Non-ISO character dates
are interpreted jointly across all supplied values. If more than one
date order remains possible, the function uses the precedence `ymd`,
`mdy`, then `dmy` and issues a warning. Use `date_format` when an
explicit interpretation is required.

### Storm-relative precipitation and flood windows

Precipitation and flood records are selected relative to each ADM2-storm
pair in `links`. Windows are centered on `LOCAL_DATE_STORM_CLOSEST` and
use the hazard-specific day and radius arguments. Geographic filters
define the eligible study area; radius arguments can narrow that area
but do not add units outside it.

Precipitation and flood tables contain unique source records and are not
duplicated when one record falls within the windows of multiple storms.
To assign records to storms, join them to `links` using `ADM2_ID` and
the corresponding window columns. Analysts should define how overlapping
storm windows will be treated to avoid double counting exposure.

Requesting precipitation or flood data also requires zero-pair files
during processing. These files identify units that may have relevant
rainfall or flooding despite no recorded wind exposure and can increase
the estimated download size.

### Download size and reproducibility

Before downloading, the function compares the estimated uncached
transfer size with `getOption("gtropic.confirm_threshold_gb")`, which
defaults to 1 GB. Larger requests require confirmation in an interactive
session and produce an error in a non-interactive session unless
`confirm = FALSE`.

Each result includes a manifest containing the source dataset versions,
files, and checksums used. Supplying that manifest to a later call
requests the same published source files. The same analytic filters must
also be supplied to reproduce the query.

## See also

[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md),
[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md),
[`gtropic_years()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_years.md),
[`gtropic_codebook()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_codebook.md),
[`gtropic_manifest()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_manifest.md),
[`gtropic_cache_enable()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_cache_enable.md),
[gtropic_options](https://sparklabnyc.github.io/gtropic/reference/gtropic_options.md)

## Examples

``` r
# \donttest{
# Wind, precipitation, flood, and population data for storms that formed
# during the 2005 season and were evaluated in Jamaica
jamaica <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 8 files (133.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading metadata/codebook.json.
jamaica
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────
#> • Historical dataset: version "3.0"
#> • Current-year dataset: version "4.0"
#> • Genesis dates: "2005-01-01" to "2005-12-31"
#> • Storms: 75
#> • ADM2 units: 827
#> 
#> ── Tables 
#> • wind: 17828 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • pop: 827 rows x 6 columns
#> • storm_metadata: 75 rows x 4 columns
#> • adm2: 827 rows x 5 columns
#> • codebook: 7 entries (list)
#> • links: 61198 rows x 11 columns
#> 
#> ℹ Use `summary()` for detail.

# Retrieve wind exposure for a named storm across all evaluated ADM2 units
katrina <- gtropic_data(
  STORM_NAME = "Katrina-2005",
  tables = c("wind", "adm2")
)
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/1980.parquet.
#> Downloading 02_wind/storm_metadata/1981.parquet.
#> Downloading 02_wind/storm_metadata/1982.parquet.
#> Downloading 02_wind/storm_metadata/1983.parquet.
#> Downloading 02_wind/storm_metadata/1984.parquet.
#> Downloading 02_wind/storm_metadata/1985.parquet.
#> Downloading 02_wind/storm_metadata/1986.parquet.
#> Downloading 02_wind/storm_metadata/1987.parquet.
#> Downloading 02_wind/storm_metadata/1988.parquet.
#> Downloading 02_wind/storm_metadata/1989.parquet.
#> Downloading 02_wind/storm_metadata/1990.parquet.
#> Downloading 02_wind/storm_metadata/1991.parquet.
#> Downloading 02_wind/storm_metadata/1992.parquet.
#> Downloading 02_wind/storm_metadata/1993.parquet.
#> Downloading 02_wind/storm_metadata/1994.parquet.
#> Downloading 02_wind/storm_metadata/1995.parquet.
#> Downloading 02_wind/storm_metadata/1996.parquet.
#> Downloading 02_wind/storm_metadata/1997.parquet.
#> Downloading 02_wind/storm_metadata/1998.parquet.
#> Downloading 02_wind/storm_metadata/1999.parquet.
#> Downloading 02_wind/storm_metadata/2000.parquet.
#> Downloading 02_wind/storm_metadata/2001.parquet.
#> Downloading 02_wind/storm_metadata/2002.parquet.
#> Downloading 02_wind/storm_metadata/2003.parquet.
#> Downloading 02_wind/storm_metadata/2004.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Downloading 02_wind/storm_metadata/2006.parquet.
#> Downloading 02_wind/storm_metadata/2007.parquet.
#> Downloading 02_wind/storm_metadata/2008.parquet.
#> Downloading 02_wind/storm_metadata/2009.parquet.
#> Downloading 02_wind/storm_metadata/2010.parquet.
#> Downloading 02_wind/storm_metadata/2011.parquet.
#> Downloading 02_wind/storm_metadata/2012.parquet.
#> Downloading 02_wind/storm_metadata/2013.parquet.
#> Downloading 02_wind/storm_metadata/2014.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Downloading 02_wind/storm_metadata/2016.parquet.
#> Downloading 02_wind/storm_metadata/2017.parquet.
#> Downloading 02_wind/storm_metadata/2018.parquet.
#> Downloading 02_wind/storm_metadata/2019.parquet.
#> Downloading 02_wind/storm_metadata/2020.parquet.
#> Downloading 02_wind/storm_metadata/2021.parquet.
#> Downloading 02_wind/storm_metadata/2022.parquet.
#> Downloading 02_wind/storm_metadata/2023.parquet.
#> Downloading 02_wind/storm_metadata/2024.parquet.
#> Downloading 02_wind/storm_metadata/2025.parquet.
#> Downloading 02_wind/storm_metadata/2026.parquet.
#> Retrieving 3 files (10.2 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.

# Define a seven-day rainfall window and a 300-km storm field for an
# epidemiologic study in the Philippines
philippines <- gtropic_data(
  WHO_ENTITY = "Philippines",
  date_range = c("2015-06-01", "2015-12-31"),
  precip_days_before = 3,
  precip_days_after = 3,
  precip_radius_km = 300,
  tables = c("precip", "pop", "adm2")
)
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Retrieving 5 files (120.0 MB) ; .
#> Downloading 02_wind/exposures/2015.parquet.
#> Downloading 02_wind/zero_pairs/2015.parquet.
#> Downloading 04_precip/2015.parquet.
#> Downloading 01_pop/2015.parquet.
#> Downloading 01_pop/2016.parquet.

# Retain source-file provenance for a subsequent retrieval
source_manifest <- gtropic_manifest(philippines)
philippines_retrieved_again <- gtropic_data(
  WHO_ENTITY = "Philippines",
  date_range = c("2015-06-01", "2015-12-31"),
  precip_days_before = 3,
  precip_days_after = 3,
  precip_radius_km = 300,
  tables = c("precip", "pop", "adm2"),
  manifest = source_manifest
)
#> Reproducing a previous retrieval from `manifest`.
#> ℹ `version` and the DOI options are ignored.
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Retrieving 5 files (120.0 MB) ; .
#> Downloading 02_wind/exposures/2015.parquet.
#> Downloading 02_wind/zero_pairs/2015.parquet.
#> Downloading 04_precip/2015.parquet.
#> Downloading 01_pop/2015.parquet.
#> Downloading 01_pop/2016.parquet.
# }
```

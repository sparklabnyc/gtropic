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
  wind_radius_km = 500,
  tables = c("wind", "precip", "flood", "pop", "adm2", "codebook", "zero_pairs",
    "tracks", "geometry", "storm_metadata"),
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
  quiet = FALSE,
  offline = FALSE
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

  Optional vector of one or two endpoints defining an inclusive period
  of storm genesis dates. A single value is used for both endpoints.
  Cannot be combined with `date_list`.

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

  Optional character vector of published, disambiguated storm names in
  `Name-YYYY` form, such as `Katrina-2005`, matched case-insensitively.
  The suffix year limits the metadata files scanned; storm and date
  filters are still combined with AND.

- USA_ATCF_ID:

  Optional character vector of US Automated Tropical Cyclone Forecasting
  System identifiers. Matching is exact and case-sensitive.

- wind_radius_km:

  Maximum published storm-track distance, in kilometers, for returned
  wind records; defaults to 500. A finite positive value restricts them
  inclusively. It also contributes to the union-boundary of `links` when
  wind is requested. `Inf` or `NULL` is unbounded; with an unbounded
  wind radius, wind records with missing published distances are
  retained. This argument does not filter returned `zero_pairs` records.

- tables:

  Character vector specifying tables to return. By default, all
  available tables are returned: `"wind"`, `"precip"`, `"flood"`,
  `"pop"`, `"adm2"`, `"codebook"`, `"zero_pairs"`, `"tracks"`,
  `"geometry"`, and `"storm_metadata"`. Supply a smaller vector to
  reduce transfer and storage requirements. Because the default includes
  `"geometry"`, it requires the suggested `sf` and `sfarrow` packages.
  `storm_metadata` is added whenever a storm-linked table is requested.
  A `links` table describing retained ADM2-storm pairs is also returned.

- precip_days_before, precip_days_after:

  Non-negative whole numbers of days before and after local storm
  closest approach to include in each precipitation window.

- precip_radius_km:

  Positive numeric value giving the maximum storm-track distance, in
  kilometers, for inclusion in precipitation linkages. `Inf` or `NULL`
  leaves this radius unbounded.

- flood_days_before, flood_days_after:

  Non-negative whole numbers of days before and after local storm
  closest approach to include in each flood window.

- flood_radius_km:

  Positive numeric value giving the maximum storm-track distance, in
  kilometers, for inclusion in flood linkages. `Inf` or `NULL` leaves
  this radius unbounded.

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
  When supplied, its recorded source files and canonical resolved query
  are used. Data-shaping arguments cannot also be supplied; only
  `cache`, `as`, `confirm`, `quiet`, and `offline` may differ during
  replay.

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

- offline:

  Logical. If `TRUE`, use only cached source files and a persisted
  discovery snapshot; no Dataverse update check is made and the snapshot
  age is reported. Requires an enabled cache. Defaults to `FALSE`.

## Value

An object of class `gtropic_data`, implemented as a named list. It
contains the requested data tables and a `links` tibble with one row per
ADM2-storm pair retained by the union of requested hazard radii (or all
available selected pairs when that union is unbounded). `links` includes
`ADM2_ID`, `STORM_ID`, the published local date and distance at closest
approach, logical `WIND_EXPOSURE` status (`TRUE` for nonzero wind
exposure and `FALSE` for zero), `WIND_IN_FIELD` and indicators for
whether the pair falls within the precipitation and flood radii, and the
start and end of each hazard-specific temporal window. With a finite
requested hazard radius, `tracks` contains only storms with at least one
qualifying link; otherwise it retains all selected storms. The `pop`
table includes a `YEAR` column derived from each yearly source filename.
By default, all available tables are returned. Tabular elements are
tibbles, `codebook` is a nested list, and `geometry` is an `sf` object.

The object also stores the source-file manifest, original query,
resolved filters, recorded diagnostics, retrieval time, and package
version as attributes, including structured availability and coverage
metadata. Use
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
must be nonempty. An unmatched `STORM_ID` or `STORM_NAME` is checked
against the published exclusion records; use
[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md),
[`gtropic_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_storms.md),
and
[`gtropic_excluded_storms()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_excluded_storms.md)
to review valid or excluded values.

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

`wind_radius_km` defaults to 500 km and inclusively limits returned wind
records to pairs at or below their published `STORM_DIST_KM`. `links`
contains every published pair needed by the union of requested hazard
fields: a larger precipitation or flood radius can therefore retain
pairs beyond the wind radius. Its `WIND_IN_FIELD` flag identifies
whether each retained pair falls inside the configured wind field. For
every `*_IN_FIELD` flag, a finite radius excludes pairs with missing
published distance, while `Inf` or `NULL` includes them. An unbounded
radius does not constrain this union. When no finite requested hazard
field bounds a query, `links` retains all available pairs within the
selected geography and storms, and returned ADM2, population, geometry,
and tracks retain their original selected scope. When a requested hazard
table has a finite radius, returned ADM2, population, and geometry rows
are limited to units in its qualifying links; the union is used for
several such hazards.

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
requests the same published source files and replays its canonical
resolved query, including resolved ADM2 and storm identifiers and the
metadata-file years originally scanned.

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
# All available tables for storms that formed during the 2005 season and
# were evaluated in Jamaica
jamaica <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
#> Retrieving 10 files (143.0 MB) ; .
#> Downloading 02_wind/exposures/2005.parquet.
#> Downloading 02_wind/zero_pairs/2005.parquet.
#> Downloading 04_precip/2005.parquet.
#> Downloading 03_flood/2005.parquet.
#> Downloading 01_pop/2005.parquet.
#> Downloading 02_wind/storm_tracks/2005.parquet.
#> Downloading metadata/codebook.json.
#> Downloading metadata/adm2_geometry_simplified.parquet.
jamaica
#> 
#> ── G-TROPIC data ───────────────────────────────────────────────────────────────
#> • Historical dataset: doi:10.70122/FK2/SWEYST (version 4.0)
#> • Genesis dates: "2005-01-01" to "2005-12-31"
#> • Storms: 5
#> • ADM2 units: 827
#> 
#> ── Tables 
#> • wind: 3719 rows x 12 columns
#> • precip: 14876 rows x 3 columns
#> • flood: 176 rows x 7 columns
#> • zero_pairs: 43370 rows x 5 columns
#> • tracks: 376 rows x 10 columns
#> • pop: 827 rows x 6 columns
#> • storm_metadata: 5 rows x 4 columns
#> • adm2: 827 rows x 5 columns
#> • geometry: 827 rows x 2 columns
#> • codebook: 7 entries (list)
#> • links: 3719 rows x 12 columns
#> 
#> ℹ Use `summary()` for detail.

# Retrieve wind exposure for a named storm across all evaluated ADM2 units
katrina <- gtropic_data(
  STORM_NAME = "Katrina-2005",
  tables = c("wind", "adm2")
)
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2005.parquet.
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
#> Discovering datasets in collection "gtropic".
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Retrieving 5 files (120.0 MB) ; .
#> Downloading 02_wind/exposures/2015.parquet.
#> Downloading 02_wind/zero_pairs/2015.parquet.
#> Downloading 04_precip/2015.parquet.
#> Downloading 01_pop/2015.parquet.

# Retain source-file provenance for a subsequent retrieval
source_manifest <- gtropic_manifest(philippines)
philippines_retrieved_again <- gtropic_data(manifest = source_manifest)
#> Reproducing a previous retrieval from `manifest`.
#> ℹ `version` and the DOI options are ignored.
#> Downloading metadata/adm2.parquet.
#> Downloading 02_wind/storm_metadata/2015.parquet.
#> Retrieving 5 files (120.0 MB) ; .
#> Downloading 02_wind/exposures/2015.parquet.
#> Downloading 02_wind/zero_pairs/2015.parquet.
#> Downloading 04_precip/2015.parquet.
#> Downloading 01_pop/2015.parquet.
# }
```

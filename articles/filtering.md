# Filtering: places, dates and storms

``` r

library(gtropic)
```

[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
has three ways to filter queries: by geography, temporality, and by
specific storms. This vignette covers the each of these filters and how
they combine with one another.

## How filters combine

**Values within a single argument are combined with a logical “OR”
(union). Values across different arguments are combined with a logical
“AND” (intersection).**

This is the same rule as in
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
or SQL’s `WHERE` clause.

``` r

# Within a single argument implies a union (OR). E.g., Every unit in the United States OR Mexico
gtropic_data(WHO_ENTITY = c("United States", "Mexico"))

# Across arguments implies an intersection (AND). E.g., Units named "San Jose" that are ALSO in Costa Rica
gtropic_data(ADM2_NAME = "San Jose", ADM2_GROUP = "CRI")

# Across arguments implies an intersection (AND), e.g. Hurrican Katrina (2005)'s USA rows only
gtropic_data(STORM_NAME = "Katrina-2005", WHO_ENTITY = "United States")
```

If two arguments conflict so that nothing satisfies both, you get an
error naming the arguments involved. For example:

``` r

gtropic_data(WHO_ENTITY = "Mexico", ADM2_GROUP = "USA")
#> Discovering datasets in collection "gtropic".
#> Error in `assemble_gtropic_data()`:
#> ! No ADM2 units satisfy all of the geographic filters supplied.
#> ✖ Filters in play: `ADM2_GROUP` and `WHO_ENTITY`.
#> ℹ Values are combined with OR within an argument and AND across arguments, so
#>   these conditions must hold together.
#> ℹ Use `gtropic_adm2()` to explore valid combinations.
```

## Geographic filters

Five named arguments select administrative units, plus `polygon` for
spatial selection.

| Argument     | Matches                       | Case             |
|--------------|-------------------------------|------------------|
| `ADM2_ID`    | unit identifier               | exact            |
| `ADM2_NAME`  | unit name                     | case-insensitive |
| `ADM2_GROUP` | country grouping code         | case-insensitive |
| `WHO_ENTITY` | WHO entity, usually a country | case-insensitive |
| `WHO_REGION` | WHO region code               | case-insensitive |

Names are matched case-insensitively and with surrounding whitespace
trimmed; the canonical spelling from the dataset is what will appear in
the resulting tables. Identifiers, on the other hand, are matched
exactly.

Values that fail to match anything throw errors, including a partial
miss. For example, if you supply five country names but one of them is
misspelled, the whole query will fail. The error will suggest near
matches:

``` r

gtropic_data(WHO_ENTITY = c("Untied States", "Mexcio"), date_range = c(2005, 2005))
#> Discovering datasets in collection "gtropic".
#> Error in `assemble_gtropic_data()`:
#> ! 2 values in `WHO_ENTITY` did not match any ADM2 unit.
#> ✖ "Untied States" - did you mean "United States"?
#> ✖ "Mexcio" - did you mean "Mexico"?
#> ℹ See `gtropic_adm2()` for the full list of valid values.
```

You can use
[`gtropic_adm2()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_adm2.md)
to browse the vocabulary before querying:

``` r

gtropic_adm2()
#> # A tibble: 49,349 × 5
#>    ADM2_ID                 ADM2_NAME        ADM2_GROUP WHO_ENTITY  WHO_REGION
#>    <chr>                   <chr>            <chr>      <chr>       <chr>
#>  1 17698898B68993560499599 Ab Band          AFG        Afghanistan Eastern Medi…
#>  2 17698898B73770643817409 Ab Kamari        AFG        Afghanistan Eastern Medi…
#>  3 17698898B14779938213850 Acheen           AFG        Afghanistan Eastern Medi…
#>  4 17698898B23166050371692 Adraskan         AFG        Afghanistan Eastern Medi…
#>  5 17698898B88187802806148 Ahmad Abad       AFG        Afghanistan Eastern Medi…
#>  6 17698898B56570025970807 Ajristan         AFG        Afghanistan Eastern Medi…
#>  7 17698898B62190249131225 Alasay           AFG        Afghanistan Eastern Medi…
#>  8 17698898B14177024461677 Ali Khail (Jaji) AFG        Afghanistan Eastern Medi…
#>  9 17698898B86734293398191 Aliabad          AFG        Afghanistan Eastern Medi…
#> 10 17698898B28188212808936 Alingar          AFG        Afghanistan Eastern Medi…
#> # ℹ 49,339 more rows
```

### Filtering with a polygon

The `polygon` argument in
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
takes an `sf` or `sfc` object. An administrative unit is included when
its population-weighted centroid falls inside the polygon. This requires
the optional `sf` and `sfarrow` packages.

``` r

library(sf)
#> Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE

north_carolina <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc_10to11 <- gtropic_data(polygon = north_carolina, date_range = c(2010, 2011))
#> Discovering datasets in collection "gtropic".
#> Downloading '01_pop/2010.parquet'.
#> Downloading '01_pop/2011.parquet'.
#> Warning: Polygon membership is year-dependent.
#> ℹ Centroids were evaluated for 2 years (2010-2011); an ADM2 unit is included if
#>   its centroid fell inside the polygon in any of them.
#> Downloading '02_wind/storm_metadata/2010.parquet'.
#> Downloading '02_wind/storm_metadata/2011.parquet'.
#> Retrieving 10 files (279.7 MB) ; 7 already cached.
#> Downloading '02_wind/exposures/2010.parquet'.
#> Downloading '02_wind/exposures/2011.parquet'.
#> Downloading '02_wind/zero_pairs/2010.parquet'.
#> Downloading '02_wind/zero_pairs/2011.parquet'.
#> Downloading '04_precip/2010.parquet'.
#> Downloading '04_precip/2011.parquet'.
#> Downloading '03_flood/2010.parquet'.
#> Downloading '03_flood/2011.parquet'.
#> Downloading '02_wind/storm_tracks/2010.parquet'.
#> Downloading '02_wind/storm_tracks/2011.parquet'.
nc_10to11$adm2
#> # A tibble: 98 × 5
#>    ADM2_ID                 ADM2_NAME ADM2_GROUP WHO_ENTITY    WHO_REGION
#>    <chr>                   <chr>     <chr>      <chr>         <chr>
#>  1 52423323B75735385248620 Alamance  USA        United States Americas
#>  2 52423323B44414006635896 Alexander USA        United States Americas
#>  3 52423323B93536446386656 Alleghany USA        United States Americas
#>  4 52423323B73600596363110 Anson     USA        United States Americas
#>  5 52423323B47317028722188 Ashe      USA        United States Americas
#>  6 52423323B91612624851462 Avery     USA        United States Americas
#>  7 52423323B44486980116087 Beaufort  USA        United States Americas
#>  8 52423323B7787272397887  Bertie    USA        United States Americas
#>  9 52423323B8015598177606  Bladen    USA        United States Americas
#> 10 52423323B12833551098527 Brunswick USA        United States Americas
#> # ℹ 88 more rows
```

There are two important points to keep in mind:

1.  **The polygon must carry a coordinate reference system**. Without
    one there is no way for `gtropic` to know what its coordinates mean,
    and an error will be thrown. By default a polygon in any other CRS
    is silently transformed to EPSG:4326 to match the published
    centroids. Setting `reproject = FALSE` turns that transformation
    off, which means a non-4326 input polygon will throw an error
    instead.

2.  **Polygon membership can change between years**. Population-weighted
    centroids move as populations shift, so a unit whose centroid sits
    near the boundary may fall inside the polygon in some years and
    outside in others. A unit is included in the resulting query if its
    centroid fell inside the polygon in *any* year your query spans. The
    package warns whenever a polygon query covers more than one year,
    and warns again, listing the units, if membership actually varied.
    The complete list is always available:

``` r

gtropic_warnings(nc_10to11)$polygon_inconsistent
#> NULL
```

The named geographic filters are not year-varying, so they raise no such
warning.

### The wind, precip, and flood radius filters

The `[wind/precip/flood]_radius_km` arguments (all `500` by default)
specify the maximum distance (in km) an ADM2 must be from the closest
approach made by a storm track for that ADM2’s corresponding
wind/precip/flood data to be returned. These filters are applied
directly to the `STORM_DIST_KM` data column in the `$links` table. Set
these `[wind/precip/flood]_radius_km` arguments to `Inf` or `NULL` to
retrieve wind/precip/flood data for all ADM2s regardless of their
distance to the selected storm tracks.

These filters also restric the set of storms returned in your query. For
example, using the defaults mean that storms failing to come within
500km of any ADM2 in your search will be excluded from your resulting
`gtropic_data` object.

**Note that these defaults of `500`km are not prescriptive, and we do
not claim to define an “exposure” with these default values. We leave
this configurable per data retrieval query so that you the user can
define your specific wind/precipitation/flood fields of interest.
Smaller values for these arguments will also result in smaller data
tables being returned, which can help save on memory consumption.**

## Temporal filters

The `date_range` argument takes two endpoints (if you pass in a single
value it gets broadcast/parsed as both endpoints, e.g. “2025” is
understood to be c(“2025”, “2025”)). Alternatively, you can use
`date_list`, which takes a vector of specific dates. The two arguments
cannot be combined.

### Date parsing

Partial dates are expanded to the start of their period for the first
element of a range and the end for the second, so `c(2001, 2003)` covers
all three years:

| You write             | Start becomes | End becomes  |
|-----------------------|---------------|--------------|
| `"2001"` or `2001`    | `2001-01-01`  | `2001-12-31` |
| `"2001-03"`           | `2001-03-01`  | `2001-03-31` |
| `"2001-03-04"`        | as given      | as given     |
| a `Date` or `POSIXct` | as given      | as given     |

If your dates are not written in ISO order, the package works out the
order for you using every value you supplied to make the determination:

``` r

# On its own "01/04/2001" could be 4 January or 1 April. But 19 cannot be a
# month, so day-month-year is impossible for this pair, which forces a
# month-day-year interpretation for both.
gtropic_storms(date_range = c("01/04/2001", "05/19/2001"))
#> # A tibble: 18 × 4
#>    STORM_ID      STORM_NAME    USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>         <chr>       <date>
#>  1 2001018S12086 Charly-2001   SH062001    2001-01-17
#>  2 2001028S15128 Terri-2001    SH072001    2001-01-27
#>  3 2001039S11139 Winsome-2001  SH082001    2001-02-08
#>  4 2001039S13117 Vincent-2001  SH092001    2001-02-07
#>  5 2001047N09134 Unnamed-2001  WP012001    2001-02-16
#>  6 2001047S17157 Unnamed-2001  SH102001    2001-02-16
#>  7 2001049S17198 Oma-2001      SH112001    2001-02-18
#>  8 2001054S15155 Abigail-2001  SH122001    2001-02-23
#>  9 2001056S11163 Paula-2001    SH132001    2001-02-25
#> 10 2001060S19223 Rita-2001     SH142001    2001-02-28
#> 11 2001064S10052 Dera-2001     SH152001    2001-03-04
#> 12 2001091S11111 Walter-2001   SH172001    2001-04-01
#> 13 2001091S13094 Unnamed-2001  SH162001    2001-04-01
#> 14 2001093S10062 Evariste-2001 SH182001    2001-04-02
#> 15 2001094S12172 Sose-2001     SH192001    2001-04-03
#> 16 2001106N04138 Unnamed-2001  WP022001    2001-04-16
#> 17 2001106S08137 Alistair-2001 SH202001    2001-04-15
#> 18 2001125N05129 Cimaron-2001  WP032001    2001-05-05
```

When more than one interpretation of the supplied dates is possible,
`gtropic` will pick by precedence – `ymd`, then `mdy`, then `dmy` –
before issuing a warning, naming the interpretation chosen:

``` r

gtropic_storms(date_range = c("01/04/2001", "05/06/2001"))
#> Warning: Dates "01/04/2001" and "05/06/2001" are ambiguous.
#> ℹ Interpreting as "mdy", so "01/04/2001" means "2001-01-04".
#> ℹ Rejected alternative: "dmy".
#> ℹ Set `date_format` to be explicit, e.g. `date_format = "dmy"`.
#> # A tibble: 18 × 4
#>    STORM_ID      STORM_NAME    USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>    <chr>         <chr>         <chr>       <date>
#>  1 2001018S12086 Charly-2001   SH062001    2001-01-17
#>  2 2001028S15128 Terri-2001    SH072001    2001-01-27
#>  3 2001039S11139 Winsome-2001  SH082001    2001-02-08
#>  4 2001039S13117 Vincent-2001  SH092001    2001-02-07
#>  5 2001047N09134 Unnamed-2001  WP012001    2001-02-16
#>  6 2001047S17157 Unnamed-2001  SH102001    2001-02-16
#>  7 2001049S17198 Oma-2001      SH112001    2001-02-18
#>  8 2001054S15155 Abigail-2001  SH122001    2001-02-23
#>  9 2001056S11163 Paula-2001    SH132001    2001-02-25
#> 10 2001060S19223 Rita-2001     SH142001    2001-02-28
#> 11 2001064S10052 Dera-2001     SH152001    2001-03-04
#> 12 2001091S11111 Walter-2001   SH172001    2001-04-01
#> 13 2001091S13094 Unnamed-2001  SH162001    2001-04-01
#> 14 2001093S10062 Evariste-2001 SH182001    2001-04-02
#> 15 2001094S12172 Sose-2001     SH192001    2001-04-03
#> 16 2001106N04138 Unnamed-2001  WP022001    2001-04-16
#> 17 2001106S08137 Alistair-2001 SH202001    2001-04-15
#> 18 2001125N05129 Cimaron-2001  WP032001    2001-05-05
```

Supplying `date_format` skips inference entirely and silences the
warning:

``` r

gtropic_storms(date_range = c("01/04/2001", "05/06/2001"), date_format = "dmy")
```

If none of the above interpretations work, you will get an error showing
the offending strings.

### Storms are selected by their genesis date

Importantly, `date_range` and `date_list` select **storms whose
`STORM_GENESIS_DATE_UTC` falls within the requested period**, i.e. the
UTC calendar date of a storm’s first best-track observation, which is
when it formed, not when it made landfall, and not when it began to
impact ADM2s. In this way, all data for these storms is then returned,
subject to your other filters. The package’s explicit year filters and
temporal storm discovery follow this same genesis-date definition.
Likewise, G-TROPIC’s annual storm metadata, wind, zero-pair, and track
files are organized by the calendar year of `STORM_GENESIS_DATE_UTC`,
not by the published `SEASON` value. There are two consequences to this
approach:

1.  **This will retrieve exposure data that may fall outside the
    requested range,** provided it is associated with a storm whose
    `STORM_GENESIS_DATE_UTC` is within the range. E.g., A storm forming
    on 30 September that persists for ten days contributes exposure and
    rainfall rows well into October. If you ask for all data associated
    with the storms beginning in September, those October rows will be
    retrieved as well, even though your range ended on 30 September.

2.  **A storm that formed before your range is excluded entirely**, even
    if it struck your region inside the range. E.g., a storm forming on
    28 August that hits Texas on 3 September would *not* be returned by:

``` r

gtropic_data(ADM2_GROUP = "USA", date_range = c("2002-09-01", "2002-09-30"))
```

That is an expected exclusion, and if you are studying September
hospital admissions, you will want to expand your date range
accordingly.

### Temporal precipitation and flood windows

The `[precip/flood]_days_[before/after]` arguments are used to define
precipitation and flood *temporal* fields, i.e., the window of dates
around the anchor date for an `ADM2_ID`-`STORM_ID` pair. The anchor date
for an `ADM2_ID`-`STORM_ID` pair is the `LOCAL_DATETIME_STORM_CLOSEST`
value for that pair, found in the returned `$links` table. This column
defines the local datetime when that `STORM_ID`’s storm track came
nearest to that `ADM2_ID`’s population-weighted centroid. By default,
`[precip/flood]_days_before = 2`, and `[precip/flood]_days_after = 1`,
meaning precipitation/flood data is obtained for an `ADM2_ID` for all
days inside the window given by the 2 days before and 1 day after a
`STORM_ID`’s `LOCAL_DATETIME_STORM_CLOSEST` date.

### How to retrieve storms by when they impacted an ADM2

Query a generous range and filter the returned `$links` table on
`LOCAL_DATE_STORM_CLOSEST` (the local date the storm passed closest to
each unit):

``` r

library(dplyr)

# Widen by a couple months at each end to catch storms that formed before September
res <- gtropic_data(
  ADM2_GROUP = "USA",
  date_range = c("2002-07-01", "2002-11-30")
)

struck_in_september <- res$links |>
  filter(
    LOCAL_DATE_STORM_CLOSEST >= as.Date("2002-09-01"),
    LOCAL_DATE_STORM_CLOSEST <= as.Date("2002-09-30")
  )
```

How far to widen is a judgment call left to you to make: tropical
cyclones rarely persist beyond two or three weeks, so two months either
side is generous.

## Storm filters

Three arguments select storms directly. `STORM_NAME` is matched
case-insensitively; `STORM_ID` and `USA_ATCF_ID` must match exactly.

``` r

gtropic_data(STORM_NAME = "Haiyan-2013")
gtropic_data(USA_ATCF_ID = "WP312013", date_range = c(2013, 2013))
```

`STORM_ID` uniquely identifies one storm and is the preferred key for
unambiguous queries, joins, grouping, and reproducible work.
`STORM_NAME` is a convenience lookup and display label, not a unique
key: even a complete value can occur more than once, including for
storms in different basins. Its `Name-YYYY` suffix is the published
`SEASON` year, which need not equal the calendar year of
`STORM_GENESIS_DATE_UTC`.

The recommended workflow is to use `gtropic_storms(STORM_NAME = ...)` to
quickly inspect all matches, identify the intended `STORM_ID`, and then
use that ID in
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md)
and throughout downstream analysis:

``` r

# Look up a storm by its convenience label and inspect the possible matches
gtropic_storms(STORM_NAME = "Haiyan-2013")
#> # A tibble: 1 × 4
#>   STORM_ID      STORM_NAME  USA_ATCF_ID STORM_GENESIS_DATE_UTC
#>   <chr>         <chr>       <chr>       <date>
#> 1 2013306N07162 Haiyan-2013 WP312013    2013-11-02
```

``` r

# After confirming the intended row above, use its unique key
haiyan <- gtropic_data(STORM_ID = "2013306N07162")

haiyan$wind |>
  dplyr::group_by(STORM_ID, ADM2_ID)
```

## Summary

- Geographic, temporal, and storm filters combine with AND across
  arguments and OR within an argument.
- Date filters select storms by genesis date, so retrieved exposure
  records can extend outside the requested date range.
- Use `STORM_NAME` to find candidate storms, then use the unique
  `STORM_ID` for retrieval, joins, grouping, and reproducible analysis.
- The `[wind/precip/flood]_radius_km` arguments, along with the
  `[precip/flood]_days_[before/after]` arguments are crucial in defining
  your spatial and temporal fields linking exposures to storms. The
  default values we supply are not prescriptive.
- Use `LOCAL_DATE_STORM_CLOSEST` in the `links` table when analysis
  should be anchored to the date a storm affected an ADM2.

## Where to go next

- [`vignette("getting-started")`](https://sparklabnyc.github.io/gtropic/articles/getting-started.md)
  introduces G-TROPIC discovery helpers and the structure of a
  `gtropic_data` result.
- [`vignette("linkage")`](https://sparklabnyc.github.io/gtropic/articles/linkage.md)
  explains how to join precipitation and flood records to storms without
  double counting.
- [`vignette("plotting")`](https://sparklabnyc.github.io/gtropic/articles/plotting.md)
  shows how to map exposure, count, and track data.
- [`vignette("versions-and-caching")`](https://sparklabnyc.github.io/gtropic/articles/versions-and-caching.md)
  covers manifests, cache management, and reproducible retrievals.

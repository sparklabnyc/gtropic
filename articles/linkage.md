# Linking rainfall and flooding to storms

``` r

library(gtropic)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

Wind exposure is straightforward: the dataset records, for each
administrative unit and storm, what winds were experienced. Rainfall and
flooding are harder, because rain falls on dates and floods span
intervals, and deciding which of them “belong” to a storm is a modelling
choice rather than a fact.

This vignette explains the choice `gtropic` makes, how to change it, and
how to join the results without double-counting.

## The pairing model

Rainfall and flood rows are selected per administrative unit-storm pair.
A pair qualifies on two criteria, both of which must hold.

**A temporal window.** Centred on `LOCAL_DATE_STORM_CLOSEST`, the local
calendar date on which the storm passed closest to that unit:

    [ closest - days_before , closest + days_after ]

The defaults are two days before and one day after. The window is
computed independently for rainfall and flooding, so you can widen one
without touching the other.

**A spatial field.** Every unit-storm pair the dataset evaluated with
`STORM_DIST_KM` at or below the radius, 500 km by default. Crucially
this includes pairs with *no measurable wind*: a unit two hundred
kilometres inland that received torrential rain but never felt the
storm’s wind field is part of the rainfall field, and excluding it would
drop exactly the compound-hazard cases the dataset exists to support.

Your geographic filter is an **outer bound** on all of this. The radius
narrows within it; it never widens beyond it. Asking for Florida will
not return Georgia because a storm’s 500 km field happened to reach
there.

## A note on downloads

Because far-field distances are recorded only in the zero-pairs table,
asking for `precip` or `flood` causes those files to be downloaded
internally, whether or not you asked for the `zero_pairs` table.
Including `"zero_pairs"` in `tables` controls only whether it is
*returned to you*, never whether it is fetched. It adds roughly 30 MB
per year to a query, which is worth knowing when you plan a long time
span.

## Reading the links table

``` r

jam <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = 2005)
#> Downloading 'metadata/adm2.parquet'.
#> Error in `assemble_gtropic_data()` at gtropic/R/gtropic_data.R:307:3:
#> ! `date_range` must have exactly 2 elements.
#> ✖ Got 1.
#> ℹ For example `date_range = c("2005-08-01", "2005-09-30")` or `date_range = c(2005, 2010)`.

head(jam$links)
#> Error:
#> ! object 'jam' not found
```

Each row is one unit-storm pair:

| Column | Meaning |
|----|----|
| `STORM_ID`, `ADM2_ID` | the pair |
| `LOCAL_DATE_STORM_CLOSEST` | local date of closest approach – the window anchor |
| `STORM_DIST_KM` | how close the track came |
| `WIND_EXPOSURE` | `TRUE` for nonzero wind exposure; `FALSE` for zero |
| `PRECIP_IN_FIELD` | was this pair inside the rainfall radius? |
| `PRECIP_WINDOW_START`, `PRECIP_WINDOW_END` | the computed rainfall window |
| `FLOOD_IN_FIELD` | was this pair inside the flood radius? |
| `FLOOD_WINDOW_START`, `FLOOD_WINDOW_END` | the computed flood window |

The `*_IN_FIELD` flags answer a question that would otherwise be
unanswerable: when a pair has no rainfall rows, was it too far from the
track to be considered, or was it considered and genuinely had no data?
Those mean very different things in an analysis.

``` r

jam$links |>
  count(WIND_EXPOSURE, PRECIP_IN_FIELD)
#> Error:
#> ! object 'jam' not found
```

## Do not double-count

**Rainfall and flood rows are not duplicated per storm.** This is the
single most important thing in this vignette.

One unit’s rainy day can fall inside two storms’ windows – a common
occurrence in an active season, when systems follow one another within
days. If the package returned that day once per storm, then this:

``` r

sum(res$precip$PRECIP_MM)   # WRONG if rows were duplicated
```

would silently overstate total rainfall, and nothing about the result
would look wrong. So `precip` and `flood` come back as tidy tables at
their natural grain – one row per unit per date, one row per flood event
– and you join through `links` deliberately.

### Joining rainfall to storms

Use a non-equi join, matching on unit and on the date falling inside the
window:

``` r

storm_rain <- jam$precip |>
  inner_join(
    jam$links |> filter(PRECIP_IN_FIELD),
    by = join_by(
      ADM2_ID,
      between(LOCAL_DATE, PRECIP_WINDOW_START, PRECIP_WINDOW_END)
    )
  )
```

This expands the data: a day inside two windows now appears twice, once
per storm. That is correct and intended – you asked for storm-attributed
rainfall, and that day is attributable to both. What matters is that you
know it happened, so you can decide what to do about it.

**If you want total rainfall per unit**, do not sum the joined table.
Aggregate the un-joined one:

``` r

total_rain <- jam$precip |>
  group_by(ADM2_ID) |>
  summarise(total_mm = sum(PRECIP_MM, na.rm = TRUE))
```

**If you want rainfall per storm**, sum the joined table, accepting that
a shared day contributes to both storms:

``` r

rain_by_storm <- storm_rain |>
  group_by(STORM_ID, ADM2_ID) |>
  summarise(storm_mm = sum(PRECIP_MM, na.rm = TRUE), .groups = "drop")
```

**If you want each day attributed to exactly one storm**, choose a rule
and apply it explicitly. Nearest approach is a defensible one:

``` r

one_storm_per_day <- storm_rain |>
  group_by(ADM2_ID, LOCAL_DATE) |>
  slice_min(STORM_DIST_KM, n = 1, with_ties = FALSE) |>
  ungroup()
```

Checking whether overlap affects your data at all is worth a moment:

``` r

storm_rain |>
  count(ADM2_ID, LOCAL_DATE) |>
  filter(n > 1)
```

## Adjusting the windows

All six window parameters are independent:

``` r

res <- gtropic_data(
  WHO_ENTITY = "Philippines",
  date_range = 2013,
  precip_days_before = 3,
  precip_days_after = 3,
  precip_radius_km = 300,
  flood_days_before = 1,
  flood_days_after = 14,
  flood_radius_km = 500
)
```

The asymmetry above is a realistic choice: rainfall arrives with the
storm and stops soon after, while flooding may not begin until days
later and can persist for weeks. A narrower rainfall radius attributes
rain more conservatively.

There is no universally correct setting. Whatever you choose is recorded
on the result, so it can be reported:

``` r

gtropic_filters(res)[c("precip_days_before", "precip_days_after",
                       "precip_radius_km")]
```

## Flood matching

Floods span intervals, so “inside the window” needs defining. Two rules
are available through `flood_match`.

**`"start"`** (the default) matches floods whose `FLOOD_START_DATE`
falls inside the window – floods the storm plausibly caused.

**`"overlap"`** also matches floods already under way when the storm
arrived: a flood beginning on or before the window’s end and finishing
on or after its start.

``` r

caused <- gtropic_data(WHO_ENTITY = "Philippines", date_range = 2013,
                       flood_match = "start")

any_overlap <- gtropic_data(WHO_ENTITY = "Philippines", date_range = 2013,
                            flood_match = "overlap")
```

Which you want depends on the question. For attributing flooding to a
storm, `"start"` is the conservative choice. For studying compound
events – a storm striking an already-flooded region, where the
consequences are worse than either hazard alone – `"overlap"` is the one
that will find them.

A consequence worth knowing: `"overlap"` reaches back one calendar year
further when fetching data, because a flood in progress may have begun
in the previous December. That happens automatically.

## Distances come from the dataset

`STORM_DIST_KM` is authoritative and should never be recomputed from the
`tracks` table. The published distance derives from a track interpolated
to fifteen-minute steps, while `tracks` ships the raw best-track
observations at roughly three-hourly intervals. Recomputing from those
would give systematically different numbers, and your results would then
disagree with the published dataset for no good reason.

The `tracks` table is there for mapping and for describing a storm’s
path, not for redoing the exposure calculation.

## Summary

- Rainfall and flooding are linked to storms per unit-storm pair, by a
  temporal window and a distance radius.
- Your geographic filter bounds everything; the radius only narrows
  within it.
- Pairs with no wind are included – that is the point.
- Tables are returned at their natural grain and never duplicated. Join
  through `links` yourself, and be deliberate about aggregation
  afterwards.
- Window and radius settings are recorded on the result, so report them.

# Plot locally available G-TROPIC data

Creates exposure maps, exposure-count maps, and storm-track maps from
tables already returned by
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).
Plotting never retrieves data or recalculates published exposure values
and storm distances.

## Usage

``` r
gtropic_plot(
  data,
  type = c("exposure", "count", "track"),
  metric = NULL,
  storm = NULL,
  adm2 = NULL,
  aggregate = NULL,
  threshold = NULL,
  binary = FALSE,
  count_unit = c("storms", "days"),
  facet_by = NULL,
  track = NULL,
  track_map_key = saffir_simpson(),
  track_labels = TRUE,
  track_interval = 6,
  track_legend_position = NULL,
  basemap = c("countries", "admin1", "none"),
  bbox = NULL,
  expand = 2,
  crs = NULL,
  interactive = FALSE,
  palette = NULL,
  direction = NULL,
  gridlines = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- data:

  A `gtropic_data` object returned by
  [`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md),
  or a uniquely named list of equivalent local tables. Data are never
  downloaded and published exposure values, distances, and tracks are
  not recalculated.

- type:

  The output type. One of `"exposure"` (the default), `"count"`, or
  `"track"`. Exposure maps summarize the selected metric; count maps
  count observations meeting its exposure threshold; track maps draw
  storm positions.

- metric:

  A supported published metric name, or a character vector of metric
  names for exposure or count maps. Its order determines panel, scale,
  and caption order. `NULL` selects the first available default:
  `"VMAX_SUST_MS"`, `"PRECIP_MM"`, or `"PROP_FLOODED_POP"` for
  exposure/count maps and `"USA_WIND"` for track maps. Track maps
  require exactly one of `"USA_WIND"` or `"USA_STATUS"`.

- storm:

  Optional character vector of storm IDs or names used to subset all
  applicable tables. Names require `storm_metadata` and are matched
  case-insensitively. `NULL` (the default) retains all storms.

- adm2:

  Optional character vector of ADM2 IDs used to subset all applicable
  tables. `NULL` (the default) retains all ADM2 units. For a track map,
  this retains tracks linked to the selected ADM2 units when linkage
  data are present.

- aggregate:

  Summary method for exposure maps: one of `"max"`, `"min"`, `"sum"`,
  `"mean"`, or `"n"`. `NULL` (the default) uses the metric-specific
  published default. A single value is recycled over `metric`; multiple
  values must either be in metric order or be named for every selected
  metric. Count maps always use `"n"` and reject another value; this
  argument does not affect track maps.

- threshold:

  A named list of finite numeric thresholds, with names matching
  selected metrics. `NULL` (the default) uses a built-in threshold where
  one is defined. Thresholds determine which observations are counted
  for `type = "count"` and which areas are classified for
  `binary = TRUE`; an explicit threshold is required when no default
  exists. It does not affect continuous exposure maps or track maps.

- binary:

  Logical; if `TRUE`, show exposure/count maps as exposed or not exposed
  using `threshold`, rather than as a continuous value scale. Defaults
  to `FALSE`. It is not available for track maps.

- count_unit:

  The unit counted by a count map: distinct qualifying `"storms"` (the
  default) or `"days"`. It applies only when `type = "count"` and is
  otherwise ignored.

- facet_by:

  An optional single column name in the assembled plotting data to use
  for facets. `NULL` (the default) adds no extra facet; multiple metrics
  always receive separate panels. The column must be available after
  matching and aggregation.

- track:

  Whether to overlay tracks on exposure/count maps. `NULL` (the default)
  overlays them when `data` contains a `tracks` table; `TRUE` or `FALSE`
  explicitly enables or suppresses the overlay. Track maps always draw
  tracks regardless of this value.

- track_map_key:

  A data frame defining wind categories for track points, with
  `category`, increasing `threshold` ceilings ending in `Inf`, one
  `unit`, and either `color` or `colour`. Defaults to
  [`saffir_simpson()`](https://sparklabnyc.github.io/gtropic/reference/saffir_simpson.md).
  It controls `USA_WIND` track points and exposure/count track overlays,
  but not a `USA_STATUS` track map.

- track_labels:

  Logical; if `TRUE` (the default), static maps label each storm with
  more than one valid observation at its first valid position when
  multiple storms are shown. It is ignored for interactive maps.

- track_interval:

  Interval in UTC hours at which track intensity points are displayed: 3
  or 6 (the default). Paths always retain every observation. `NULL`
  displays every point. It affects track maps and enabled track
  overlays.

- track_legend_position:

  Position of per-panel metric scale strips on static exposure/count
  maps. `NULL` (the default) places horizontal strips below the panels.
  `"top"`, `"bottom"`, `"left"`, `"right"`, `"top left"`, `"top right"`,
  `"bottom left"`, and `"bottom right"` place them in each panel. It is
  ignored for track and interactive maps.

- basemap:

  Installed Natural Earth layer: `"countries"` (the default),
  `"admin1"`, or `"none"`. No layer is downloaded. `"admin1"` requires
  the optional `rnaturalearthhires` package.

- bbox:

  Map extent as an sf/sfc object, an sf bbox with a known CRS, or
  geographic `c(xmin, xmax, ymin, ymax)`. `NULL` (the default) derives
  the extent from the plotted geometry and any displayed tracks.

- expand:

  Nonnegative geographic-degree padding added to each side of the map
  extent after `bbox` is resolved. Defaults to 2.

- crs:

  A coordinate reference system understood by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).
  `NULL` (the default) selects EPSG:4326 for longitudinal spans under 40
  degrees and Equal Earth for wider extents. Static maps use this
  projection; interactive Leaflet maps are always rendered in geographic
  coordinates.

- interactive:

  Logical; if `TRUE`, return an offline Leaflet exposure or count map
  instead of a ggplot. Defaults to `FALSE`; track maps cannot be
  interactive.

- palette:

  Viridis palette name or letter, or a vector of them. `NULL` (the
  default) uses metric-specific palettes. One value is recycled over
  `metric`; multiple values must be in metric order or named for every
  metric. It applies to exposure/count fills and a `USA_STATUS` track
  map, but is ignored for categorical `USA_WIND` track points controlled
  by `track_map_key`.

- direction:

  Palette direction: 1 (the default) or -1. It accepts the same scalar,
  positional, and named forms as `palette`, and applies in the same
  situations.

- gridlines:

  Logical; if `TRUE` (the default), static maps show gridlines and
  longitude/latitude labels. It is ignored for Leaflet maps, which have
  no static axes.

- title, subtitle, caption:

  Plot labels. Each defaults to `NULL`, which uses an automatic
  title/subtitle/caption. A supplied caption replaces the automatic
  provenance and method caption.

- quiet:

  Logical; if `FALSE` (the default), report data-preparation progress.

- ...:

  Named arguments passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  for static exposure or count polygons, or to
  [`leaflet::addPolygons()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
  for interactive polygons. Track plots accept no additional arguments.

## Value

A ggplot for static output or a leaflet widget for interactive output.

## Details

Maps use the actual date range of contributing observations after storm,
ADM2, and linkage filters. Metric panels use readable metric names.
Automatic captions report the applicable radii, windows, aggregations,
track interval, and plotting CRS in metric order.

Track paths are drawn from every supplied observation. By default,
constant- size points use
[`saffir_simpson()`](https://sparklabnyc.github.io/gtropic/reference/saffir_simpson.md)
categories derived from `USA_WIND`, which is published in knots and
converted to the map key's unit before classification. An explicit
`metric = "USA_STATUS"` retains the status-colour display.

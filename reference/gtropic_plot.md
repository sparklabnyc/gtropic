# Plot G-TROPIC data as static or interactive maps

Creates exposure maps, exposure-day or exposure-storm count maps, and
storm-track maps from data retrieved by
[`gtropic_data()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_data.md).
This function never retrieves data itself, nor does it recalculate
published exposure values, storm distances, or metadata.

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
  wind_categories = NULL,
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
  or a named list of equivalent tables.

- type:

  The output type. One of `"exposure"` (the default), `"count"`, or
  `"track"`. Exposure maps summarize the specified `metric` argument;
  count maps plot exposure-day or exposure-storm counts that meet
  exposure-specific `threshold`s; and track maps draw storm track paths.

- metric:

  A supported published metric name, or a character vector of metric
  names to plot. The `metric`s are plotted in the order they are given.
  For `type = "exposure"`, `NULL` selects every available core metric in
  this order: `"VMAX_SUST_MS"`, `"PRECIP_MM"`, and
  `"PROP_FLOODED_AREA"`. For `type = "count"`, it selects the first
  available count default; `type = "track"` selects `"USA_WIND"`. Track
  maps require exactly one of `"USA_WIND"` or `"USA_STATUS"`. See
  [`gtropic_exposures()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_exposures.md)
  for all available `metric`s available for plotting.

- storm:

  Optional character vector of `STORM_ID`s or `STORM_NAME`s used to
  subset all applicable tables in `data`. Specifying `storm`s requires
  that the `data` object passed in contains the `storm_metadata` table.
  `STORM_NAME`s are matched case-insensitively. `storm = NULL` (the
  default) retains all storms.

- adm2:

  Optional character vector of `ADM2_ID`s used to subset all applicable
  tables in `data`. `adm2 = NULL` (the default) retains all ADM2 units.
  For a track map, this retains tracks linked to the selected ADM2 units
  when the `links` table is present in `data`.

- aggregate:

  Summary method for exposure maps: one of `"max"`, `"min"`, `"sum"`,
  `"mean"`, or `"n"`. `aggregate = NULL` (the default) uses the
  metric-specific published default (see the `aggregate` column of the
  tables returned by
  [`gtropic_exposures()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_exposures.md)
  for these defaults). A single value is recycled over `metric`;
  multiple values must either be in `metric` order or be named for every
  selected `metric`. Count maps always use `"n"` and reject another
  value; this argument is ignored when `type = "track"` is passed.

- threshold:

  A named list of finite numeric thresholds, with names matching
  selected `metric`s. `threshold = NULL` (the default) uses a built-in
  threshold where one is defined. Thresholds determine which
  observations are counted for `type = "count"` and which areas are
  classified for `binary = TRUE`; an explicit threshold is required when
  no default exists. This argument is ignored when `binary = FALSE` and
  `type = "exposure"` or `"track"`.

- binary:

  Logical; if `TRUE`, show exposure/count maps as exposed or not exposed
  using `threshold`, rather than as a continuous value scale. Defaults
  to `FALSE`. Ignored when `type = "track"`.

- wind_categories:

  Optional categorical exposure key or named list of keys. A key is a
  data frame with `category`, numeric upper `threshold`, constant
  `unit`, and `color` columns. It classifies aggregated ADM2 values,
  retaining every key category in the static and interactive legends
  even when unobserved. With multiple selected metrics, supply a
  uniquely named list keyed by selected wind metric; a bare key is
  allowed only for one selected metric. Keys must use the source
  metric's unit; conventional spellings of meters per second, such as
  `"m/s"` and `"meters per second"`, are treated as compatible. The
  [`saffir_simpson()`](https://sparklabnyc.github.io/gtropic/reference/saffir_simpson.md)
  key is allowed only for `VMAX_SUST_MS` with `aggregate = "max"`, where
  it means a local sustained-wind equivalent, not a storm status. Other
  wind metrics or aggregates require an explicitly custom key. Applies
  only to non-binary exposure maps and wind metrics.

- count_unit:

  The unit counted when `type = "count"`: distinct qualifying `"storms"`
  (the default) or `"days"`. It applies only when `type = "count"` and
  is otherwise ignored.

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

  A data frame defining the discrete color scale used when plotting
  track points. Defaults to the data frame returned by
  [`saffir_simpson()`](https://sparklabnyc.github.io/gtropic/reference/saffir_simpson.md).
  The `track_map_key` must contain the following four columns:
  `category`, `threshold`, `unit`, and `color`. The `unit` column must
  contain a single, constant value, and can be one of: `"m/s"`,
  `"knots"`, `"mph"`, `"km/h"`. This key controls `USA_WIND` track
  points and exposure/count track overlays, but not a `USA_STATUS` track
  map. Static wind legends display every category in the supplied key,
  including categories absent from the plotted observations.

- track_labels:

  Logical; if `TRUE` (the default), static maps label each storm with
  more than one valid observation at its first valid position when
  multiple storms are shown. It is ignored for interactive maps.

- track_interval:

  Interval in UTC hours at which track intensity points are displayed.
  Can be `3` or `6` (the default). Paths always retain every
  observation. `NULL` displays every point. This argument affects
  `type = "track"` maps and enabled track overlays.

- track_legend_position:

  Position of per-panel metric scale strips on static exposure/count
  maps. `NULL` (the default) places horizontal strips below the panels.
  Can be one of `"top"`, `"bottom"`, `"left"`, `"right"`, `"top left"`,
  `"top right"`, `"bottom left"`, or `"bottom right"`. This argument is
  ignored when `type = "track"` or `interactive = TRUE`.

- basemap:

  The Installed Natural Earth layer to use for plotting. Must be one of
  `"countries"` (the default), `"admin1"`, or `"none"`. No layers are
  ever downloaded. `basemap = "admin1"` requires the optional
  `rnaturalearthhires` package.

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
ADM2, and linkage filters. Automatic subtitles begin with
`"Aggregated across"` and show the displayed date range. Single-metric
titles use readable aggregate labels; multi-metric titles describe the
panel set. A single plotted storm prefixes the title, while multiple
storms do not alter it. Metric strips use readable aggregate and
active-threshold labels. Automatic captions use newline-separated
source, retrieval, dataset-provenance, hazard-parameter, track-interval,
and plotting-CRS lines. Hazard radii and windows are derived from rows
that contributed to the plot.

Track paths are drawn from every supplied observation. By default,
constant- size points use
[`saffir_simpson()`](https://sparklabnyc.github.io/gtropic/reference/saffir_simpson.md)
categories derived from `USA_WIND`, which is published in knots and
converted to the map key's unit before classification. An explicit
`metric = "USA_STATUS"` retains the status-color display.

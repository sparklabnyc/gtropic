#' Plot G-TROPIC data as static or interactive maps
#'
#' Creates exposure maps, exposure-day or exposure-storm count maps, and
#' storm-track maps from data retrieved by [gtropic_data()]. This function never
#' retrieves data itself, nor does it recalculate published exposure values,
#' storm distances, or metadata.
#'
#' @param data A `gtropic_data` object returned by [gtropic_data()], or a named
#'   list of equivalent tables.
#' @param type The output type. One of `"exposure"` (the default), `"count"`, or
#'   `"track"`. Exposure maps summarize the specified `metric` argument; count
#'   maps plot exposure-day or exposure-storm counts that meet exposure-specific
#'   `threshold`s; and track maps draw storm track paths.
#' @param metric A supported published metric name, or a character vector of metric
#'   names to plot. The `metric`s are plotted in the order they are given.
#'   For `type = "exposure"`, `NULL` selects every available core metric in this
#'   order: `"VMAX_SUST_MS"`, `"PRECIP_MM"`, and `"PROP_AREA_IN_FLOOD_REPORT"`,
#'   the last drawn as a yes/no flood-report panel (see Details); name it in
#'   `metric` to map the share itself.
#'   For `type = "count"`, it selects the first available count default;
#'   `type = "track"` selects `"USA_WIND"`. Track maps require exactly one of
#'   `"USA_WIND"` or `"USA_STATUS"`. See [gtropic_metrics()] for all available
#'   `metric`s available for plotting.
#' @param storm Optional character vector of `STORM_ID`s or `STORM_NAME`s used
#'   to subset all applicable tables in `data`. Specifying `storm`s requires that the `data`
#'   object passed in contains the `storm_metadata` table. `STORM_NAME`s are matched
#'   case-insensitively and a complete name can select multiple `STORM_ID`s,
#'   including across basins. Names are convenience labels in `Name-YYYY` form,
#'   where `YYYY` is the published `SEASON` year, not necessarily the genesis
#'   calendar year. Prefer unique `STORM_ID`s for unambiguous plots.
#'   `storm = NULL` (the default) retains all storms.
#' @param adm2 Optional character vector of `ADM2_ID`s used to subset all applicable
#'   tables in `data`. `adm2 = NULL` (the default) retains all ADM2 units. For a track map, this
#'   retains tracks linked to the selected ADM2 units when the `links` table is present in `data`.
#' @param aggregate Summary method for exposure maps: one of `"max"`, `"min"`,
#'   `"sum"`, `"mean"`, or `"n"`. `aggregate = NULL` (the default) uses the metric-specific
#'   published default (see the `aggregate` column of the tables returned by
#'   [gtropic_metrics()] for these defaults).
#'   A single value is recycled over `metric`; multiple values
#'   must either be in `metric` order or be named for every selected `metric`. Count
#'   maps always use `"n"` and reject another value; this argument is ignored when
#'   `type = "track"` is passed.
#' @param threshold A named list of finite numeric thresholds, with names matching
#'   selected `metric`s. `threshold = NULL` (the default) uses a built-in threshold where one is
#'   defined. Thresholds determine which observations are counted for
#'   `type = "count"` and which areas are classified for `binary = TRUE`; an explicit
#'   threshold is required when no default exists. This argument is ignored when
#'   `binary = FALSE` and `type = "exposure"` or `"track"`.
#' @param binary Logical; default `FALSE`. `TRUE` shows exposure and count maps
#'   as exposed or not exposed, using `threshold`, instead of a continuous
#'   scale; flood panels show "Flood reported" or "No flood reported". The
#'   default exposure map's flood panel is yes/no even when `binary = FALSE`.
#'   Ignored for track maps.
#' @param wind_categories Optional key that classifies aggregated wind values
#'   into categories instead of a continuous scale: a data frame with
#'   `category`, upper `threshold`, constant `unit` and `color` columns, or a
#'   list of such keys named by wind metric. `NULL` (the default) uses the
#'   continuous scale. Applies only to non-binary exposure maps of wind
#'   metrics. [gtropic_saffir_simpson()] is allowed only for `VMAX_SUST_MS`
#'   with `aggregate = "max"`; see `vignette("plotting", package = "gtropic")`
#'   for the rules a custom key must follow.
#' @param count_unit The unit counted when `type = "count"`: distinct qualifying
#'   `"storms"` (the default) or `"days"`. It applies only when `type = "count"`
#'   and is otherwise ignored.
#' @param facet_by An optional single column name in the assembled plotting data to
#'   use for facets. `NULL` (the default) adds no extra facet; multiple metrics
#'   always receive separate panels. The column must be available after matching
#'   and aggregation.
#' @param track Whether to overlay tracks on exposure/count maps. `NULL` (the
#'   default) overlays them when `data` contains a `tracks` table; `TRUE` or
#'   `FALSE` explicitly enables or suppresses the overlay. Track maps always draw
#'   tracks regardless of this value.
#' @param track_map_key A data frame defining the discrete color scale used when
#'   plotting track points. Defaults to the data frame returned by
#'   [gtropic_saffir_simpson()]. The `track_map_key` must contain the following
#'   four columns: `category`, `threshold`, `unit`, and `color`.
#'   The `unit` column must contain a single, constant value, and can be one of:
#'   `"m/s"`, `"knots"`, `"mph"`, `"km/h"`. This key controls
#'   `USA_WIND` track points and exposure/count track overlays, but not a
#'   `USA_STATUS` track map. Static wind legends display every category in the
#'   supplied key, including categories absent from the plotted observations.
#' @param track_labels Logical; if `TRUE` (the default), static maps label each
#'   storm with more than one valid observation at its first valid position when
#'   multiple storms are shown. It is ignored for interactive maps.
#' @param track_interval Hours (UTC) between displayed track intensity points:
#'   `6` (the default), `3`, or `NULL` for every point. Paths always use every
#'   observation. Affects track maps and track overlays.
#' @param track_legend_position Position of per-panel metric scale strips on static
#'   exposure/count maps. `NULL` (the default) places horizontal strips below the
#'   panels. Can be one of `"top"`, `"bottom"`, `"left"`, `"right"`, `"top left"`,
#'   `"top right"`, `"bottom left"`, or `"bottom right"`.
#'   This argument is ignored when `type = "track"` or `interactive = TRUE`.
#' @param basemap Natural Earth background layer: `"countries"` (the
#'   default), `"admin1"` or `"none"`. Layers come from installed packages and
#'   are never downloaded; `"admin1"` needs the optional `rnaturalearthhires`
#'   package.
#' @param bbox Map extent as an sf/sfc object, an sf bbox with a known CRS, or
#'   four geographic bounds in [sf::st_bbox()] order:
#'   `c(xmin, ymin, xmax, ymax)`. `NULL` (the default) derives the extent from
#'   the plotted geometry and any displayed tracks.
#' @param expand Padding (degrees, 0 or more) added to each side of the map
#'   extent; default 2. Latitude limits stay within -90 to 90.
#' @param crs A coordinate reference system understood by [sf::st_crs()]. `NULL`
#'   (the default) selects EPSG:4326 for longitudinal spans under 40 degrees and
#'   Equal Earth for wider extents. Static maps use this projection; interactive
#'   Leaflet maps are always rendered in geographic coordinates.
#' @param interactive Logical; if `TRUE`, return an offline Leaflet exposure or
#'   count map instead of a ggplot. Defaults to `FALSE`; track maps cannot be
#'   interactive.
#' @param palette Viridis palette name or letter, or a vector of them. `NULL` (the
#'   default) uses metric-specific palettes. One value is recycled over `metric`;
#'   multiple values must be in metric order or named for every metric. It applies
#'   to exposure/count fills and a `USA_STATUS` track map, but is ignored for
#'   categorical `USA_WIND` track points controlled by `track_map_key`.
#' @param direction Palette direction: 1 (the default) or -1. It accepts the same
#'   scalar, positional, and named forms as `palette`, and applies in the same
#'   situations.
#' @param gridlines Logical; if `TRUE` (the default), static maps show gridlines
#'   and longitude/latitude labels. It is ignored for Leaflet maps, which have no
#'   static axes.
#' @param make_valid Logical. If `TRUE` (the default), invalid ADM2 geometries
#'   are repaired with [sf::st_make_valid()] before plotting. Set to `FALSE` to
#'   preserve supplied geometry or avoid the validity-check and repair cost.
#'   Repair can change geometry types. This argument is ignored for track maps.
#' @param title,subtitle,caption Plot labels. Each defaults to `NULL`, which uses
#'   an automatic title/subtitle/caption. A supplied caption replaces the automatic
#'   provenance and method caption.
#' @param quiet Logical; if `FALSE`, report data-preparation progress. Defaults
#'   to `getOption("gtropic.quiet", FALSE)`.
#' @param ... For static exposure and count maps, named arguments passed to
#'   [ggplot2::geom_sf()] for the ADM2 polygons. For interactive maps, named
#'   arguments passed to [leaflet::addPolygons()]. Track plots accept none.
#'
#' @section Arguments by purpose:
#' - **What to plot:** `data`, `type`, `metric`, `storm`, `adm2`, `aggregate`,
#'   `threshold`, `binary`, `wind_categories`, `count_unit`, `facet_by`.
#' - **Tracks:** `track`, `track_map_key`, `track_labels`, `track_interval`,
#'   `track_legend_position`.
#' - **Map frame:** `basemap`, `bbox`, `expand`, `crs`, `interactive`.
#' - **Appearance:** `palette`, `direction`, `gridlines`, `make_valid`,
#'   `title`, `subtitle`, `caption`, `quiet`, `...`.
#'
#' @details
#' Maps use the actual date range of contributing observations after storm, ADM2,
#' and linkage filters. Automatic subtitles begin with `"Aggregated across"` and
#' show the displayed date range. Single-metric titles use readable aggregate
#' labels; multi-metric titles describe the panel set. A single plotted storm
#' prefixes the title, while multiple storms do not alter it. Metric strips use
#' readable aggregate and active-threshold labels. Automatic captions use
#' newline-separated source, retrieval, dataset-provenance, hazard-parameter,
#' known source-data limitation, track-interval, and plotting-CRS lines as
#' applicable. Hazard radii and windows are derived from rows that contributed
#' to the plot.
#'
#' Flood rows record places named in news reports about flooding, not mapped
#' flood extent ([gtropic_known_limitations]). A yes/no flood panel therefore
#' shows "Flood reported" when at least one report's place covers a share of
#' the unit above the threshold (default: more than 0) in a matched storm's
#' flood window, and "No flood reported" when the unit was in a storm's flood
#' field, its window lies inside the Groundsource record (2000-01-01 to
#' 2026-01-22) and no report matched. Units whose windows fall outside the
#' record have no flood data and are shown as missing.
#'
#' Track paths are drawn from every supplied observation. By default, constant-
#' size points use [gtropic_saffir_simpson()] categories derived from
#' `USA_WIND`, which is published in knots and converted to the map key's unit
#' before classification.
#' An explicit `metric = "USA_STATUS"` retains the status-color display.
#'
#' @return A ggplot for static output or a leaflet widget for interactive output.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)
#' x <- gtropic_example()
#'
#' # Exposure map of maximum sustained wind
#' gtropic_plot(x, metric = "VMAX_SUST_MS", basemap = "none")
#'
#' # Track map
#' gtropic_plot(x, type = "track", basemap = "none")
#' @family plotting functions
#' @export
gtropic_plot <- function(
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
  track_map_key = gtropic_saffir_simpson(),
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
  make_valid = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  quiet = getOption("gtropic.quiet", FALSE),
  ...
) {
  type <- rlang::arg_match(type)
  count_unit <- rlang::arg_match(count_unit)
  basemap <- rlang::arg_match(basemap)
  check_flag(binary)
  check_flag(interactive)
  check_flag(quiet)
  check_flag(gridlines)
  check_flag(track_labels)
  check_flag(make_valid)
  if (!is.null(track)) {
    check_flag(track)
  }
  if (interactive && !type %in% c("exposure", "count")) {
    cli::cli_abort(
      "Interactive plots support only type = 'exposure' or 'count'."
    )
  }
  if (binary && !type %in% c("exposure", "count")) {
    cli::cli_abort("{.arg binary} applies only to exposure and count maps.")
  }
  plot_check_strings(metric, "metric")
  plot_check_strings(storm, "storm")
  plot_check_strings(adm2, "adm2")
  plot_check_strings(facet_by, "facet_by")
  if (!is.null(facet_by) && length(facet_by) != 1L) {
    cli::cli_abort("{.arg facet_by} must be one string or NULL.")
  }
  if (
    !is.numeric(expand) ||
      length(expand) != 1L ||
      !is.finite(expand) ||
      expand < 0
  ) {
    cli::cli_abort("{.arg expand} must be one nonnegative finite number.")
  }
  track_map_key <- plot_validate_track_map_key(track_map_key)
  track_interval <- plot_validate_track_interval(track_interval)
  track_legend_position <- plot_validate_legend_position(
    track_legend_position
  )
  plot_validate_static_dots(type, interactive, ...)

  prepared <- prepare_plot_data(
    data,
    type = type,
    metric = metric,
    storm = storm,
    adm2 = adm2,
    aggregate = aggregate,
    threshold = threshold,
    binary = binary,
    wind_categories = wind_categories,
    count_unit = count_unit,
    facet_by = facet_by,
    track = track,
    palette = palette,
    direction = direction,
    track_map_key = track_map_key,
    track_labels = track_labels,
    track_interval = track_interval,
    track_legend_position = track_legend_position,
    make_valid = make_valid,
    quiet = quiet
  )
  rlang::check_installed("ggplot2", reason = "to build G-TROPIC plots")
  if (interactive) {
    rlang::check_installed("leaflet", reason = "to build interactive maps")
  }
  cartography <- plot_cartography(
    prepared,
    basemap = basemap,
    bbox = bbox,
    expand = expand,
    crs = crs
  )
  if (interactive) {
    return(build_interactive_plot(
      prepared,
      cartography,
      title = title,
      subtitle = subtitle,
      caption = caption,
      gridlines = gridlines,
      ...
    ))
  }
  builder <- switch(
    type,
    exposure = build_exposure_plot,
    count = build_count_plot,
    track = build_track_plot
  )
  builder(
    prepared,
    cartography,
    title = title,
    subtitle = subtitle,
    caption = caption,
    gridlines = gridlines,
    ...
  )
}

#' Plot a G-TROPIC retrieval result
#'
#' Thin S3 interface to [gtropic_plot()].
#' @param x A `gtropic_data` object.
#' @param type Plot type, defaulting to `"exposure"`.
#' @param ... Arguments passed to [gtropic_plot()].
#' @return The plot object returned by [gtropic_plot()].
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)
#' plot(gtropic_example(), metric = "VMAX_SUST_MS", basemap = "none")
#' @family plotting functions
#' @export
plot.gtropic_data <- function(x, type = "exposure", ...) {
  gtropic_plot(x, type = type, ...)
}

#' Reject static geom arguments that would be ignored or collide with builders
#' @keywords internal
#' @noRd
plot_validate_static_dots <- function(type, interactive, ...) {
  if (interactive || ...length() == 0L) {
    return(invisible(NULL))
  }
  if (type == "track") {
    cli::cli_abort("Unsupported arguments in {.arg ...} for track plots.")
  }
  dots <- list(...)
  nms <- names(dots)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    cli::cli_abort("All static plotting arguments in {.arg ...} must be named.")
  }
  nms[nms == "color"] <- "colour"
  if (anyDuplicated(nms)) {
    cli::cli_abort("Duplicate static plotting arguments in {.arg ...}.")
  }
  conflicts <- intersect(nms, c("data", "mapping", "inherit.aes"))
  if (length(conflicts)) {
    cli::cli_abort(
      "Arguments in {.arg ...} conflict with the map builder: {.val {conflicts}}."
    )
  }
  rlang::check_installed("ggplot2", reason = "to validate plotting arguments")
  supported <- c(
    names(formals(ggplot2::geom_sf)),
    ggplot2::GeomSf$parameters(extra = TRUE),
    ggplot2::GeomSf$aesthetics(),
    "key_glyph"
  )
  unknown <- setdiff(nms, supported)
  if (length(unknown)) {
    cli::cli_abort(
      "Unknown static plotting arguments in {.arg ...}: {.val {unknown}}."
    )
  }
  invisible(NULL)
}

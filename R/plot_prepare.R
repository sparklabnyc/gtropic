#' Built-in plotting metrics and scientifically explicit defaults
#'
#' Missing default thresholds are intentional: callers must supply a definition
#' rather than borrowing the sustained-wind criterion for other metrics.
#' @keywords internal
#' @noRd
plot_metric_registry <- function(codebook = NULL) {
  out <- tibble::tibble(
    metric = c(
      "VMAX_SUST_MS", "VMAX_GUST_MS", "SUST_DUR_MINUTES", "GUST_DUR_MINUTES",
      "WIND_RADII_VMAX_SUST_MS", "WIND_RADII_SUST_DUR_MINUTES", "STORM_DIST_KM",
      "PRECIP_MM", "PROP_FLOODED_POP", "PROP_FLOODED_AREA",
      "USA_WIND", "USA_STATUS"
    ),
    table = c(rep("wind", 7), "precip", rep("flood", 2), rep("tracks", 2)),
    aggregate = c(rep("max", 6), "min", "sum", rep("max", 4)),
    label = c(
      "Maximum sustained wind (m/s)",
      "Maximum wind gust (m/s)",
      "Sustained wind duration (minutes)", "Wind gust duration (minutes)",
      "Wind radii sustained wind (m/s)",
      "Wind radii duration (minutes)", "Storm distance (km)",
      "Precipitation (mm)",
      "Flooded population (proportion, 0-1)",
      "Flooded area (proportion, 0-1)",
      "Track maximum sustained wind", "Storm status"
    ),
    unit = c(
      "m/s", "m/s", "minutes", "minutes", "m/s", "minutes", "km", "mm",
      "proportion", "proportion", "knots", ""
    ),
    operator = c(rep(">=", 6), "<=", ">=", ">", ">", ">=", "=="),
    threshold = c(17.5, rep(NA_real_, 5), 100, 75, 0, 0, NA_real_, NA_real_),
    palette = c(
      rep("inferno", 7), "mako", "viridis", "viridis",
      "inferno", "viridis"
    )
  )
  for (i in seq_len(nrow(out))) {
    entry <- plot_codebook_entry(codebook, out$metric[i])
    unit <- entry$units %||% entry$unit
    label <- entry$label %||% entry$long_name
    if (is.character(unit) && length(unit) == 1L && !is.na(unit)) {
      out$unit[i] <- unit
    }
    if (is.character(label) && length(label) == 1L && !is.na(label)) {
      out$label[i] <- label
    }
  }
  out$legend <- out$label
  out
}

#' Find a variable definition in nested published codebooks
#' @keywords internal
#' @noRd
plot_codebook_entry <- function(x, metric) {
  if (!is.list(x)) {
    return(NULL)
  }
  if (is.list(x[[metric]])) {
    return(x[[metric]])
  }
  key <- x$variable %||% x$name %||% x$column
  if (is.character(key) && length(key) == 1L && identical(key, metric)) {
    return(x)
  }
  for (part in x) {
    entry <- plot_codebook_entry(part, metric)
    if (!is.null(entry)) {
      return(entry)
    }
  }
  NULL
}

#' Resolve selected metrics, aggregations, and threshold definitions
#' @keywords internal
#' @noRd
resolve_plot_metrics <- function(data, type, metric = NULL, aggregate = NULL,
                                 threshold = NULL, binary = FALSE,
                                 palette = NULL, direction = NULL) {
  registry <- plot_metric_registry(data$codebook)
  candidates <- if (type == "track") {
    registry[registry$table == "tracks", ]
  } else {
    registry[registry$table != "tracks", ]
  }
  available <- candidates$metric[vapply(seq_len(nrow(candidates)), function(i) {
    candidates$metric[i] %in% names(data[[candidates$table[i]]])
  }, logical(1))]
  if (is.null(metric)) {
    if (type == "exposure") {
      # Exposure maps make the core wind, rainfall, and flooded-area measures
      # visible together whenever their source tables are available.
      preferred <- c("VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_AREA")
      metric <- intersect(preferred, available)
      if (!length(metric)) metric <- preferred[1L]
    } else {
      preferred <- if (type == "track") "USA_WIND" else {
        c("VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_POP")
      }
      metric <- intersect(preferred, available)[1L]
      if (is.na(metric)) metric <- preferred[1L]
    }
  }
  if (
    !is.character(metric) || !length(metric) || anyNA(metric) ||
      any(!metric %in% candidates$metric)
  ) {
    cli::cli_abort(c("Unknown or unsupported {.arg metric} for {.val {type}}.",
      i = "Available metrics in the supplied tables: {.val {available}}."
    ))
  }
  if (type == "track" && length(metric) != 1L) {
    cli::cli_abort("Track plots require one metric: USA_WIND or USA_STATUS.")
  }
  if (anyDuplicated(metric)) {
    cli::cli_abort("{.arg metric} cannot contain duplicates.")
  }
  out <- candidates[match(metric, candidates$metric), ]
  out$aggregate <- plot_resolve_metric_option(
    aggregate, out$metric, "aggregate",
    choices = c("max", "min", "sum", "mean", "n"), defaults = out$aggregate
  )
  out$palette <- plot_resolve_metric_option(
    palette, out$metric, "palette",
    choices = c(
      "magma", "inferno", "plasma", "viridis", "cividis", "rocket",
      "mako", "turbo", LETTERS[1:8]
    ), defaults = out$palette
  )
  out$direction <- as.numeric(plot_resolve_metric_option(
    direction, out$metric, "direction",
    choices = c(-1, 1), defaults = rep(1, nrow(out))
  ))
  if (type == "count") {
    if (!is.null(aggregate) && any(out$aggregate != "n")) {
      cli::cli_abort("Count plots require aggregate = 'n';
                      omit {.arg aggregate} to use it.")
    }
    out$aggregate <- "n"
  }
  if (!is.null(threshold)) {
    if (
      !is.list(threshold) || is.null(names(threshold)) ||
        anyNA(names(threshold)) || anyDuplicated(names(threshold)) ||
        any(!names(threshold) %in% out$metric)
    ) {
      cli::cli_abort("{.arg threshold} must be a uniquely named list
                      keyed by selected metrics.")
    }
    for (nm in names(threshold)) {
      value <- threshold[[nm]]
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        cli::cli_abort("Threshold {.val {nm}} must be one finite number.")
      }
      out$threshold[out$metric == nm] <- value
    }
  }
  out$threshold_active <- type == "count" || binary
  out$value_label <- out$label
  out$value_unit <- out$unit
  if (binary) {
    out$value_label <- "Exposed"
    out$value_unit <- ""
    out$legend <- "Exposed"
  }
  missing <- out$metric[out$threshold_active & is.na(out$threshold)]
  if (length(missing)) {
    cli::cli_abort("Supply an explicit {.arg threshold} for {.val {missing}};
                    no default is defined.")
  }
  out
}

#' Resolve scalar, positional, or fully named metric options
#' @keywords internal
#' @noRd
plot_resolve_metric_option <- function(value, metrics, argument, choices, defaults) {
  if (is.null(value)) {
    return(defaults)
  }
  valid <- function(x) {
    !is.null(x) && length(x) && !anyNA(x) && all(x %in% choices)
  }
  if (!valid(value)) {
    cli::cli_abort("{.arg {argument}} must contain only {.val {choices}}.")
  }
  if (!is.null(names(value))) {
    if (anyNA(names(value)) || any(!nzchar(names(value))) || anyDuplicated(names(value)) ||
      !setequal(names(value), metrics)) {
      cli::cli_abort("Named {.arg {argument}} values must name every selected metric exactly once.")
    }
    return(unname(value[metrics]))
  }
  if (length(value) == 1L) {
    return(rep(value, length(metrics)))
  }
  if (length(value) != length(metrics)) {
    cli::cli_abort("{.arg {argument}} must have length one or length {.val {length(metrics)}}.")
  }
  unname(value)
}

#' Resolve optional categorical keys for aggregated exposure metrics
#'
#' A category key uses inclusive upper-bound thresholds and must state the same
#' unit as its source metric. The built-in Saffir-Simpson key is deliberately
#' restricted to the published local maximum sustained-wind measure.
#' @keywords internal
#' @noRd
plot_resolve_wind_categories <- function(wind_categories, type, binary, metrics) {
  if (is.null(wind_categories)) {
    return(list())
  }
  if (type != "exposure") {
    cli::cli_abort("{.arg wind_categories} applies only to type = 'exposure' maps.")
  }
  if (binary) {
    cli::cli_abort("{.arg wind_categories} requires {.code binary = FALSE}.")
  }

  if (is.data.frame(wind_categories)) {
    if (nrow(metrics) != 1L) {
      cli::cli_abort(c(
        "A bare {.arg wind_categories} key requires exactly one selected metric.",
        i = "For multiple metrics, supply a named list keyed by metric."
      ))
    }
    keys <- stats::setNames(list(wind_categories), metrics$metric)
  } else {
    if (!is.list(wind_categories) || !length(wind_categories) ||
      is.null(names(wind_categories)) || anyNA(names(wind_categories)) ||
      any(!nzchar(names(wind_categories))) || anyDuplicated(names(wind_categories)) ||
      any(!names(wind_categories) %in% metrics$metric)) {
      cli::cli_abort("{.arg wind_categories} must be a category-key data frame or a uniquely named list keyed by selected metrics.")
    }
    keys <- wind_categories
  }

  out <- lapply(names(keys), function(metric) {
    key <- plot_validate_wind_category_key(
      keys[[metric]], paste0("wind_categories$", metric)
    )
    spec <- metrics[match(metric, metrics$metric), ]
    if (!identical(as.character(spec$table), "wind")) {
      cli::cli_abort(c(
        "{.arg wind_categories} can classify only selected wind metrics.",
        i = "{.val {metric}} is not a wind exposure metric."
      ))
    }
    standard <- plot_is_saffir_simpson_key(key)
    if (standard && (!identical(metric, "VMAX_SUST_MS") || !identical(as.character(spec$aggregate), "max"))) {
      cli::cli_abort(c(
        "The built-in Saffir-Simpson key is valid only for {.field VMAX_SUST_MS} with {.code aggregate = 'max'}.",
        i = "Provide an explicitly custom wind-category key for other metrics or aggregates."
      ))
    }
    if (!plot_units_are_compatible(key$unit[1L], as.character(spec$unit[1L]))) {
      cli::cli_abort(c(
        "{.arg wind_categories} key for {.val {metric}} has incompatible units.",
        i = "Its unit must be {.val {spec$unit}}, the unit of the numeric source metric."
      ))
    }
    list(
      key = key,
      kind = if (standard) "saffir_simpson" else "custom",
      label = if (standard) {
        "Maximum local sustained wind category (Saffir\u2013Simpson-equivalent)"
      } else {
        paste0("Local exposure category (user-defined; ", key$unit[1L], ")")
      }
    )
  })
  stats::setNames(out, names(keys))
}

#' Categorize numeric aggregated exposure values using a category key
#' @keywords internal
#' @noRd
plot_categorize_wind_exposure <- function(value, key) {
  key <- plot_validate_wind_category_key(key, "wind_categories")
  if (!is.numeric(value)) {
    cli::cli_abort("Categorical exposure values must be numeric.")
  }
  category <- rep(NA_character_, length(value))
  present <- !is.na(value)
  if (any(present)) {
    category[present] <- key$category[vapply(value[present], function(x) {
      which(x <= key$threshold)[1L]
    }, integer(1))]
  }
  factor(category, levels = key$category, ordered = TRUE)
}

#' Subset every local table consistently by storm IDs/names and ADM2 IDs
#' @keywords internal
#' @noRd
subset_plot_data <- function(data, storm = NULL, adm2 = NULL) {
  for (nm in c("storm", "adm2")) {
    value <- get(nm)
    if (is.null(value)) next
    if (!is.character(value) || !length(value) || anyNA(value)) {
      cli::cli_abort("{.arg {nm}} must be a nonempty character vector or NULL.")
    }
  }
  ids <- storm
  if (!is.null(storm) && !is.null(data$storm_metadata)) {
    meta <- data$storm_metadata
    matched <- toupper(meta$STORM_NAME) %in% toupper(storm)
    ids <- unique(c(storm, meta$STORM_ID[matched]))
  }
  for (nm in setdiff(names(data), "codebook")) {
    x <- data[[nm]]
    if (is.null(x)) next
    if (!is.null(ids) && "STORM_ID" %in% names(x)) {
      x <- x[x$STORM_ID %in% ids, , drop = FALSE]
    }
    if (!is.null(adm2) && "ADM2_ID" %in% names(x)) {
      x <- x[x$ADM2_ID %in% adm2, , drop = FALSE]
    }
    data[[nm]] <- x
  }
  data
}

#' Add available ADM2 and storm labels without replacing published columns
#' @keywords internal
#' @noRd
plot_attach_labels <- function(rows, data) {
  for (nm in c("adm2", "storm_metadata")) {
    key <- if (nm == "adm2") "ADM2_ID" else "STORM_ID"
    meta <- data[[nm]]
    if (is.null(meta) || !key %in% names(rows)) next
    cols <- c(key, setdiff(names(meta), names(rows)))
    if (anyDuplicated(meta[[key]])) {
      cli::cli_abort("Table {.val {nm}} must have unique
                      {.field {key}} values.")
    }
    rows <- dplyr::left_join(rows, meta[, cols, drop = FALSE], by = key)
  }
  rows
}

#' Attribute track observations to the metric panels actually mapped
#' @keywords internal
#' @noRd
plot_prepare_overlay <- function(tracks, membership, facet_by) {
  if (is.null(tracks)) {
    return(NULL)
  }
  columns <- unique(c("STORM_ID", "metric", facet_by))
  panels <- dplyr::distinct(membership[, columns, drop = FALSE])
  # Panel values come from mapped rows, including ADM2-only metadata.
  tracks <- dplyr::select(tracks, -dplyr::any_of(setdiff(columns, "STORM_ID")))
  dplyr::inner_join(tracks, panels,
    by = "STORM_ID", relationship = "many-to-many"
  )
}

#' Prepare a plot frame and its builder metadata using only local tables
#'
#' Milestone-2 join contract (no fallback implementations):
#' `plot_join_metric(data, spec, flood_match)` takes normalized/subset tables,
#' a one-row metric registry tibble, and `"start"`/`"overlap"`. It returns a
#' tibble at source-row x matching-storm grain with ADM2_ID, STORM_ID, metric,
#' numeric value, and .plot_row_id (stable source-row identity before joins).
#' Preserve published columns, including dates, distance, and closest approach;
#' precipitation adds Date `date`; flood retains its start/end dates. Use only
#' in-field links and inclusive windows. Never derive distance from tracks.
#'
#' `plot_aggregate_metric(rows, spec, type, count_unit, facet_by, binary)` takes
#' these labeled rows and returns ADM2_ID x metric x facet, with numeric value,
#' metric and facet columns; retain label columns and published STORM_DIST_KM
#' (minimum when combining storms). For binary output add logical `exposed`.
#' spec contains resolved aggregate, operator, threshold and threshold_active.
#' Deduplicate .plot_row_id within each aggregation group before exposure sums;
#' threshold/count operations use the original daily/event/pair values, count
#' distinct storms or dates, and preserve evaluated zero-count units. Missing
#' observations stay missing. Flood days are the union of qualifying event days
#' intersected with the matching windows, never a sum of event durations.
#'
#' Returned maps are sf with ADM2_ID, metric, value and optional exposed/facet
#' columns. Track frames retain published columns, with ISO_TIME parsed to
#' POSIXct UTC.
#' `attr(x, "plot_info")` records type, metrics (resolved registry rows),
#' metric (names), registry (all built-ins), aggregate/threshold (named),
#' binary, count_unit, facet_by, facets (levels), track settings, tracks,
#' plotted pairs, observed date range, filters, flood_match, query, and default
#' caption.
#' @keywords internal
#' @noRd
prepare_plot_data <- function(data, type, metric = NULL, storm = NULL,
                              adm2 = NULL, aggregate = NULL, threshold = NULL,
                              binary = FALSE, count_unit = "storms",
                              facet_by = NULL, track = NULL, ...) {
  dots <- rlang::list2(...)
  unknown <- setdiff(names(dots), c(
    "interactive", "quiet", "palette", "direction", "track_map_key",
    "track_labels", "track_interval", "track_legend_position", "wind_categories"
  ))
  unnamed <- length(dots) &&
    (is.null(names(dots)) || any(!nzchar(names(dots))))
  if (length(unknown) || unnamed) {
    cli::cli_abort("Unused preparation arguments in {.arg ...};
                    supported: interactive, quiet, palette, direction,
                    track_map_key, track_labels, track_interval,
                    track_legend_position, wind_categories.")
  }
  type <- rlang::arg_match(type, c("exposure", "count", "track"))
  count_unit <- rlang::arg_match(count_unit, c("storms", "days"))
  plot_flag(binary, "binary")
  if (binary && !type %in% c("exposure", "count")) {
    cli::cli_abort("{.arg binary} applies only to exposure and count maps.")
  }
  if (is.list(data) && any(vapply(data, inherits, logical(1), what = "sf"))) {
    rlang::check_installed("sf", reason = "to preserve supplied map geometry")
    loadNamespace("sf")
  }
  data <- normalize_plot_data(data)
  track_default <- !is.null(data$tracks)
  track <- track %||% track_default
  if (type == "track") track <- TRUE
  plot_flag(track, "track")
  metrics <- resolve_plot_metrics(
    data, type, metric,
    aggregate, threshold, binary, dots$palette, dots$direction
  )
  wind_categories <- plot_resolve_wind_categories(
    dots$wind_categories, type, binary, metrics
  )
  if (type == "count") {
    # Counts summarize precipitation observations, so retain its measure but
    # not the source-table's daily-rate suffix in count-map labels.
    precipitation <- metrics$metric == "PRECIP_MM"
    metrics$label[precipitation] <- gsub(
      "\\s+per\\s+local\\s+day\\b", "", metrics$label[precipitation],
      ignore.case = TRUE
    )
  }
  if (type == "count" && !binary) {
    metrics$value_label <- paste("Number of", count_unit)
    metrics$value_unit <- count_unit
    metrics$legend <- metrics$value_label
  }
  if (is.null(facet_by) && nrow(metrics) > 1L) facet_by <- "metric"
  requirements <- plot_requirements(type, metrics, track, count_unit)
  if (type == "track") {
    requirements$tracks <- unique(c(requirements$tracks, metrics$metric))
  }
  plot_require(data, requirements, type)
  validate_plot_values(data, metrics, track || type == "track")
  data <- subset_plot_data(data, storm, adm2)
  filters <- attr(data, "filters_resolved", exact = TRUE) %||% list()
  flood_match <- filters$flood_match %||% "start"
  flood_match <- rlang::arg_match(flood_match, c("start", "overlap"))
  tracks <- if (track || type == "track") {
    plot_attach_labels(data$tracks, data)
  } else {
    NULL
  }
  membership <- NULL
  if (type == "track") {
    tracks$ISO_TIME <- plot_parse_utc(tracks$ISO_TIME, "ISO_TIME")
    if (!is.null(adm2) && !is.null(data$links) && "STORM_ID" %in% names(data$links)) {
      tracks <- tracks[tracks$STORM_ID %in% unique(data$links$STORM_ID), , drop = FALSE]
    }
    if (!nrow(tracks)) cli::cli_abort("No track observations remain after subsetting.")
    out <- tracks
  } else {
    raw_parts <- lapply(seq_len(nrow(metrics)), function(i) {
      spec <- metrics[i, ]
      # The two join/aggregate functions are supplied by milestone 2.
      rows <- plot_join_metric( # nolint: object_usage_linter.
        data, spec,
        flood_match = flood_match
      )
      rows <- plot_attach_labels(rows, data)
      validate_plot_facet(rows, facet_by)
      rows
    })
    raw <- dplyr::bind_rows(raw_parts)
    keys <- unique(c("ADM2_ID", "STORM_ID", "metric", facet_by))
    membership <- dplyr::distinct(raw[, keys, drop = FALSE])
    parts <- lapply(seq_len(nrow(metrics)), function(i) {
      plot_aggregate_metric(
        raw_parts[[i]], metrics[i, ],
        type = type, count_unit = count_unit,
        facet_by = facet_by, binary = binary
      )
    })
    out <- dplyr::bind_rows(parts)
  }
  if (!nrow(out)) {
    cli::cli_abort("No plotting data remain after subsetting
                    and matching storm windows.")
  }
  if (length(wind_categories)) {
    out$wind_category <- NA_character_
    for (metric in names(wind_categories)) {
      selected <- as.character(out$metric) == metric
      out$wind_category[selected] <- as.character(plot_categorize_wind_exposure(
        out$value[selected], wind_categories[[metric]]$key
      ))
    }
  }
  validate_plot_facet(out, facet_by)
  if (type %in% c("exposure", "count")) {
    rlang::check_installed("sf", reason = "to prepare ADM2 maps")
    geometry <- data$geometry
    if (!inherits(geometry, "sf")) {
      cli::cli_abort("The geometry table must be an sf object.")
    }
    if (is.na(sf::st_crs(geometry))) {
      cli::cli_abort("The geometry table must have a known CRS.")
    }
    if (anyDuplicated(geometry$ADM2_ID)) {
      cli::cli_abort("Geometry must have one feature per ADM2_ID.")
    }
    missing <- setdiff(out$ADM2_ID, geometry$ADM2_ID)
    if (length(missing)) {
      cli::cli_warn("Plotted ADM2 units lack geometry and were omitted:
                     {.val {missing}}.")
    }
    geometry <- geometry[geometry$ADM2_ID %in% out$ADM2_ID,
      "ADM2_ID",
      drop = FALSE
    ]
    out <- dplyr::inner_join(geometry, out,
      by = "ADM2_ID", relationship = "one-to-many"
    )
    if (!nrow(out)) cli::cli_abort("No plotting data have matching geometry.")
    if (nrow(geometry) > 20000L) {
      cli::cli_warn("More than 20,000 polygons: rendering may be slow;
                     use geometry_resolution = 'simplified'
                     or a tighter filter.")
    }
  }
  if (type %in% c("exposure", "count")) {
    keys <- unique(c("ADM2_ID", "metric", facet_by))
    membership <- dplyr::semi_join(membership, sf::st_drop_geometry(out),
      by = keys
    )
    tracks <- plot_prepare_overlay(tracks, membership, facet_by)
    if (!is.null(tracks)) {
      tracks$ISO_TIME <- plot_parse_utc(tracks$ISO_TIME, "ISO_TIME")
    }
  }
  if ("metric" %in% names(out)) {
    out$metric <- factor(out$metric, levels = metrics$metric)
  }
  if (!is.null(tracks) && "metric" %in% names(tracks)) {
    tracks$metric <- factor(tracks$metric, levels = metrics$metric)
  }
  facets <- if (is.null(facet_by)) NULL else unique(out[[facet_by]])
  observed_range <- if (type == "track") {
    plot_track_date_range(out)
  } else {
    plot_observation_date_range(raw_parts)
  }
  hazard_settings <- plot_hazard_settings(if (type == "track") NULL else raw_parts)
  storm_info <- plot_storm_info(out, data$storm_metadata, membership)
  info <- list(
    type = type, metrics = metrics, metric = metrics$metric,
    registry = plot_metric_registry(data$codebook),
    aggregate = stats::setNames(metrics$aggregate, metrics$metric),
    threshold = stats::setNames(metrics$threshold, metrics$metric),
    binary = binary, count_unit = count_unit,
    wind_categories = wind_categories,
    facet_by = facet_by, facets = facets,
    track = track, tracks = tracks, plotted_pairs = membership,
    track_map_key = dots$track_map_key %||% saffir_simpson(),
    track_labels = dots$track_labels %||% TRUE,
    track_interval = dots$track_interval %||% 6L,
    track_legend_position = dots$track_legend_position,
    observed_range = observed_range,
    hazard_settings = hazard_settings,
    # Retain this field for callers which used the earlier prepared-data API.
    wind_radius_km = hazard_settings$wind$radius_km,
    storm_ids = storm_info$ids, storm_labels = storm_info$labels,
    filters = filters, flood_match = flood_match,
    query = attr(data, "query", exact = TRUE),
    manifest = attr(data, "manifest", exact = TRUE),
    retrieved_at = attr(data, "retrieved_at", exact = TRUE),
    gtropic_version = attr(data, "gtropic_version", exact = TRUE)
  )
  info$metric_labels <- plot_metric_panel_labels(metrics)
  info$caption <- plot_caption(info)
  attr(out, "plot_info") <- info
  out
}

#' Determine the date coverage of observations that reached map aggregation
#' @keywords internal
#' @noRd
plot_observation_date_range <- function(raw_parts) {
  dates <- unlist(lapply(raw_parts, function(rows) {
    observed <- !is.na(rows$.plot_row_id) & is.finite(rows$value)
    if (!any(observed)) {
      return(as.Date(character()))
    }
    if ("date" %in% names(rows)) {
      return(as.Date(rows$date[observed]))
    }
    if (all(c("FLOOD_START_DATE", "FLOOD_END_DATE", "FLOOD_WINDOW_START", "FLOOD_WINDOW_END") %in% names(rows))) {
      start <- pmax(
        plot_join_date(rows$FLOOD_START_DATE[observed], "FLOOD_START_DATE"),
        plot_join_date(rows$FLOOD_WINDOW_START[observed], "FLOOD_WINDOW_START")
      )
      end <- pmin(
        plot_join_date(rows$FLOOD_END_DATE[observed], "FLOOD_END_DATE"),
        plot_join_date(rows$FLOOD_WINDOW_END[observed], "FLOOD_WINDOW_END")
      )
      return(c(start[start <= end], end[start <= end]))
    }
    if ("LOCAL_DATE_STORM_CLOSEST" %in% names(rows)) {
      return(plot_join_date(rows$LOCAL_DATE_STORM_CLOSEST[observed], "LOCAL_DATE_STORM_CLOSEST"))
    }
    as.Date(character())
  }), use.names = FALSE)
  dates <- as.Date(dates, origin = "1970-01-01")
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(NULL)
  }
  range(dates)
}

#' Determine the displayed UTC calendar-date range for a track map
#' @keywords internal
#' @noRd
plot_track_date_range <- function(tracks) {
  dates <- as.Date(tracks$ISO_TIME, tz = "UTC")
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(NULL)
  }
  range(dates)
}

#' Derive plotted radii and temporal windows from the contributing rows
#'
#' These settings deliberately come from rows that contributed to the plotted
#' values, not from original retrieval filters. A supplied or subsequently
#' subsetted object can therefore describe a smaller effective window.
#' @keywords internal
#' @noRd
plot_hazard_settings <- function(raw_parts) {
  empty <- list(radius_km = NULL, days_before = NULL, days_after = NULL)
  settings <- stats::setNames(rep(list(empty), 3L), c("wind", "precip", "flood"))
  if (is.null(raw_parts)) return(settings)

  registry <- plot_metric_registry()
  for (rows in raw_parts) {
    if (!nrow(rows) || !"metric" %in% names(rows)) next
    table <- registry$table[match(as.character(rows$metric[1L]), registry$metric)]
    if (is.na(table) || !table %in% names(settings)) next
    observed <- !is.na(rows$.plot_row_id) & is.finite(rows$value)
    if (!any(observed)) next
    if ("STORM_DIST_KM" %in% names(rows)) {
      radius <- rows$STORM_DIST_KM[observed]
      radius <- radius[is.finite(radius)]
      if (length(radius)) {
        settings[[table]]$radius_km <- max(c(settings[[table]]$radius_km, radius))
      }
    }
    window_columns <- c(
      "LOCAL_DATE_STORM_CLOSEST", paste0(toupper(table), "_WINDOW_START"),
      paste0(toupper(table), "_WINDOW_END")
    )
    if (!all(window_columns %in% names(rows))) next
    closest <- plot_join_date(rows[[window_columns[1L]]][observed], window_columns[1L])
    start <- plot_join_date(rows[[window_columns[2L]]][observed], window_columns[2L])
    end <- plot_join_date(rows[[window_columns[3L]]][observed], window_columns[3L])
    before <- as.numeric(closest - start)
    after <- as.numeric(end - closest)
    if (any(is.finite(before))) {
      settings[[table]]$days_before <- max(c(settings[[table]]$days_before, before), na.rm = TRUE)
    }
    if (any(is.finite(after))) {
      settings[[table]]$days_after <- max(c(settings[[table]]$days_after, after), na.rm = TRUE)
    }
  }
  settings
}

#' Identify plotted storms and use published names where available
#' @keywords internal
#' @noRd
plot_storm_info <- function(data, storm_metadata = NULL, membership = NULL) {
  ids <- if (!is.null(membership) && "STORM_ID" %in% names(membership)) {
    unique(as.character(membership$STORM_ID))
  } else if ("STORM_ID" %in% names(data)) {
    unique(as.character(data$STORM_ID))
  } else {
    character()
  }
  ids <- ids[!is.na(ids) & nzchar(ids)]
  labels <- ids
  if (!is.null(storm_metadata) && all(c("STORM_ID", "STORM_NAME") %in% names(storm_metadata))) {
    names <- storm_metadata$STORM_NAME[match(ids, storm_metadata$STORM_ID)]
    present <- !is.na(names) & nzchar(trimws(names))
    labels[present] <- as.character(names[present])
  } else if ("STORM_NAME" %in% names(data)) {
    names <- data$STORM_NAME[match(ids, data$STORM_ID)]
    present <- !is.na(names) & nzchar(trimws(names))
    labels[present] <- as.character(names[present])
  }
  list(ids = ids, labels = labels)
}

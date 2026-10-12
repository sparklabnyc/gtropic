#' Validate a categorical wind map key for static track plots
#'
#' @keywords internal
#' @noRd
plot_validate_track_map_key <- function(track_map_key) {
  if (!is.data.frame(track_map_key)) {
    cli::cli_abort("{.arg track_map_key} must be a data frame.")
  }
  key <- tibble::as_tibble(track_map_key)
  colour_columns <- intersect(c("colour", "color"), names(key))
  required <- c("category", "threshold", "unit")
  missing <- setdiff(required, names(key))
  if (length(missing) || !length(colour_columns)) {
    cli::cli_abort(c(
      "{.arg track_map_key} must contain category, threshold, unit, and colour or color.",
      i = "Missing columns: {.field {c(missing, if (!length(colour_columns)) 'colour/color')}}."
    ))
  }
  if (length(colour_columns) == 2L && !identical(key$colour, key$color)) {
    cli::cli_abort("{.arg track_map_key} cannot contain conflicting colour and color columns.")
  }
  if (!nrow(key)) {
    cli::cli_abort("{.arg track_map_key} must contain at least one category.")
  }
  key$colour <- key[[colour_columns[1L]]]
  if (
    !is.character(key$category) || anyNA(key$category) ||
      any(!nzchar(key$category)) || anyDuplicated(key$category)
  ) {
    cli::cli_abort("{.arg track_map_key}$category must contain unique, nonempty strings.")
  }
  if (!is.numeric(key$threshold) || anyNA(key$threshold)) {
    cli::cli_abort("{.arg track_map_key}$threshold must contain numeric ceilings.")
  }
  if (
    !is.character(key$unit) || anyNA(key$unit) ||
      length(unique(key$unit)) != 1L || !key$unit[1L] %in% c("m/s", "knots", "mph", "km/h")
  ) {
    cli::cli_abort("{.arg track_map_key}$unit must be one constant: m/s, knots, mph, or km/h.")
  }
  if (!is.character(key$colour) || anyNA(key$colour) || any(!nzchar(key$colour))) {
    cli::cli_abort("{.arg track_map_key}$colour must contain nonempty colour values.")
  }
  invalid_colours <- vapply(key$colour, function(x) {
    inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
  }, logical(1))
  if (any(invalid_colours)) {
    cli::cli_abort("{.arg track_map_key}$colour contains invalid colour values.")
  }
  thresholds <- key$threshold
  if (
    !is.infinite(thresholds[length(thresholds)]) ||
      any(thresholds < 0) ||
      any(!is.finite(thresholds[-length(thresholds)])) ||
      any(diff(thresholds) <= 0)
  ) {
    cli::cli_abort("{.arg track_map_key}$threshold must be strictly increasing ceilings ending in Inf.")
  }
  key
}

#' Convert IBTrACS USA_WIND values from knots
#'
#' @keywords internal
#' @noRd
plot_convert_track_wind <- function(wind, unit) {
  if (!is.numeric(wind)) {
    cli::cli_abort("{.arg wind} must be numeric USA_WIND values in knots.")
  }
  if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
    !unit %in% c("m/s", "knots", "mph", "km/h")) {
    cli::cli_abort("{.arg unit} must be one of m/s, knots, mph, or km/h.")
  }
  multiplier <- c(
    "m/s" = 0.514444444444444, knots = 1, mph = 1.150779448,
    "km/h" = 1.852
  )[[unit]]
  wind * multiplier
}

#' Categorize USA_WIND values using map-key ceilings
#'
#' Values equal to a ceiling belong to that category. Missing values remain
#' missing so ggplot2 can use its scale's missing-value treatment.
#' @keywords internal
#' @noRd
plot_categorize_track_wind <- function(wind, track_map_key) {
  key <- plot_validate_track_map_key(track_map_key)
  converted <- plot_convert_track_wind(wind, key$unit[1L])
  category <- rep(NA_character_, length(converted))
  present <- !is.na(converted)
  if (any(present)) {
    category[present] <- key$category[vapply(converted[present], function(value) {
      which(value <= key$threshold)[1L]
    }, integer(1))]
  }
  factor(category, levels = key$category, ordered = TRUE)
}

#' Validate the interval used to sample static track points
#'
#' @keywords internal
#' @noRd
plot_validate_track_interval <- function(track_interval) {
  if (is.null(track_interval)) {
    return(NULL)
  }
  if (
    !is.numeric(track_interval) || length(track_interval) != 1L ||
      is.na(track_interval) || !track_interval %in% c(3, 6)
  ) {
    cli::cli_abort("{.arg track_interval} must be NULL, 3, or 6 hours.")
  }
  as.integer(track_interval)
}

#' Sample track observations on UTC 3- or 6-hour boundaries
#'
#' @keywords internal
#' @noRd
plot_sample_track_points <- function(tracks, track_interval = NULL) {
  track_interval <- plot_validate_track_interval(track_interval)
  if (is.null(track_interval) || !nrow(tracks)) {
    return(tracks)
  }
  if (!inherits(tracks$ISO_TIME, "POSIXt")) {
    cli::cli_abort("Track point sampling requires POSIXct {.field ISO_TIME} values.")
  }
  seconds <- as.numeric(tracks$ISO_TIME)
  sampled <- !is.na(seconds) & seconds %% (track_interval * 3600) == 0
  tracks[sampled, , drop = FALSE]
}

#' Select one readable label anchor for each multi-observation storm
#'
#' @keywords internal
#' @noRd
plot_track_label_data <- function(tracks, facet_by = NULL) {
  if (!nrow(tracks)) {
    return(tracks)
  }
  keys <- unique(c("STORM_ID", facet_by))
  valid <- is.finite(tracks$LON) & is.finite(tracks$LAT)
  tracks <- tracks[valid, , drop = FALSE]
  if (!nrow(tracks)) {
    tracks$.track_label <- character()
    return(tracks)
  }
  tracks <- dplyr::arrange(tracks, !!!rlang::syms(keys), .data$ISO_TIME)
  labels <- dplyr::group_by(tracks, !!!rlang::syms(keys))
  labels <- dplyr::filter(labels, dplyr::n() > 1L)
  labels <- dplyr::slice_head(labels, n = 1L)
  labels <- dplyr::ungroup(labels)
  labels$.track_label <- plot_track_display_name(labels)
  labels
}

#' Select readable storm names with stable identifier fallbacks
#' @keywords internal
#' @noRd
plot_track_display_name <- function(tracks) {
  out <- if ("STORM_NAME" %in% names(tracks)) {
    as.character(tracks$STORM_NAME)
  } else {
    rep(NA_character_, nrow(tracks))
  }
  fallback <- as.character(tracks$STORM_ID)
  missing <- is.na(out) | !nzchar(trimws(out))
  out[missing] <- fallback[missing]
  out
}

#' Sort storm observations and split paths at longitude and panel discontinuities
#'
#' Cartography may shift LON to a 0--360 frame while retaining original_LON.
#' Group IDs are unique across storms, including storms with only one point.
#' @keywords internal
#' @noRd
plot_split_tracks <- function(tracks, facet_by = NULL) {
  keys <- unique(c("STORM_ID", intersect(c("metric", facet_by), names(tracks))))
  tracks <- dplyr::arrange(
    tibble::as_tibble(tracks), !!!rlang::syms(keys), .data$ISO_TIME
  )
  if (!nrow(tracks)) {
    tracks$.plot_group <- integer()
    return(tracks)
  }
  longitude <- tracks[["original_LON"]] %||% tracks$LON
  panels <- dplyr::group_indices(dplyr::group_by(tracks, !!!rlang::syms(keys)))
  previous <- seq_len(nrow(tracks) - 1L)
  current <- previous + 1L
  # Missing coordinates must not allow a path to bridge unobserved positions.
  missing <- !is.finite(longitude) | !is.finite(tracks$LON) |
    !is.finite(tracks$LAT)
  breaks <- panels[current] != panels[previous] |
    abs(longitude[current] - longitude[previous]) > 180 |
    abs(tracks$LON[current] - tracks$LON[previous]) > 180 |
    missing[current] | missing[previous]
  tracks$.plot_group <- cumsum(c(TRUE, breaks))
  tracks
}

#' Add ordered track paths and observations to a map
#'
#' `track_map_key`, `track_interval`, and `track_labels` are internal static
#' builder arguments for coordinator integration. A wind map key switches wind
#' observations to categorical filled points; paths always retain all rows.
#' @keywords internal
#' @noRd
plot_add_tracks <- function(p, tracks, facet_by = NULL, colour_metric = NULL,
                            palette = NULL, direction = 1,
                            track_map_key = NULL, track_interval = NULL,
                            track_labels = FALSE) {
  rlang::check_installed("ggplot2", reason = "to plot storm tracks")
  if (is.null(tracks) || !nrow(tracks)) {
    return(p)
  }
  if (!is.logical(track_labels) || length(track_labels) != 1L || is.na(track_labels)) {
    cli::cli_abort("{.arg track_labels} must be TRUE or FALSE.")
  }
  track_interval <- plot_validate_track_interval(track_interval)
  info <- attr(p$data, "plot_info", exact = TRUE) %||%
    attr(tracks, "plot_info", exact = TRUE)
  registry <- info$registry %||% plot_metric_registry()
  tracks <- plot_split_tracks(tracks, facet_by)
  paths <- ggplot2::geom_path(
    data = tracks,
    mapping = ggplot2::aes(x = .data$LON, y = .data$LAT, group = .data$.plot_group),
    inherit.aes = FALSE, colour = "grey30", linewidth = 0.5, na.rm = TRUE
  )
  if (is.null(colour_metric)) {
    points <- ggplot2::geom_point(
      data = plot_sample_track_points(tracks, track_interval),
      mapping = ggplot2::aes(
        x = .data$LON, y = .data$LAT, group = .data$.plot_group,
        size = .data$USA_WIND
      ),
      inherit.aes = FALSE, colour = "grey30", na.rm = TRUE
    )
    p <- p + paths + points + ggplot2::scale_size_continuous(
      name = registry$legend[match("USA_WIND", registry$metric)],
      guide = ggplot2::guide_legend(position = "bottom", nrow = 1L, byrow = TRUE)
    )
  } else {
    colour_metric <- rlang::arg_match(colour_metric, c("USA_WIND", "USA_STATUS"))
    if (colour_metric == "USA_WIND" && !is.null(track_map_key)) {
      key <- plot_validate_track_map_key(track_map_key)
      points_data <- plot_sample_track_points(tracks, track_interval)
      points_data$.track_category <- plot_categorize_track_wind(
        points_data$USA_WIND, key
      )
      points <- ggplot2::geom_point(
        data = points_data,
        mapping = ggplot2::aes(x = .data$LON, y = .data$LAT, fill = .data$.track_category),
        inherit.aes = FALSE, shape = 21, size = 2.5, colour = "grey20", na.rm = TRUE
      )
      p <- p + paths + points + ggplot2::scale_fill_manual(
        values = stats::setNames(key$colour, key$category),
        breaks = key$category, drop = FALSE,
        name = paste0(
          registry$label[match("USA_WIND", registry$metric)],
          " (", key$unit[1L], ")"
        ),
        na.value = "grey90",
        guide = ggplot2::guide_legend(position = "bottom", nrow = 1L, byrow = TRUE)
      )
    } else {
      colour_mapping <- ggplot2::aes(colour = .data[[colour_metric]])
      path_mapping <- ggplot2::aes(
        x = .data$LON, y = .data$LAT,
        group = .data$.plot_group
      )
      point_mapping <- ggplot2::aes(
        x = .data$LON, y = .data$LAT,
        group = .data$.plot_group, size = .data$USA_WIND
      )
      path_mapping$colour <- point_mapping$colour <- colour_mapping$colour
      defaults <- plot_scale_defaults(colour_metric, palette, NULL, direction)
      scale <- if (colour_metric == "USA_STATUS") {
        ggplot2::scale_colour_viridis_d
      } else {
        ggplot2::scale_colour_viridis_c
      }
      p <- p + ggplot2::geom_path(
        data = tracks, mapping = path_mapping, inherit.aes = FALSE,
        linewidth = 0.5, na.rm = TRUE
      ) + ggplot2::geom_point(
        data = plot_sample_track_points(tracks, track_interval),
        mapping = point_mapping, inherit.aes = FALSE, na.rm = TRUE
      ) + scale(
        option = defaults$palette, direction = direction,
        name = registry$legend[match(colour_metric, registry$metric)],
        na.value = "grey90",
        guide = if (colour_metric == "USA_STATUS") {
          ggplot2::guide_legend(position = "bottom", nrow = 1L, byrow = TRUE)
        } else {
          ggplot2::guide_colourbar(position = "bottom")
        }
      ) + ggplot2::scale_size_continuous(
        name = registry$legend[match("USA_WIND", registry$metric)],
        guide = ggplot2::guide_legend(position = "bottom", nrow = 1L, byrow = TRUE)
      )
    }
  }
  if (track_labels && dplyr::n_distinct(tracks$STORM_ID) > 1L) {
    p <- p + ggplot2::geom_text(
      data = plot_track_label_data(tracks, facet_by),
      mapping = ggplot2::aes(x = .data$LON, y = .data$LAT, label = .data$.track_label),
      inherit.aes = FALSE, hjust = -0.1, vjust = -0.4, na.rm = TRUE
    )
  }
  p
}

#' Build a track map from prepared observations and shared cartography
#'
#' @keywords internal
#' @noRd
build_track_plot <- function(data, cartography,
                             title = NULL, subtitle = NULL, caption = NULL,
                             gridlines = TRUE, ...) {
  info <- attr(data, "plot_info", exact = TRUE)
  p <- plot_map_base(data, cartography)
  p <- plot_add_tracks(p, cartography$tracks,
    facet_by = info$facet_by, colour_metric = info$metric,
    palette = info$metrics$palette, direction = info$metrics$direction,
    track_map_key = info$track_map_key, track_interval = info$track_interval,
    track_labels = info$track_labels
  )
  plot_map_finish(p, data, cartography, title, subtitle, caption, gridlines)
}

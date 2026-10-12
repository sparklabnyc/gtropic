#' Publication defaults for G-TROPIC plots
#'
#' A minimal theme with subdued grid lines, bold titles, and readable captions.
#' Map builders remove axis titles and move multi-panel legends to the bottom.
#' @param base_size Base text size in points.
#' @return A ggplot2 theme that can be added to a plot.
#' @examples
#' \donttest{
#' theme_gtropic()
#' }
#' @export
theme_gtropic <- function(base_size = 12) {
  rlang::check_installed("ggplot2", reason = "to use the plotting theme")
  if (
    !is.numeric(base_size) || length(base_size) != 1L ||
      !is.finite(base_size) || base_size <= 0
  ) {
    cli::cli_abort("{.arg base_size} must be one positive finite number.")
  }
  ggplot2::theme_minimal(base_size = base_size) + ggplot2::theme(
    panel.grid.major = ggplot2::element_line(
      colour = "grey90", linewidth = 0.25
    ),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0),
    legend.position = "right",
    strip.text = ggplot2::element_text(face = "bold")
  )
}

#' Color-blind-safe fill scales for G-TROPIC plots
#'
#' Native ggplot2 viridis scales: inferno for wind, mako for precipitation,
#' and viridis for flooding when a metric is supplied. Missing values are grey.
#' @param ... Additional arguments passed to the ggplot2 viridis fill scale.
#' @param metric Optional published metric name, used for palette and title.
#' @param palette Viridis palette name; NULL chooses a hazard-specific palette.
#' @param direction Palette direction, either 1 or -1.
#' @param name Legend title; NULL uses the metric label and unit when available.
#' @param na.value Color for missing values.
#' @return A continuous or discrete ggplot2 fill scale.
#' @examples
#' \donttest{
#' scale_fill_gtropic_c(metric = "VMAX_SUST_MS")
#' scale_fill_gtropic_d(name = "Exposed")
#' }
#' @export
scale_fill_gtropic_c <- function(
  ..., metric = NULL, palette = NULL, direction = 1, name = NULL,
  na.value = "grey90" # nolint: object_name_linter.
) {
  rlang::check_installed("ggplot2", reason = "to use plotting scales")
  defaults <- plot_scale_defaults(metric, palette, name, direction)
  ggplot2::scale_fill_viridis_c(...,
    option = defaults$palette,
    direction = direction, name = defaults$name, na.value = na.value
  )
}

#' @rdname scale_fill_gtropic_c
#' @export
scale_fill_gtropic_d <- function(
  ..., metric = NULL, palette = NULL, direction = 1, name = NULL,
  na.value = "grey90" # nolint: object_name_linter.
) {
  rlang::check_installed("ggplot2", reason = "to use plotting scales")
  defaults <- plot_scale_defaults(metric, palette, name, direction)
  ggplot2::scale_fill_viridis_d(...,
    option = defaults$palette,
    direction = direction, name = defaults$name, na.value = na.value
  )
}

#' Resolve hazard-specific scale defaults
#' @keywords internal
#' @noRd
plot_scale_defaults <- function(metric, palette, name, direction) {
  if (
    !is.numeric(direction) || length(direction) != 1L ||
      is.na(direction) || !direction %in% c(-1, 1)
  ) {
    cli::cli_abort("{.arg direction} must be 1 or -1.")
  }
  registry <- plot_metric_registry()
  if (!is.null(metric)) {
    if (length(metric) != 1L || is.na(metric) || !metric %in% registry$metric) {
      cli::cli_abort("{.arg metric} must be one known plotting metric or NULL.")
    }
  }
  spec <- registry[match(metric, registry$metric), ]
  list(
    palette = palette %||% if (nrow(spec)) spec$palette else "viridis",
    name = name %||% if (nrow(spec)) spec$legend else ggplot2::waiver()
  )
}

#' Assemble the visible exposure definition and retrieval settings
#' @keywords internal
#' @noRd
plot_caption <- function(info, caption = NULL, crs = NULL) {
  if (!is.null(caption)) {
    return(caption)
  }
  metrics <- info$metrics
  pieces <- "Source: G-TROPIC"
  filters <- info$filters
  for (i in seq_len(nrow(metrics))) {
    spec <- metrics[i, ]
    hazard <- switch(spec$table,
      wind = "wind",
      precip = "precip",
      flood = "flood",
      NULL
    )
    if (is.null(hazard)) next
    if (spec$threshold_active) {
      pieces <- c(pieces, paste0(
        "exposed ", spec$metric, ": ", spec$operator, " ",
        plot_format_number(spec$threshold),
        if (nzchar(spec$unit)) paste0(" ", spec$unit)
      ))
    }
    if (hazard == "wind" && !is.null(info$wind_radius_km)) {
      pieces <- c(pieces, paste0(
        "wind radius: ", plot_format_number(round(info$wind_radius_km)), " km"
      ))
    }
    if (hazard %in% c("precip", "flood")) {
      radius <- filters[[paste0(hazard, "_radius_km")]]
      before <- filters[[paste0(hazard, "_days_before")]]
      after <- filters[[paste0(hazard, "_days_after")]]
      if (!is.null(radius)) pieces <- c(pieces, paste0(hazard, " radius: ", radius, " km"))
      if (!is.null(before) && !is.null(after)) {
        pieces <- c(pieces, paste0(hazard, " window: -", before, "/+", after, " days"))
      }
    }
    pieces <- c(pieces, paste0(hazard, " aggregate: ", spec$aggregate))
    if (hazard == "flood") pieces <- c(pieces, paste0("flood match: ", info$flood_match))
  }
  if (isTRUE(info$track)) {
    pieces <- c(pieces, paste0("track interval: every ", info$track_interval, " hours"))
  }
  if (!is.null(crs)) pieces <- c(pieces, paste0("CRS: ", plot_crs_label(crs)))
  paste(pieces, collapse = ". ")
}

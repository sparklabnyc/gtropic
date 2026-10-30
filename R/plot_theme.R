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
#' and viridis for flooding when a metric is supplied. Missing values are gray.
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
  pieces <- plot_provenance_caption(info)
  settings <- info$hazard_settings %||% list()
  for (i in seq_len(nrow(metrics))) {
    spec <- metrics[i, ]
    hazard <- switch(spec$table,
      wind = "wind",
      precip = "precip",
      flood = "flood",
      NULL
    )
    if (is.null(hazard)) next
    hazard_settings <- settings[[hazard]] %||% list()
    pieces <- c(pieces, plot_hazard_caption(hazard, hazard_settings, spec, info))
  }
  if (length(info$wind_categories)) {
    for (metric in names(info$wind_categories)) {
      spec <- metrics[match(metric, metrics$metric), ]
      pieces <- c(pieces, plot_wind_category_caption(
        spec, info$wind_categories[[metric]]
      ))
    }
  }
  if (isTRUE(info$track)) {
    pieces <- c(pieces, paste0(
      "Storm track interval: every ", info$track_interval, " hours"
    ))
  }
  if (!is.null(crs)) pieces <- c(pieces, paste0("CRS: ", plot_crs_label(crs)))
  paste(pieces, collapse = "\n")
}

#' Format one hazard's automatic caption line
#' @keywords internal
#' @noRd
plot_hazard_caption <- function(hazard, settings, spec, info) {
  label <- switch(hazard,
    wind = "Wind",
    precip = "Precip",
    flood = "Flood"
  )
  parameters <- character()
  radius <- settings$radius_km
  if (!is.null(radius) && length(radius) == 1L && is.finite(radius)) {
    parameters <- c(parameters, paste0(
      "radius=", plot_format_number(round(radius)), " km"
    ))
  }
  if (hazard %in% c("precip", "flood")) {
    before <- settings$days_before
    after <- settings$days_after
    if (!is.null(before) && !is.null(after) && length(before) == 1L &&
      length(after) == 1L && is.finite(before) && is.finite(after)) {
      parameters <- c(parameters, paste0("window=-", before, "/+", after, " days"))
    }
  }
  parameters <- c(parameters, paste0("aggregate=", spec$aggregate))
  if (hazard == "flood" && plot_caption_value(info$flood_match)) {
    parameters <- c(parameters, paste0("match=", info$flood_match))
  }
  paste0(label, " parameters: ", paste(parameters, collapse = ", "))
}

#' Describe a categorical exposure classification in an automatic caption
#' @keywords internal
#' @noRd
plot_wind_category_caption <- function(spec, category_config) {
  key_label <- if (identical(category_config$kind, "saffir_simpson")) {
    "Saffir\u2013Simpson-equivalent"
  } else {
    "user-defined"
  }
  paste0(
    "Categorical exposure: source metric=", spec$metric,
    ", aggregate=", spec$aggregate,
    ", key=", key_label,
    ", unit=", category_config$key$unit[1L]
  )
}

#' Format reproducibility metadata for an automatic plot caption
#' @keywords internal
#' @noRd
plot_provenance_caption <- function(info) {
  manifest <- info$manifest
  if (is.list(manifest)) {
    retrieved_at <- info$retrieved_at %||% manifest$retrieved_at
    package_version <- info$gtropic_version %||% manifest$gtropic_version
  } else {
    retrieved_at <- info$retrieved_at
    package_version <- info$gtropic_version
  }
  if (!plot_caption_value(package_version)) {
    package_version <- as.character(utils::packageVersion("gtropic"))
  }
  pieces <- plot_source_caption(package_version)
  if (inherits(retrieved_at, "POSIXt") && length(retrieved_at) == 1L &&
    !is.na(retrieved_at)) {
    zone <- attr(retrieved_at, "tzone") %||% ""
    pieces <- c(pieces, paste0(
      "Data retrieved on ", format(retrieved_at, "%Y-%m-%d %H:%M:%S %Z", tz = zone)
    ))
  }
  if (is.list(manifest)) {
    datasets <- plot_manifest_dataset_captions(manifest)
    if (length(datasets)) {
      pieces <- c(pieces, paste0("Datasets: ", paste(datasets, collapse = " / ")))
    }
  }
  pieces
}

#' Format dataset provenance from a manifest
#' @keywords internal
#' @noRd
plot_manifest_dataset_captions <- function(manifest) {
  datasets <- manifest_datasets(manifest)
  if (is.data.frame(datasets) && all(c("dataset_doi", "dataset_version") %in% names(datasets))) {
    return(unlist(Map(
      plot_dataset_caption,
      datasets$source_role, datasets$dataset_version, datasets$dataset_doi
    ), use.names = FALSE))
  }
  character(0)
}

#' Format the source line for an automatic plot caption
#' @keywords internal
#' @noRd
plot_source_caption <- function(package_version = NULL) {
  if (!plot_caption_value(package_version)) {
    package_version <- as.character(utils::packageVersion("gtropic"))
  }
  version <- sub("^v\\.?", "", as.character(package_version))
  paste0("Source: G-TROPIC R package v.", version)
}

#' Format one complete dataset provenance entry
#' @keywords internal
#' @noRd
plot_dataset_caption <- function(label, version, doi) {
  if (!plot_caption_value(version) || !plot_caption_value(doi)) return(NULL)
  version <- sub("^v\\.?", "", as.character(version))
  doi <- as.character(doi)
  if (!startsWith(doi, "doi:")) doi <- paste0("doi:", doi)
  paste0(label, " v.", version, " (", doi, ")")
}

#' Test whether one caption metadata field can be displayed
#' @keywords internal
#' @noRd
plot_caption_value <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(as.character(x))
}

#' Construct readable facet labels for selected metrics
#' @keywords internal
#' @noRd
plot_metric_panel_labels <- function(metrics) {
  labels <- vapply(seq_len(nrow(metrics)), function(i) {
    plot_metric_display_label(metrics[i, ])
  }, character(1))
  stats::setNames(labels, metrics$metric)
}

#' Describe a metric using its semantic aggregation and active threshold
#' @keywords internal
#' @noRd
plot_metric_display_label <- function(spec) {
  aggregate <- as.character(spec$aggregate)
  label <- plot_metric_aggregate_label(spec)
  if (!isTRUE(spec$threshold_active)) return(label)
  paste0(label, " ", spec$operator, " ", plot_format_number(spec$threshold),
    if (nzchar(spec$unit)) paste0(" ", spec$unit)
  )
}

#' Apply a reader-facing aggregation label to a published metric label
#' @keywords internal
#' @noRd
plot_metric_aggregate_label <- function(spec) {
  aggregate <- as.character(spec$aggregate)
  prefixes <- c(max = "Maximum", min = "Minimum", mean = "Mean", sum = "Cumulative")
  prefix <- if (aggregate %in% names(prefixes)) prefixes[[aggregate]] else NULL
  label <- as.character(spec$label)
  if (identical(aggregate, "n")) {
    return(label)
  }
  if (is.null(prefix)) return(label)
  # Avoid duplicate wording when a codebook label already starts with the
  # same aggregate (e.g., "Maximum sustained wind").
  label <- sub(paste0("^", prefix, "\\s+"), "", label, ignore.case = TRUE)
  if (identical(as.character(spec$table), "precip") && aggregate %in% c("max", "min", "mean")) {
    return(paste(prefix, "(daily)", plot_lower_initial(label)))
  }
  paste(prefix, plot_lower_initial(label))
}

#' Convert only the first letter of a metric label to sentence case
#' @keywords internal
#' @noRd
plot_lower_initial <- function(x) {
  paste0(tolower(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

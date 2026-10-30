#' Build an exposure or count map from prepared values and cartography
#'
#' Each metric owns its fill scale, shared across that metric's storm facets.
#' This builder only assembles layers; published values are prepared upstream.
#' @keywords internal
#' @noRd
build_exposure_plot <- function(data, cartography,
                                title = NULL, subtitle = NULL, caption = NULL,
                                gridlines = TRUE, ...) {
  rlang::check_installed(c("ggplot2", "sf"), reason = "to draw exposure maps")
  info <- attr(data, "plot_info", exact = TRUE)
  specs <- info$metrics
  if (nrow(specs) > 1L) {
    rlang::check_installed("ggnewscale",
      reason = "for independent metric fills"
    )
  }
  mapped <- cartography$data %||% data
  p <- plot_map_base(mapped, cartography)
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    category_config <- info$wind_categories[[as.character(spec$metric)]]
    # Distinct titles prevent ggplot2 merging equal-break binary/count guides
    # whose hazard palettes differ.
    legend <- category_config$label %||% if (sum(specs$legend == spec$legend) > 1L) {
      paste(spec$label, spec$legend, sep = ": ")
    } else {
      spec$legend
    }
    part <- mapped[mapped$metric == spec$metric, , drop = FALSE]
    if (i > 1L) p <- p + ggnewscale::new_scale_fill()
    if (!is.null(category_config)) {
      key <- category_config$key
      p <- p + ggplot2::geom_sf(
        data = part, mapping = ggplot2::aes(fill = .data$wind_category),
        inherit.aes = FALSE, ...
      ) + ggplot2::scale_fill_manual(
        values = stats::setNames(key$colour, key$category),
        limits = key$category, breaks = key$category, drop = FALSE,
        name = legend, na.value = "grey90", guide = "none"
      )
    } else if (info$binary) {
      p <- p + ggplot2::geom_sf(
        data = part, mapping = ggplot2::aes(fill = .data$exposed),
        inherit.aes = FALSE, ...
      ) + scale_fill_gtropic_d(
        metric = spec$metric, palette = spec$palette, direction = spec$direction,
        name = legend, limits = c(FALSE, TRUE), drop = FALSE,
        guide = "none"
      )
    } else if (info$type == "count") {
      count_categories <- plot_count_categories(part$value)
      part$count_category <- plot_count_category(part$value, count_categories)
      p <- p + ggplot2::geom_sf(
        data = part, mapping = ggplot2::aes(fill = .data$count_category),
        inherit.aes = FALSE, ...
      ) + scale_fill_gtropic_d(
        metric = spec$metric, palette = spec$palette, direction = spec$direction,
        name = legend, limits = count_categories, breaks = count_categories,
        drop = FALSE, guide = "none"
      )
    } else {
      observed <- part$value[is.finite(part$value)]
      limits <- if (length(observed)) range(observed) else NULL
      p <- p + ggplot2::geom_sf(
        data = part, mapping = ggplot2::aes(fill = .data$value),
        inherit.aes = FALSE, ...
      ) + scale_fill_gtropic_c(
        metric = spec$metric, palette = spec$palette, direction = spec$direction,
        name = legend, limits = limits,
        guide = "none"
      )
    }
    p <- plot_add_metric_key(
      p, part, spec, info, cartography$bbox, info$track_legend_position,
      category_config,
      if (info$type == "count" && !info$binary) count_categories else NULL
    )
  }
  if (isTRUE(info$track) && !is.null(cartography$tracks)) {
    rlang::check_installed("ggnewscale", reason = "for track intensity points")
    p <- p + ggnewscale::new_scale_fill()
    p <- plot_add_tracks(p, cartography$tracks,
      facet_by = info$facet_by, colour_metric = "USA_WIND",
      track_map_key = info$track_map_key,
      track_interval = info$track_interval,
      track_labels = info$track_labels
    )
  }
  plot_map_finish(p, data, cartography, title, subtitle, caption, gridlines)
}

#' Draw a horizontal scale key below every panel belonging to one metric
#'
#' ggplot2 guides are global to a faceted plot, so separate fill scales cannot
#' be assigned to their own facets. These tiles use the metric's existing fill
#' scale and facet value, making the key local to its panel instead. Cartography
#' reserves a small band below the geographic extent for these keys.
#' @keywords internal
#' @noRd
plot_add_metric_key <- function(p, data, spec, info, bbox, position = NULL,
                                 category_config = NULL,
                                 category_values = NULL) {
  categorical <- !is.null(category_config) || !is.null(category_values)
  values <- if (categorical) {
    category_values %||% category_config$key$category
  } else if (info$binary) {
    c(FALSE, TRUE)
  } else {
    values <- data$value[is.finite(data$value)]
    if (!length(values)) return(p)
    seq(min(values), max(values), length.out = 32L)
  }
  if (!length(values)) return(p)
  layout <- plot_metric_key_layout(position, bbox, length(values))
  key <- data.frame(
    x = layout$x, y = layout$y,
    value = values
  )
  key$metric <- factor(spec$metric, levels = levels(data$metric))
  if (categorical) {
    horizontal <- length(unique(layout$x)) > 1L
    labels <- key
    labels$x <- if (horizontal) layout$x else rep(layout$label_x[1L], nrow(key))
    labels$y <- if (horizontal) rep(layout$label_y[1L], nrow(key)) else layout$y
    labels$hjust <- if (horizontal) 0.5 else rep(layout$label_hjust[1L], nrow(key))
    labels$label <- as.character(values)
  } else {
    labels <- key[c(1L, nrow(key)), , drop = FALSE]
    labels$x <- layout$label_x
    labels$y <- layout$label_y
    labels$hjust <- layout$label_hjust
    labels$label <- if (info$binary) {
    c("Not exposed", "Exposed")
    } else {
    plot_format_key_number(labels$value)
    }
  }
  p + ggplot2::geom_tile(
    data = key, mapping = ggplot2::aes(x = .data$x, y = .data$y,
      fill = .data$value),
    inherit.aes = FALSE, width = layout$tile_width, height = layout$tile_height
  ) + ggplot2::geom_text(
    data = labels,
    mapping = ggplot2::aes(
      x = .data$x, y = .data$y, label = .data$label, hjust = .data$hjust
    ),
    inherit.aes = FALSE, size = 3
  )
}

#' Format finite count values as ordered discrete fill categories
#'
#' Count maps use a categorical scale so adjacent count values receive distinct
#' colors rather than positions along a continuous exposure gradient.
#' @keywords internal
#' @noRd
plot_count_categories <- function(value) {
  observed <- sort(unique(value[is.finite(value)]))
  if (!length(observed)) return(character())
  plot_format_number(observed)
}

#' Classify numeric counts for a discrete map fill scale
#' @keywords internal
#' @noRd
plot_count_category <- function(value, categories = plot_count_categories(value)) {
  category <- rep(NA_character_, length(value))
  observed <- is.finite(value)
  category[observed] <- plot_format_number(value[observed])
  factor(category, levels = categories, ordered = TRUE)
}

#' Locate a metric scale strip relative to one panel's geographic extent
#' @keywords internal
#' @noRd
plot_metric_key_layout <- function(position, bbox, n) {
  x_span <- bbox[2] - bbox[1]
  y_span <- bbox[4] - bbox[3]
  horizontal <- function(y, label_y) {
    x <- seq(bbox[1] + x_span * 0.34, bbox[1] + x_span * 0.72, length.out = n)
    list(
      x = x, y = rep(y, n),
      label_x = x[c(1L, n)], label_y = rep(label_y, 2L),
      label_hjust = c(0, 1),
      tile_width = x_span * 0.39 / n, tile_height = y_span * 0.025
    )
  }
  vertical <- function(x, y_start, y_end, label_x, hjust) {
    y <- seq(y_start, y_end, length.out = n)
    list(
      x = rep(x, n), y = y,
      label_x = rep(label_x, 2L), label_y = y[c(1L, n)],
      label_hjust = rep(hjust, 2L),
      tile_width = x_span * 0.018, tile_height = (y_end - y_start) * 1.04 / n
    )
  }
  if (is.null(position)) {
    return(horizontal(
      bbox[3] - y_span * 0.065,
      bbox[3] - y_span * 0.105
    ))
  }
  switch(position,
    top = horizontal(bbox[4] - y_span * 0.07, bbox[4] - y_span * 0.115),
    bottom = horizontal(bbox[3] + y_span * 0.07, bbox[3] + y_span * 0.025),
    left = vertical(
      bbox[1] + x_span * 0.07, bbox[3] + y_span * 0.35,
      bbox[3] + y_span * 0.65, bbox[1] + x_span * 0.10, 0
    ),
    right = vertical(
      bbox[2] - x_span * 0.07, bbox[3] + y_span * 0.35,
      bbox[3] + y_span * 0.65, bbox[2] - x_span * 0.10, 1
    ),
    `top left` = vertical(
      bbox[1] + x_span * 0.07, bbox[3] + y_span * 0.63,
      bbox[3] + y_span * 0.93, bbox[1] + x_span * 0.10, 0
    ),
    `top right` = vertical(
      bbox[2] - x_span * 0.07, bbox[3] + y_span * 0.63,
      bbox[3] + y_span * 0.93, bbox[2] - x_span * 0.10, 1
    ),
    `bottom left` = vertical(
      bbox[1] + x_span * 0.07, bbox[3] + y_span * 0.07,
      bbox[3] + y_span * 0.37, bbox[1] + x_span * 0.10, 0
    ),
    `bottom right` = vertical(
      bbox[2] - x_span * 0.07, bbox[3] + y_span * 0.07,
      bbox[3] + y_span * 0.37, bbox[2] - x_span * 0.10, 1
    )
  )
}

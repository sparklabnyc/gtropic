#' Build a count map using the shared choropleth builder
#'
#' Preparation supplies distinct storm/day counts and count-specific legend
#' labels in `plot_info$metrics`; no counting takes place in the builder.
#' @keywords internal
#' @noRd
build_count_plot <- function(data, cartography,
                             title = NULL, subtitle = NULL, caption = NULL,
                             gridlines = TRUE, ...) {
  rlang::check_installed("ggplot2", reason = "to build count plots")
  build_exposure_plot(
    data = data, cartography = cartography, title = title, subtitle = subtitle,
    caption = caption, gridlines = gridlines, ...
  )
}

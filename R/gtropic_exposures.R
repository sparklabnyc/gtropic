#' List G-TROPIC exposure metrics
#'
#' Summarises the published wind, precipitation, and flooding exposure metrics
#' available in G-TROPIC. The returned aggregation is the default used when a
#' metric is summarised across its published records; it does not recompute a
#' published exposure value.
#'
#' `plot_supported` identifies metrics currently supported by [gtropic_plot()].
#' `FLOODED_POP_CENTROID` is included because it documents a published flood
#' exposure, although it is not currently available for plotting.
#'
#' @return A named list with `wind`, `precip`, and `flood` components. Each
#'   component is a tibble with columns `metric`, `label`, `unit`, `aggregate`,
#'   and `plot_supported`.
#'
#' @examples
#' exposures <- gtropic_exposures()
#' exposures$flood
#'
#' @export
gtropic_exposures <- function() {
  registry <- plot_metric_registry()
  registry <- registry[
    registry$table %in% c("wind", "precip", "flood") &
      registry$metric != "STORM_DIST_KM",
    c("table", "metric", "label", "unit", "aggregate")
  ]
  registry$plot_supported <- TRUE

  out <- lapply(c("wind", "precip", "flood"), function(table_name) {
    tibble::as_tibble(registry[
      registry$table == table_name,
      c("metric", "label", "unit", "aggregate", "plot_supported")
    ])
  })
  names(out) <- c("wind", "precip", "flood")

  out$flood <- tibble::add_row(
    out$flood,
    metric = "FLOODED_POP_CENTROID",
    label = "Flooded population centroid",
    unit = "logical",
    aggregate = "any",
    plot_supported = FALSE
  )

  out
}

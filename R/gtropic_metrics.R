#' List G-TROPIC exposure metrics
#'
#' Summarizes the published wind, precipitation, and flooding exposure metrics
#' available in G-TROPIC. The returned aggregation is the default used when a
#' metric is summarized across its published records; it does not recompute a
#' published exposure value.
#'
#' `plot_supported` identifies metrics currently supported by [gtropic_plot()].
#' `POP_CENTROID_IN_FLOOD_REPORT` is included because it documents a published
#' flood exposure, although it is not currently available for plotting.
#'
#' The flood metrics describe places named in news reports about flooding, not
#' mapped flood extent, and gtropic returns them under names that differ from
#' the published ones; see [gtropic_known_limitations].
#'
#' @return A named list with `wind`, `precip`, and `flood` components. Each
#'   component is a tibble with columns `metric`, `label`, `unit`, `aggregate`,
#'   and `plot_supported`.
#'
#' @examples
#' metrics <- gtropic_metrics()
#' metrics$flood
#'
#' @family discovery functions
#' @export
gtropic_metrics <- function() {
  registry <- plot_metric_registry()
  registry <- registry[
    registry$table %in%
      c("wind", "precip", "flood") &
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
    metric = "POP_CENTROID_IN_FLOOD_REPORT",
    label = "Population centroid in flood report",
    unit = "logical",
    aggregate = "any",
    plot_supported = FALSE
  )

  out
}

#' Example G-TROPIC result
#'
#' Returns a small synthetic [gtropic_data()] result for examples, tests and
#' trying out the plotting and accessor functions without a network
#' connection. All values are invented: six square administrative units in two
#' made-up places ("Example Coast" and "Example Pacific", one straddling the
#' antimeridian), three storms in August 2020, and a manifest that points at
#' the non-existent server `example.invalid` and the placeholder DOI
#' `doi:10.0000/EXAMPLE`. Do not use it for analysis.
#'
#' @return A `gtropic_data` object with the tables `wind`, `precip`, `flood`,
#'   `zero_pairs`, `tracks`, `storm_metadata`, `adm2`, `geometry`, `codebook`
#'   and `links`. `geometry` is an `sf` table when the \pkg{sf} package is
#'   installed and is left out otherwise.
#'
#' @examples
#' x <- gtropic_example()
#' x
#' names(x)
#' x$wind
#' @export
gtropic_example <- function() {
  x <- readRDS(system.file(
    "extdata", "gtropic-example.rds",
    package = "gtropic", mustWork = TRUE
  ))
  wkt <- x$geometry
  if (requireNamespace("sf", quietly = TRUE)) {
    geometry <- sf::st_sf(
      ADM2_ID = wkt$ADM2_ID,
      geometry = sf::st_as_sfc(wkt$geometry_wkt, crs = 4326)
    )
    attr(geometry, "geometry_resolution") <- "simplified"
    x$geometry <- geometry
  } else {
    x$geometry <- NULL
  }
  attr(x, "gtropic_version") <- as.character(utils::packageVersion("gtropic"))
  x
}

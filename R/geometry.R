#' Repair invalid simple-feature geometries when requested
#'
#' Validity is checked first so geometry already repaired during retrieval is
#' not processed again when it reaches the plotting boundary.
#'
#' @param geometry An `sf` object.
#' @param make_valid Whether to repair invalid features.
#'
#' @return `geometry`, with invalid features repaired when requested.
#' @keywords internal
#' @noRd
repair_invalid_geometry <- function(geometry, make_valid = TRUE) {
  if (!make_valid || nrow(geometry) == 0L) {
    return(geometry)
  }

  valid <- sf::st_is_valid(geometry)
  invalid <- is.na(valid) | !valid
  if (any(invalid)) {
    shapes <- sf::st_geometry(geometry)
    shapes[invalid] <- sf::st_make_valid(shapes[invalid])
    sf::st_geometry(geometry) <- shapes
  }
  geometry
}


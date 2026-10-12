#' Saffir-Simpson tropical-cyclone wind categories
#'
#' Provides the category thresholds and conventional display colours for the
#' Saffir-Simpson Hurricane Wind Scale. Thresholds are sustained wind speeds in
#' metres per second. Each threshold is the upper bound of its category, except
#' for `"Cat5"`, which has no upper bound.
#'
#' @return A tibble with one row per category and columns `category`,
#'   `threshold`, `unit`, and `color`. `threshold` is numeric and `"Cat5"` has
#'   an infinite threshold.
#'
#' @examples
#' saffir_simpson()
#'
#' @export
saffir_simpson <- function() {
  tibble::tibble(
    category = c("TD", "TS", "Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"),
    threshold = c(17, 32, 42, 49, 58, 70, Inf),
    unit = rep("m/s", 7L),
    color = c(
      "#6EC1EA", "#5FF", "#FFFFD9", "#FBDA8C", "#F89F59",
      "#F6738A", "#A188FC"
    )
  )
}

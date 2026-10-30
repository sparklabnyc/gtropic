#' Saffir-Simpson tropical-cyclone wind categories
#'
#' Provides the category thresholds and conventional display colors for the
#' Saffir-Simpson Hurricane Wind Scale. Thresholds are sustained wind speeds in
#' meters per second. Each threshold is the upper bound of its category, except
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

#' Validate a categorical key whose thresholds are inclusive upper bounds
#'
#' This common schema is used for both track intensity keys and categorical
#' exposure fills. It deliberately keeps units explicit because category
#' thresholds are not converted during ADM2 exposure plotting.
#' @keywords internal
#' @noRd
plot_validate_wind_category_key <- function(key, argument = "wind_categories") {
  if (!is.data.frame(key)) {
    cli::cli_abort("{.arg {argument}} must be a data frame.")
  }
  key <- tibble::as_tibble(key)
  colour_columns <- intersect(c("colour", "color"), names(key))
  required <- c("category", "threshold", "unit")
  missing <- setdiff(required, names(key))
  if (length(missing) || !length(colour_columns)) {
    cli::cli_abort(c(
      "{.arg {argument}} must contain category, threshold, unit, and colour or color.",
      i = "Missing columns: {.field {c(missing, if (!length(colour_columns)) 'colour/color')}}."
    ))
  }
  if (length(colour_columns) == 2L && !identical(key$colour, key$color)) {
    cli::cli_abort("{.arg {argument}} cannot contain conflicting colour and color columns.")
  }
  if (!nrow(key)) {
    cli::cli_abort("{.arg {argument}} must contain at least one category.")
  }
  key$colour <- key[[colour_columns[1L]]]
  if (
    !is.character(key$category) || anyNA(key$category) ||
      any(!nzchar(key$category)) || anyDuplicated(key$category)
  ) {
    cli::cli_abort("{.arg {argument}}$category must contain unique, nonempty strings.")
  }
  if (!is.numeric(key$threshold) || anyNA(key$threshold)) {
    cli::cli_abort("{.arg {argument}}$threshold must contain numeric ceilings.")
  }
  if (
    !is.character(key$unit) || anyNA(key$unit) ||
      length(unique(key$unit)) != 1L || !nzchar(key$unit[1L])
  ) {
    cli::cli_abort("{.arg {argument}}$unit must contain one constant, nonempty unit.")
  }
  if (!is.character(key$colour) || anyNA(key$colour) || any(!nzchar(key$colour))) {
    cli::cli_abort("{.arg {argument}}$colour must contain nonempty colour values.")
  }
  invalid_colours <- vapply(key$colour, function(x) {
    inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
  }, logical(1))
  if (any(invalid_colours)) {
    cli::cli_abort("{.arg {argument}}$colour contains invalid colour values.")
  }
  thresholds <- key$threshold
  if (
    !is.infinite(thresholds[length(thresholds)]) ||
      any(!is.finite(thresholds[-length(thresholds)])) ||
      any(diff(thresholds) <= 0)
  ) {
    cli::cli_abort("{.arg {argument}}$threshold must be strictly increasing ceilings ending in Inf.")
  }
  key
}

#' Identify the unmodified built-in Saffir-Simpson category key
#' @keywords internal
#' @noRd
plot_is_saffir_simpson_key <- function(key) {
  standard <- saffir_simpson()
  identical(as.character(key$category), standard$category) &&
    identical(as.numeric(key$threshold), standard$threshold) &&
    identical(as.character(key$unit), standard$unit) &&
    identical(as.character(key$colour), standard$color)
}

#' Normalize conventional meters-per-second unit spellings for comparison
#' @keywords internal
#' @noRd
plot_normalize_mps_unit <- function(unit) {
  unit <- tolower(trimws(enc2utf8(unit)))
  unit <- gsub("\u2212", "-", unit, fixed = TRUE)
  unit <- gsub("\u207b", "-", unit, fixed = TRUE)
  unit <- gsub("\u00b9", "1", unit, fixed = TRUE)
  unit <- gsub("met(er|re)s?", "m", unit)
  unit <- gsub("sec(ond)?s?", "s", unit)
  unit <- gsub("per", "/", unit)
  unit <- gsub("[[:space:]{}*.]", "", unit)
  if (unit %in% c("m/s", "ms-1", "ms^-1")) "m/s" else unit
}

#' Compare category-key and source units without converting values
#' @keywords internal
#' @noRd
plot_units_are_compatible <- function(key_unit, source_unit) {
  if (identical(key_unit, source_unit)) return(TRUE)
  identical(plot_normalize_mps_unit(key_unit), "m/s") &&
    identical(plot_normalize_mps_unit(source_unit), "m/s")
}

#' Save a static plot
#'
#' Save a ggplot with [ggplot2::ggsave()]. This helper writes files;
#' [gtropic_plot()] itself only returns plot objects.
#'
#' @param plot A ggplot, such as one returned by [gtropic_plot()].
#' @param filename One nonempty output filename, optionally including a path.
#'   For static plots the extension selects the device unless `device` is
#'   supplied in `...`.
#' @param ... Uniquely named arguments. For static plots, passed unchanged to
#'   [ggplot2::ggsave()], including `device`, `path`, `bg`, and `scale`.
#' @param dpi Positive finite resolution in dots per inch. Defaults to 300.
#' @param width,height Positive finite output dimensions in `units`, or NULL
#'   to infer them. Explicit dimensions are preserved. See Details for defaults.
#' @param units Dimension units: `"in"`, `"cm"`, `"mm"`, or `"px"`. Pixel sizes
#'   are converted using `dpi`.
#'
#' @details
#' ## Figure dimensions
#' Map plots carry a geographic `gtropic_bbox` attribute in the order
#' `c(xmin, xmax, ymin, ymax)` (named bounds may be in any order). The estimated
#' width-to-height ratio is longitude span times the cosine of mean latitude,
#' divided by latitude span. This is a geographic approximation, not a
#' measurement of the final projected panels, legends, or facets.
#'
#' Without usable bounds, the ratio is 7:5. With neither dimension supplied,
#' width defaults to 7 inches and height follows the ratio. With one dimension
#' supplied, the other follows the ratio. Inferred dimensions are bounded to
#' 3--10 inches to keep narrow, polar, or very wide extents usable. Supplying
#' both dimensions overrides all automatic sizing. Defaults are converted to
#' the requested units.
#'
#' @return Invisibly, the output filename, combined with `path` when supplied.
#'   The path is not normalized. Saver errors are propagated.
#' @seealso [gtropic_plot()], [ggplot2::ggsave()]
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point()
#'   output <- gtropic_save(p, tempfile(fileext = ".pdf"), width = 7, height = 5)
#'   unlink(output)
#' }
#' }
#' @export
gtropic_save <- function(plot, filename, ..., dpi = 300,
                         width = NULL, height = NULL, units = "in") {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot.")
  }
  plot_save_string(filename, "filename")
  for (nm in c("dpi", "width", "height")) {
    value <- get(nm)
    if (nm != "dpi" && is.null(value)) next
    if (
      !is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value <= 0
    ) {
      cli::cli_abort("{.arg {nm}} must be one positive finite number.")
    }
  }
  units <- rlang::arg_match(units, c("in", "cm", "mm", "px"))
  dots <- list(...)
  if (length(dots)) {
    nms <- names(dots)
    if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
      cli::cli_abort("All arguments in {.arg ...} must be named.")
    }
    if (anyDuplicated(nms)) {
      cli::cli_abort("Arguments in {.arg ...} must be uniquely named.")
    }
  }
  path <- dots[["path"]]
  if (!is.null(path)) plot_save_string(path, "path")
  output <- if (is.null(path)) filename else file.path(path, filename)
  size <- plot_save_dimensions(plot, width, height, units, dpi)

  rlang::check_installed("ggplot2", reason = "to save static plots")
  do.call(ggplot2::ggsave, c(list(
    filename = filename, plot = plot, dpi = dpi,
    width = size$width, height = size$height, units = units
  ), dots))
  invisible(output)
}

#' Validate output filenames and optional directories
#' @keywords internal
#' @noRd
plot_save_string <- function(value, argument) {
  if (
    !is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))
  ) {
    cli::cli_abort("{.arg {argument}} must be one nonempty string.")
  }
  invisible(value)
}

#' Infer physical dimensions while preserving explicitly supplied sizes
#' @keywords internal
#' @noRd
plot_save_dimensions <- function(plot, width, height, units, dpi) {
  factor <- switch(units,
    "in" = 1,
    cm = 2.54,
    mm = 25.4,
    px = dpi
  )
  aspect <- plot_save_aspect(attr(plot, "gtropic_bbox", exact = TRUE))
  bound <- function(x) max(3, min(10, x))
  if (is.null(width) && is.null(height)) {
    width <- 7 * factor
    height <- bound(7 / aspect) * factor
  } else if (is.null(width)) {
    width <- bound(height / factor * aspect) * factor
  } else if (is.null(height)) {
    height <- bound(width / factor / aspect) * factor
  }
  list(width = width, height = height)
}

#' Estimate a geographic aspect without requiring spatial dependencies
#' @keywords internal
#' @noRd
plot_save_aspect <- function(bbox) {
  fallback <- 7 / 5
  if (!is.numeric(bbox) || length(bbox) != 4L || any(!is.finite(bbox))) {
    return(fallback)
  }
  order <- c("xmin", "xmax", "ymin", "ymax")
  if (!is.null(names(bbox))) {
    if (!setequal(names(bbox), order)) {
      return(fallback)
    }
    bbox <- bbox[order]
  }
  span <- unname(c(bbox[2] - bbox[1], bbox[4] - bbox[3]))
  if (any(span <= 0) || span[1] > 360 || any(abs(bbox[3:4]) > 90)) {
    return(fallback)
  }
  aspect <- span[1] * cos(mean(bbox[3:4]) * pi / 180) / span[2]
  if (!is.finite(aspect) || aspect <= 0) fallback else aspect
}

#' Null-coalescing operator
#'
#' @param x,y Values; `y` is returned when `x` is `NULL`.
#' @return `x` unless it is `NULL`, otherwise `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Drop `NULL` elements from a list
#'
#' @param x A list.
#' @return `x` without its `NULL` elements.
#' @keywords internal
#' @noRd
compact <- function(x) x[!vapply(x, is.null, logical(1))]

#' Normalise a character filter argument
#'
#' Filter arguments are user input and arrive in every shape imaginable. We
#' accept `NULL` (no filter), reject empty vectors and `NA` (almost always a
#' mistake that would otherwise match nothing), and coerce factors to character.
#'
#' @param x Value supplied by the user.
#' @param arg Argument name, for error messages.
#' @param call Calling environment.
#'
#' @return `NULL` or a character vector.
#' @keywords internal
#' @noRd
as_filter_chr <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) x <- as.character(x)
  if (length(x) == 0L) {
    cli::cli_abort(
      c("{.arg {arg}} was supplied but is empty.",
        i = "Use {.code NULL} to leave a filter unset."
      ),
      call = call
    )
  }
  if (anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain {.val NA}.", call = call)
  }
  unique(trimws(x))
}

#' Truncate a vector for display
#'
#' Long vectors of identifiers make messages unreadable. Show the first `n` and
#' report how many were withheld; the full vector is always available on the
#' returned object.
#'
#' @param x Vector to display.
#' @param n Number of elements to show.
#'
#' @return A character vector suitable for `cli` bullet interpolation.
#' @keywords internal
#' @noRd
truncate_display <- function(x, n = 10L) {
  x <- as.character(x)
  if (length(x) <= n) {
    return(x)
  }
  c(utils::head(x, n), glue::glue("... and {length(x) - n} more"))
}

#' Human-readable byte size
#'
#' @param bytes Numeric vector of byte counts.
#' @return A character vector.
#' @keywords internal
#' @noRd
format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  vapply(bytes, function(b) {
    if (is.na(b)) {
      return("unknown size")
    }
    units <- c("B", "KB", "MB", "GB", "TB")
    i <- if (b <= 0) 1L else min(length(units), floor(log(b, 1024)) + 1L)
    sprintf("%.1f %s", b / 1024^(i - 1L), units[i])
  }, character(1))
}

#' Emit an informational message unless quiet
#'
#' `quiet` suppresses progress chatter only. Warnings and errors always speak.
#'
#' @param ... Passed to [cli::cli_inform()].
#' @param .envir Environment used to evaluate cli glue expressions.
#' @param quiet Logical; when `TRUE` nothing is emitted.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
inform_unless_quiet <- function(..., quiet = FALSE,
                                .envir = rlang::caller_env()) {
  if (!isTRUE(quiet)) cli::cli_inform(..., .envir = .envir)
  invisible(NULL)
}

#' Is this an interactive session?
#'
#' Wrapped so tests can stub it.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
is_interactive <- function() interactive()

#' Coerce a year-ish value to integer
#'
#' @param x Numeric or character years.
#' @return Integer vector.
#' @keywords internal
#' @noRd
as_year_int <- function(x) as.integer(as.character(x))

#' Sequence of years spanning a set of dates
#'
#' @param ... Date vectors; `NA`s are dropped.
#' @return Integer vector of consecutive years, or `integer(0)`.
#' @keywords internal
#' @noRd
year_span <- function(...) {
  dates <- unlist(lapply(list(...), as.character), use.names = FALSE)
  dates <- as.Date(dates[!is.na(dates)])
  if (length(dates) == 0L) {
    return(integer(0))
  }
  yrs <- as.integer(format(dates, "%Y"))
  seq.int(min(yrs), max(yrs))
}

# ---------------------------------------------------------------------------
# Date parsing.
#
# There is no `tz` argument anywhere in this file and there must never be one.
# Filtering targets STORM_GENESIS_DATE_UTC, a UTC *calendar date*, and a
# calendar date has no timezone.
#
# Order inference is JOINT across every string supplied. This matters: taken one
# at a time, "01/04/2001" is ambiguous between mdy and dmy, but in the company
# of "05/19/2001" -- where 19 cannot be a month -- dmy is infeasible for the set,
# so mdy is forced for both and no warning is warranted.
# ---------------------------------------------------------------------------

#' Parse the temporal filter arguments
#'
#' @param date_range Length-2 vector of range endpoints, or `NULL`.
#' @param date_list Vector of specific genesis dates, or `NULL`.
#' @param date_format Explicit order/format string. When supplied, inference is
#'   skipped entirely.
#' @param call Calling environment for error reporting.
#'
#' @return A list with `type` (`"none"`, `"range"`, `"list"`), `start`, `end`,
#'   `dates`, `inferred_order` and `ambiguous`.
#' @keywords internal
#' @noRd
parse_date_input <- function(date_range = NULL,
                             date_list = NULL,
                             date_format = NULL,
                             call = rlang::caller_env()) {
  if (!is.null(date_range) && !is.null(date_list)) {
    cli::cli_abort(
      c("{.arg date_range} and {.arg date_list} cannot both be supplied.",
        i = "{.arg date_range} selects storms forming between two dates;
             {.arg date_list} selects storms forming on specific dates."
      ),
      call = call
    )
  }

  if (is.null(date_range) && is.null(date_list)) {
    return(list(
      type = "none", start = NULL, end = NULL, dates = NULL,
      inferred_order = NULL, ambiguous = FALSE
    ))
  }

  if (!is.null(date_range)) {
    if (length(date_range) != 2L) {
      cli::cli_abort(
        c("{.arg date_range} must have exactly 2 elements.",
          x = "Got {length(date_range)}.",
          i = 'For example {.code date_range = c("2005-08-01", "2005-09-30")}
               or {.code date_range = c(2005, 2010)}.'
        ),
        call = call
      )
    }

    order <- resolve_date_order(date_range, date_format, call = call)
    start <- expand_date_element(date_range[[1]], "start", order$order,
      arg = "date_range", call = call
    )
    end <- expand_date_element(date_range[[2]], "end", order$order,
      arg = "date_range", call = call
    )

    if (start > end) {
      cli::cli_abort(
        c("{.arg date_range} is reversed.",
          x = "Start {.val {format(start)}} is after end {.val {format(end)}}.",
          i = "Supply the earlier date first."
        ),
        call = call
      )
    }

    return(list(
      type = "range", start = start, end = end, dates = NULL,
      inferred_order = order$order, ambiguous = order$ambiguous
    ))
  }

  order <- resolve_date_order(date_list, date_format, call = call)
  # Each element of date_list is a specific genesis date, so partial forms are
  # expanded at their start: "2005-08" would mean 2005-08-01, which is almost
  # certainly not what the user meant. Reject partial forms here instead.
  dates <- vapply(date_list, function(el) {
    as.character(expand_date_element(el, "start", order$order,
      arg = "date_list", allow_partial = FALSE,
      call = call
    ))
  }, character(1))

  list(
    type = "list", start = min(as.Date(dates)), end = max(as.Date(dates)),
    dates = as.Date(unname(dates)), inferred_order = order$order,
    ambiguous = order$ambiguous
  )
}

#' Decide which date order to parse with
#'
#' @param x The user's date elements.
#' @param date_format Explicit order supplied by the user, or `NULL`.
#' @param call Calling environment.
#'
#' @return A list with `order` and `ambiguous`.
#' @keywords internal
#' @noRd
resolve_date_order <- function(x, date_format = NULL,
                               call = rlang::caller_env()) {
  if (!is.null(date_format)) {
    # Explicit instruction bypasses inference completely, by design: a user who
    # has told us the order should never be second-guessed or warned at.
    return(list(order = date_format, ambiguous = FALSE))
  }

  # Only strings that are neither already-unambiguous ISO nor bare year/month
  # forms need an order decided for them.
  candidates <- unlist(lapply(x, function(el) {
    if (inherits(el, c("Date", "POSIXt"))) {
      return(NULL)
    }
    s <- as.character(el)
    if (grepl("^\\d{4}$", s)) {
      return(NULL)
    }
    if (grepl("^\\d{4}-\\d{1,2}$", s)) {
      return(NULL)
    }
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", s)) {
      return(NULL)
    }
    s
  }), use.names = FALSE)

  if (length(candidates) == 0L) {
    return(list(order = "ymd", ambiguous = FALSE))
  }

  orders <- c("ymd", "mdy", "dmy")
  feasible <- orders[vapply(orders, function(o) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(candidates, orders = o, quiet = TRUE)
    )
    !anyNA(parsed)
  }, logical(1))]

  if (length(feasible) == 0L) {
    cli::cli_abort(
      c("Could not interpret the supplied date value(s).",
        x = "Offending values: {.val {candidates}}.",
        i = 'Supply {.arg date_format}, e.g. {.code date_format = "dmy"}, or use
             ISO dates such as {.val 2005-08-29}.'
      ),
      call = call
    )
  }

  if (length(feasible) == 1L) {
    return(list(order = feasible, ambiguous = FALSE))
  }

  # Genuine ambiguity survives joint inference only when every supplied string
  # is consistent with more than one order. Precedence is ymd > mdy > dmy;
  # we warn because the alternative reading is a real possibility.
  chosen <- feasible[1]
  rejected <- setdiff(feasible, chosen)
  example <- lubridate::parse_date_time(candidates[1], orders = chosen)

  cli::cli_warn(c(
    "Date{?s} {.val {candidates}} {?is/are} ambiguous.",
    i = "Interpreting as {.val {chosen}}, so {.val {candidates[1]}} means
         {.val {format(as.Date(example))}}.",
    i = "Rejected alternative{?s}: {.val {rejected}}.",
    i = 'Set {.arg date_format} to be explicit, e.g.
         {.code date_format = "{rejected[1]}"}.'
  ))

  list(order = chosen, ambiguous = TRUE)
}

#' Expand one date element to a concrete date
#'
#' Partial inputs expand to the start of their period for the first element of a
#' range and to the end for the second, so `c(2001, 2003)` covers all three
#' years rather than collapsing to a single day.
#'
#' @param x A single element: a year, year-month, date string, `Date` or
#'   `POSIXct`.
#' @param side `"start"` or `"end"`.
#' @param order Date order to parse with.
#' @param arg Argument name for error messages.
#' @param allow_partial Whether year and year-month forms are acceptable.
#' @param call Calling environment.
#'
#' @return A `Date` of length 1.
#' @keywords internal
#' @noRd
expand_date_element <- function(x, side = c("start", "end"), order = "ymd",
                                arg = "date_range", allow_partial = TRUE,
                                call = rlang::caller_env()) {
  side <- match.arg(side)

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }

  s <- trimws(as.character(x))

  # Bare year.
  if (grepl("^\\d{4}$", s)) {
    if (!allow_partial) abort_partial(s, arg, call)
    y <- as.integer(s)
    return(if (side == "start") {
      as.Date(sprintf("%04d-01-01", y))
    } else {
      as.Date(sprintf("%04d-12-31", y))
    })
  }

  # Year-month.
  if (grepl("^\\d{4}-\\d{1,2}$", s)) {
    if (!allow_partial) abort_partial(s, arg, call)
    first <- as.Date(paste0(s, "-01"))
    return(if (side == "start") {
      first
    } else {
      # Last day of the month, without hardcoding month lengths or leap years.
      lubridate::ceiling_date(first, unit = "month") - 1
    })
  }

  # Full date. ISO strings are parsed as ISO no matter what order was inferred
  # elsewhere in the same call; a well-formed ISO date has only one reading.
  if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", s)) {
    return(as.Date(lubridate::ymd(s, quiet = TRUE)))
  }

  parsed <- suppressWarnings(
    lubridate::parse_date_time(s, orders = order, quiet = TRUE)
  )
  if (is.na(parsed)) {
    cli::cli_abort(
      c("Could not parse {.val {s}} in {.arg {arg}} as a date.",
        i = "Supply {.arg date_format} or use an ISO date such as
             {.val 2005-08-29}."
      ),
      call = call
    )
  }

  as.Date(parsed)
}

#' @keywords internal
#' @noRd
abort_partial <- function(s, arg, call) {
  cli::cli_abort(
    c("{.val {s}} in {.arg {arg}} is not a complete date.",
      i = "{.arg {arg}} matches storm genesis dates exactly, so each value must
           name a single day.",
      i = "Use {.arg date_range} to select a span of time."
    ),
    call = call
  )
}

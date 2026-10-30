# ---------------------------------------------------------------------------
# Source availability is metadata, not a promise that every requested record
# exists. It makes known upstream update and lag behaviour visible beside an
# otherwise reproducible source-file manifest.
# ---------------------------------------------------------------------------

#' Build structured source availability and coverage metadata
#'
#' @param date_spec Parsed date selection from `parse_date_input()`.
#' @param tables Normalized requested tables.
#' @param storm_metadata Resolved storm metadata, when available.
#' @param links Computed hazard links, when available.
#' @param today Current date, injectable for deterministic tests.
#' @return A named list.
#' @keywords internal
#' @noRd
availability_metadata <- function(date_spec,
                                  tables,
                                  storm_metadata = NULL,
                                  links = NULL,
                                  today = Sys.Date()) {
  requested_coverage <- availability_coverage(
    if (identical(date_spec$type, "none")) as.Date(character(0)) else date_spec$start,
    if (identical(date_spec$type, "none")) as.Date(character(0)) else date_spec$end
  )
  storm_coverage <- if (is.null(storm_metadata)) {
    # This fallback keeps the helper useful before selection has run. Retrievals
    # pass resolved metadata below, so an open-ended query is never assumed to
    # include the current period merely because it omitted a date filter.
    requested_coverage
  } else {
    availability_coverage(storm_metadata$STORM_GENESIS_DATE_UTC)
  }
  precip_coverage <- availability_window_coverage(links, "PRECIP")
  flood_coverage <- availability_window_coverage(links, "FLOOD")

  # Without links, there is no planned precip/flood extraction. The fallback is
  # for callers using this helper before planning, not for assembled retrievals.
  if (is.null(links)) {
    precip_coverage <- requested_coverage
    flood_coverage <- requested_coverage
  }

  requested <- list(
    start = if (identical(date_spec$type, "none")) as.Date(NA) else date_spec$start,
    end = if (identical(date_spec$type, "none")) as.Date(NA) else date_spec$end,
    includes_current_period = availability_reaches(storm_coverage, today - 7)
  )
  list(
    assessed_on = as.Date(today),
    requested = requested,
    coverage = list(
      storms = storm_coverage,
      precip = precip_coverage,
      flood = flood_coverage
    ),
    gtropic = list(
      update_schedule = "Sundays around 02:00 America/New_York"
    ),
    ibtracs = list(
      raw_update_schedule = "Sunday, Tuesday, and Thursday",
      provisional_lag_days = c(1L, 3L),
      best_track_finalization_years = c(1L, 2L),
      applies_to = intersect(tables, c("wind", "zero_pairs", "tracks", "storm_metadata"))
    ),
    era5_land = list(
      frequency = "daily",
      typical_lag_days = 5L,
      applies_to = intersect(tables, "precip")
    ),
    groundsource_flood = list(
      available_from = as.Date("2000-01-01"),
      available_through = as.Date("2026-01-22"),
      applies_to = intersect(tables, "flood")
    )
  )
}

#' Generate targeted availability diagnostics
#'
#' @param availability Output of `availability_metadata()`.
#' @return Named character list suitable for `gtropic_warnings()`.
#' @keywords internal
#' @noRd
availability_warnings <- function(availability) {
  requested <- availability$requested
  dynamic_end <- isTRUE(requested$includes_current_period)
  coverage <- availability$coverage
  out <- list()

  if (length(availability$ibtracs$applies_to) > 0L &&
    availability_reaches(coverage$storms, availability$assessed_on - 7)) {
    out$ibtracs_provisional <- paste(
      "Requested storm data reaches the current/recent IBTrACS period:",
      "raw updates are published Sunday, Tuesday, and Thursday and can lag",
      "by 1-3 days; best-track values can remain provisional for 1-2 years."
    )
  }
  if (length(availability$era5_land$applies_to) > 0L &&
    availability_reaches(coverage$precip, availability$assessed_on - 7)) {
    out$era5_land_lag <- paste(
      "Requested precipitation reaches the current/recent ERA5-Land period;",
      "daily data are typically available with about a 5-day lag."
    )
  }
  flood_end <- availability$groundsource_flood$available_through
  if (length(availability$groundsource_flood$applies_to) > 0L &&
    availability_reaches(coverage$flood, flood_end)) {
    qualifier <- if (coverage$flood$end > flood_end) "exceeds" else "reaches"
    out$groundsource_flood_coverage <- paste0(
      "Requested flood time ", qualifier,
      " known Groundsource flood availability, which ends on ", format(flood_end), "."
    )
  }
  if (length(out) > 0L && dynamic_end) {
    out$gtropic_update_schedule <- paste(
      "G-TROPIC is normally updated Sundays around 02:00 America/New_York;",
      "a current-period result may change after the next update."
    )
  }
  out
}

#' Build a temporal coverage interval
#' @keywords internal
#' @noRd
availability_coverage <- function(...) {
  parts <- lapply(list(...), as.Date)
  dates <- if (length(parts) == 0L) as.Date(character(0)) else do.call(c, parts)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0L) {
    return(list(start = as.Date(NA), end = as.Date(NA)))
  }
  list(start = min(dates), end = max(dates))
}

#' Extract the coverage of links within a requested hazard field
#' @keywords internal
#' @noRd
availability_window_coverage <- function(links, prefix) {
  if (is.null(links)) return(NULL)

  cols <- paste0(prefix, c("_IN_FIELD", "_WINDOW_START", "_WINDOW_END"))
  if (!all(cols %in% names(links))) {
    return(availability_coverage())
  }
  in_field <- !is.na(links[[cols[1]]]) & links[[cols[1]]]
  availability_coverage(links[[cols[2]]][in_field], links[[cols[3]]][in_field])
}

#' Does a temporal coverage interval reach a threshold date?
#' @keywords internal
#' @noRd
availability_reaches <- function(coverage, threshold) {
  !is.null(coverage) && !is.na(coverage$end) && coverage$end >= threshold
}

# ---------------------------------------------------------------------------
# Storm selection is storm-level and geography-independent.
#
# date_range selects storms whose STORM_GENESIS_DATE_UTC falls in the range,
# and then returns all requested data for those storms. Two consequences that
# surprise people and are documented prominently:
#
#   * Data outside the range comes back. A storm forming on the last day of the
#     range and lasting ten days contributes exposure and precip rows well past
#     DATE_END.
#   * A storm that formed before the range is excluded entirely, even if it
#     struck the requested region inside the range. A storm forming 28 August
#     that hits Texas on 3 September is NOT returned by
#     date_range = c("2002-09-01", "2002-09-30").
#
# The payoff, which is why it is designed this way: date_range selects the same
# storm set every time, independent of the geographic filters.
# ---------------------------------------------------------------------------

#' Resolve the storm filters to a set of storm identifiers
#'
#' @param storm_metadata The `storm_metadata` table for the candidate years.
#' @param date_spec Output of `parse_date_input()`.
#' @param storm_filters Named list with `STORM_ID`, `STORM_NAME`,
#'   `USA_ATCF_ID`.
#' @param call Calling environment for error reporting.
#'
#' @return A character vector of `STORM_ID`.
#' @keywords internal
#' @noRd
resolve_storms <- function(storm_metadata,
                           date_spec = list(type = "none"),
                           storm_filters = list(),
                           call = rlang::caller_env()) {
  storm_filters <- compact(storm_filters)

  if (!"STORM_GENESIS_DATE_UTC" %in% names(storm_metadata)) {
    cli::cli_abort(
      c("The published {.field storm_metadata} table has no
         {.field STORM_GENESIS_DATE_UTC} column.",
        i = "All temporal filtering keys on it; the dataset may predate the
             column's introduction."
      ),
      call = call
    )
  }

  genesis <- as.Date(storm_metadata$STORM_GENESIS_DATE_UTC)
  keep <- rep(TRUE, nrow(storm_metadata))
  used_args <- character(0)

  # --- temporal ------------------------------------------------------------
  if (identical(date_spec$type, "range")) {
    keep <- keep & !is.na(genesis) &
      genesis >= date_spec$start & genesis <= date_spec$end
    used_args <- c(used_args, "date_range")
  } else if (identical(date_spec$type, "list")) {
    keep <- keep & !is.na(genesis) & genesis %in% date_spec$dates
    used_args <- c(used_args, "date_list")

    unmatched <- setdiff(
      as.character(date_spec$dates),
      as.character(genesis[!is.na(genesis)])
    )
    if (length(unmatched) > 0L) {
      cli::cli_abort(
        c("{length(unmatched)} date{?s} in {.arg date_list} matched no storm
           genesis date.",
          x = "{.val {truncate_display(unmatched)}}",
          i = "{.arg date_list} matches {.field STORM_GENESIS_DATE_UTC}
               exactly -- the date a storm first appears in the best-track
               record, not the date it made landfall.",
          i = "See {.code gtropic_storms()} for genesis dates."
        ),
        call = call
      )
    }
  }

  # --- storm identity ------------------------------------------------------
  id_spec <- list(
    STORM_ID = list(column = "STORM_ID", exact = TRUE),
    USA_ATCF_ID = list(column = "USA_ATCF_ID", exact = TRUE),
    STORM_NAME = list(column = "STORM_NAME", exact = FALSE)
  )

  for (arg in names(id_spec)) {
    values <- storm_filters[[arg]]
    if (is.null(values)) next

    column <- id_spec[[arg]]$column
    if (!column %in% names(storm_metadata)) {
      cli::cli_abort(
        "Column {.field {column}} is not present in {.field storm_metadata}.",
        call = call
      )
    }

    canonical <- match_values(
      values,
      candidates = storm_metadata[[column]],
      arg = arg,
      exact = id_spec[[arg]]$exact,
      helper = "gtropic_storms()",
      what = "storm",
      call = call
    )

    hit <- if (id_spec[[arg]]$exact) {
      storm_metadata[[column]] %in% canonical
    } else {
      norm_key(storm_metadata[[column]]) %in% norm_key(canonical)
    }

    keep <- keep & hit
    used_args <- c(used_args, arg)
  }

  if (!any(keep)) {
    cli::cli_abort(
      c("No storms satisfy all of the filters supplied.",
        x = "Filters in play: {.arg {used_args}}.",
        i = "Storm and date filters are combined with AND. A storm is selected
             by the date its track begins, not by when or where it struck.",
        i = "Use {.code gtropic_storms()} to explore the available storms."
      ),
      call = call
    )
  }

  unique(as.character(storm_metadata$STORM_ID[keep]))
}

#' Verify the genesis-year file convention on loaded metadata
#'
#' The backend assigns a storm's rows to the file for `year(min(ISO_TIME))`,
#' which is by construction the year of `STORM_GENESIS_DATE_UTC`. Every
#' year-scanning rule in the planner depends on that. If the convention ever
#' changes upstream we must not silently drop storms, so on failure we widen the
#' scan by a year each way, warn, and continue.
#'
#' @param storm_metadata Loaded metadata with a `.gtropic_file_year` column
#'   recording which yearly file each row came from.
#' @param years The years that were scanned.
#'
#' @return A list with `years` (possibly widened) and `violated` (logical).
#' @keywords internal
#' @noRd
assert_genesis_year_convention <- function(storm_metadata, years) {
  if (nrow(storm_metadata) == 0L ||
    !".gtropic_file_year" %in% names(storm_metadata)) {
    return(list(years = years, violated = FALSE))
  }

  # (a) no STORM_ID may appear in more than one yearly file
  dup <- storm_metadata |>
    dplyr::distinct(.data$STORM_ID, .data$.gtropic_file_year) |>
    dplyr::count(.data$STORM_ID) |>
    dplyr::filter(.data$n > 1L)

  # (b) every storm's genesis year must equal its file year
  genesis_year <- as.integer(format(
    as.Date(storm_metadata$STORM_GENESIS_DATE_UTC), "%Y"
  ))
  mismatch <- which(genesis_year != storm_metadata$.gtropic_file_year)

  if (nrow(dup) == 0L && length(mismatch) == 0L) {
    return(list(years = years, violated = FALSE))
  }

  offenders <- unique(c(
    as.character(dup$STORM_ID),
    as.character(storm_metadata$STORM_ID[mismatch])
  ))

  cli::cli_warn(c(
    "The published storm-to-year file assignment does not match the expected
     genesis-year convention.",
    "!" = "{length(offenders)} storm{?s} affected: {truncate_display(offenders)}",
    i = "Widening the year scan by one year in each direction so no storms are
         silently dropped. Results remain correct but the query will be slower."
  ))

  widened <- seq.int(min(years) - 1L, max(years) + 1L)
  list(years = widened, violated = TRUE)
}

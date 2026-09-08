# ---------------------------------------------------------------------------
# Linkage: which precip and flood rows belong to which ADM2-storm pair.
#
# A pair is defined by a temporal window and a spatial field:
#
#   window = [LOCAL_DATE_STORM_CLOSEST - days_before,
#             LOCAL_DATE_STORM_CLOSEST + days_after]
#
#   field  = all evaluated pairs (from wind AND zero_pairs) with
#            STORM_DIST_KM <= radius_km, intersected with the user's geography
#
# The user's geographic filter is an OUTER BOUND. The radius narrows within it
# and never widens beyond it: asking for Florida never returns Georgia because
# a storm's 500 km field reached there.
#
# STORM_DIST_KM is authoritative and must never be recomputed from storm_tracks.
# The published column derives from a 15-minute interpolated track; the tracks
# table ships raw ~3-hourly observations. Recomputing would drift from the
# dataset and produce results that disagree with the publication.
# ---------------------------------------------------------------------------

#' Derive the local date of closest approach
#'
#' `LOCAL_DATETIME_STORM_CLOSEST` is a string in ISO 8601 form with a UTC
#' offset, e.g. `2005-08-29T06:10:00-05:00`. Because it is *already* expressed
#' in local time, the local calendar date is simply its first 10 characters. No
#' timezone arithmetic is required, and doing any would be a bug.
#'
#' The extraction is expressed with [stringr::str_sub()] so that it is pushed
#' down into the Arrow pipeline rather than materialising every row in R.
#'
#' @param x An Arrow Table or data frame containing
#'   `LOCAL_DATETIME_STORM_CLOSEST`.
#' @param validate Whether to check the string format on a sample.
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return `x` with a `LOCAL_DATE_STORM_CLOSEST` character column added.
#' @keywords internal
#' @noRd
derive_local_date <- function(x, validate = TRUE, label = "file",
                              call = rlang::caller_env()) {
  if (!"LOCAL_DATETIME_STORM_CLOSEST" %in% names(x)) {
    cli::cli_abort(
      c("{.field LOCAL_DATETIME_STORM_CLOSEST} is missing from
         {.path {label}}.",
        i = "It anchors every precipitation and flood window."
      ),
      call = call
    )
  }

  if (validate) validate_anchor(x, label = label, call = call)

  dplyr::mutate(
    x,
    LOCAL_DATE_STORM_CLOSEST = stringr::str_sub(
      .data$LOCAL_DATETIME_STORM_CLOSEST, 1L, 10L
    )
  )
}

#' Validate the anchor column's format
#'
#' Validate, do not assume. Unpadded components (`2005-8-29T...`) would make the
#' 10-character extraction silently produce garbage, corrupting every window
#' downstream, and the result would look entirely plausible. A missing anchor is
#' equally fatal: the column is documented as never missing, so an `NA` means
#' something has gone wrong upstream.
#'
#' @param x Arrow Table or data frame.
#' @param n Sample size to check.
#' @param label Path used in messages.
#' @param call Calling environment.
#'
#' @return Invisibly `TRUE`; aborts on a malformed or missing value.
#' @keywords internal
#' @noRd
validate_anchor <- function(x, n = 1000L, label = "file",
                            call = rlang::caller_env()) {
  sample_vals <- x |>
    dplyr::select("LOCAL_DATETIME_STORM_CLOSEST") |>
    utils::head(n) |>
    dplyr::collect()
  vals <- sample_vals[[1]]

  if (length(vals) == 0L) {
    return(invisible(TRUE))
  }

  if (anyNA(vals)) {
    cli::cli_abort(
      c("{.field LOCAL_DATETIME_STORM_CLOSEST} contains missing values in
         {.path {label}}.",
        i = "This column anchors every precipitation and flood window and is
             documented as never missing.",
        i = "Please report this against the published dataset."
      ),
      call = call
    )
  }

  bad <- vals[!grepl("^\\d{4}-\\d{2}-\\d{2}T", vals)]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c("{.field LOCAL_DATETIME_STORM_CLOSEST} is not in the expected ISO 8601
         form in {.path {label}}.",
        x = "For example: {.val {utils::head(bad, 3)}}",
        i = "Expected {.val 2005-08-29T06:10:00-05:00}, with zero-padded
             components.",
        i = "Window dates are read from the first 10 characters, so an
             unpadded value would silently corrupt every window."
      ),
      call = call
    )
  }

  invisible(TRUE)
}

#' Build the links table
#'
#' One row per ADM2-storm pair. `links` is always returned alongside any
#' storm-linked table: it is what lets users join precip and flood back to
#' storms without the package duplicating rows on their behalf.
#'
#' @param wind The `wind` table (nonzero exposures), already collected.
#' @param zero_pairs The `zero_pairs` table, already collected. May be `NULL`
#'   when neither precip nor flood was requested.
#' @param adm2_ids Resolved ADM2 identifiers (the outer geographic bound).
#' @param storm_ids Resolved storm identifiers.
#' @param precip_window List with `before`, `after`, `radius_km`.
#' @param flood_window List with `before`, `after`, `radius_km`.
#'
#' @return A tibble as described in the plan's links schema.
#' @keywords internal
#' @noRd
build_links <- function(wind,
                        zero_pairs = NULL,
                        adm2_ids,
                        storm_ids,
                        precip_window = list(
                          before = 2, after = 1,
                          radius_km = 500
                        ),
                        flood_window = list(
                          before = 2, after = 1,
                          radius_km = 500
                        )) {
  cols <- c("ADM2_ID", "STORM_ID", "STORM_DIST_KM", "LOCAL_DATE_STORM_CLOSEST")

  take <- function(df, exposure) {
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    df |>
      dplyr::filter(
        .data$ADM2_ID %in% adm2_ids,
        .data$STORM_ID %in% storm_ids
      ) |>
      dplyr::select(dplyr::all_of(cols)) |>
      dplyr::mutate(WIND_EXPOSURE = exposure)
  }

  pairs <- dplyr::bind_rows(
    take(wind, TRUE),
    take(zero_pairs, FALSE)
  )

  if (is.null(pairs) || nrow(pairs) == 0L) {
    return(links_empty())
  }

  # A pair should appear in exactly one of the two tables. If it somehow appears
  # in both, the nonzero record is the informative one.
  pairs <- pairs |>
    dplyr::arrange(
      .data$ADM2_ID, .data$STORM_ID,
      dplyr::desc(.data$WIND_EXPOSURE)
    ) |>
    dplyr::distinct(.data$ADM2_ID, .data$STORM_ID, .keep_all = TRUE)

  anchor <- as.Date(pairs$LOCAL_DATE_STORM_CLOSEST)

  tibble::tibble(
    STORM_ID = pairs$STORM_ID,
    ADM2_ID = pairs$ADM2_ID,
    LOCAL_DATE_STORM_CLOSEST = anchor,
    STORM_DIST_KM = pairs$STORM_DIST_KM,
    WIND_EXPOSURE = pairs$WIND_EXPOSURE,
    # The *_IN_FIELD flags make it explicit why a pair has no precip or flood
    # rows: outside the radius, versus inside it with genuinely no data.
    PRECIP_IN_FIELD = !is.na(pairs$STORM_DIST_KM) &
      pairs$STORM_DIST_KM <= precip_window$radius_km,
    PRECIP_WINDOW_START = anchor - precip_window$before,
    PRECIP_WINDOW_END = anchor + precip_window$after,
    FLOOD_IN_FIELD = !is.na(pairs$STORM_DIST_KM) &
      pairs$STORM_DIST_KM <= flood_window$radius_km,
    FLOOD_WINDOW_START = anchor - flood_window$before,
    FLOOD_WINDOW_END = anchor + flood_window$after
  )
}

#' @keywords internal
#' @noRd
links_empty <- function() {
  tibble::tibble(
    STORM_ID = character(0), ADM2_ID = character(0),
    LOCAL_DATE_STORM_CLOSEST = as.Date(character(0)),
    STORM_DIST_KM = numeric(0), WIND_EXPOSURE = logical(0),
    PRECIP_IN_FIELD = logical(0),
    PRECIP_WINDOW_START = as.Date(character(0)),
    PRECIP_WINDOW_END = as.Date(character(0)),
    FLOOD_IN_FIELD = logical(0),
    FLOOD_WINDOW_START = as.Date(character(0)),
    FLOOD_WINDOW_END = as.Date(character(0))
  )
}

#' The calendar years the computed windows span
#'
#' Derived from the real window bounds rather than assumed. Boundary crossing --
#' a late-December storm whose window reaches into January -- falls out
#' naturally, with no hardcoded plus-or-minus one.
#'
#' @param links A links tibble.
#' @return A list with integer vectors `precip` and `flood`.
#' @keywords internal
#' @noRd
window_years <- function(links) {
  in_precip <- links[links$PRECIP_IN_FIELD, , drop = FALSE]
  in_flood <- links[links$FLOOD_IN_FIELD, , drop = FALSE]

  list(
    precip = year_span(
      in_precip$PRECIP_WINDOW_START,
      in_precip$PRECIP_WINDOW_END
    ),
    flood = year_span(in_flood$FLOOD_WINDOW_START, in_flood$FLOOD_WINDOW_END)
  )
}

#' Select the precipitation rows falling in any pair's window
#'
#' Rows are NOT duplicated per storm. One ADM2's precipitation day can fall
#' inside two storms' windows; emitting it twice would silently break anyone
#' summing PRECIP_MM, and they would have no way to notice. Tidy tables plus
#' `links` let users join deliberately and decide for themselves how to handle
#' overlaps.
#'
#' @param links A links tibble.
#' @param precip The `precip` table, collected.
#' @param date_col Name of the local date column in `precip`.
#'
#' @return A tibble of distinct precipitation rows.
#' @keywords internal
#' @noRd
select_precip <- function(links, precip) {
  if (is.null(precip) || nrow(precip) == 0L) {
    return(precip)
  }

  field <- links[links$PRECIP_IN_FIELD, , drop = FALSE]
  if (nrow(field) == 0L) {
    return(precip[0, , drop = FALSE])
  }

  precip <- dplyr::mutate(precip, .gtropic_date = as.Date(.data[["LOCAL_DATE"]]))

  # dplyr >= 1.1 non-equi join. The semi_join keeps precip's grain intact: one
  # row per ADM2-date no matter how many pairs it satisfies.
  dplyr::semi_join(
    precip,
    dplyr::select(field, "ADM2_ID", "PRECIP_WINDOW_START", "PRECIP_WINDOW_END"),
    by = dplyr::join_by(
      ADM2_ID,
      dplyr::between(
        .gtropic_date, PRECIP_WINDOW_START,
        PRECIP_WINDOW_END
      )
    )
  ) |>
    dplyr::select(-".gtropic_date")
}

#' Select the flood rows matching any pair's window
#'
#' @param links A links tibble.
#' @param flood The `flood` table, collected.
#' @param match `"start"` matches floods whose `FLOOD_START_DATE` falls inside
#'   the window. `"overlap"` also matches floods already in progress when the
#'   storm arrived.
#'
#' @return A tibble of distinct flood rows.
#' @keywords internal
#' @noRd
select_flood <- function(links, flood, match = c("start", "overlap")) {
  match <- rlang::arg_match(match)
  if (is.null(flood) || nrow(flood) == 0L) {
    return(flood)
  }

  field <- links[links$FLOOD_IN_FIELD, , drop = FALSE]
  if (nrow(field) == 0L) {
    return(flood[0, , drop = FALSE])
  }

  flood <- dplyr::mutate(
    flood,
    .gtropic_start = as.Date(.data$FLOOD_START_DATE),
    .gtropic_end = as.Date(.data$FLOOD_END_DATE)
  )

  windows <- dplyr::select(
    field, "ADM2_ID", "FLOOD_WINDOW_START",
    "FLOOD_WINDOW_END"
  )

  out <- if (identical(match, "start")) {
    dplyr::semi_join(
      flood, windows,
      by = dplyr::join_by(
        ADM2_ID,
        dplyr::between(
          .gtropic_start, FLOOD_WINDOW_START,
          FLOOD_WINDOW_END
        )
      )
    )
  } else {
    # Interval overlap: the flood had started by the window's end and had not
    # finished before the window began.
    dplyr::semi_join(
      flood, windows,
      by = dplyr::join_by(
        ADM2_ID,
        .gtropic_start <= FLOOD_WINDOW_END,
        .gtropic_end >= FLOOD_WINDOW_START
      )
    )
  }

  dplyr::select(out, -".gtropic_start", -".gtropic_end")
}

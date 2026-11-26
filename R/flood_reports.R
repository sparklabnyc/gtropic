# ---------------------------------------------------------------------------
# Flood rows come from Groundsource, a record of places named in news reports
# about flooding. Each published row pairs one reported place's outline with
# one ADM2 unit, so the published PROP_FLOODED_* names overstate what the
# values measure. gtropic renames them on read (see ?gtropic_known_limitations).
# ---------------------------------------------------------------------------

#' Published flood column names and the names gtropic returns
#' @keywords internal
#' @noRd
FLOOD_COLUMN_NAMES <- c(
  PROP_FLOODED_AREA = "PROP_AREA_IN_FLOOD_REPORT",
  PROP_FLOODED_POP = "PROP_POP_IN_FLOOD_REPORT",
  FLOODED_POP_CENTROID = "POP_CENTROID_IN_FLOOD_REPORT"
)

#' Rename published flood columns to the names gtropic returns
#'
#' @param flood A flood table as read from the published files, or `NULL`.
#' @return `flood` with any published `PROP_FLOODED_*` or
#'   `FLOODED_POP_CENTROID` column renamed.
#' @keywords internal
#' @noRd
rename_flood_columns <- function(flood) {
  if (is.null(flood)) {
    return(flood)
  }
  published <- match(names(flood), names(FLOOD_COLUMN_NAMES))
  renamed <- !is.na(published)
  names(flood)[renamed] <- unname(FLOOD_COLUMN_NAMES[published[renamed]])
  flood
}

#' The period covered by the Groundsource flood record
#'
#' Flood windows outside it have no flood data at all, so a missing report
#' there means "not recorded", never "no flood reported".
#' @keywords internal
#' @noRd
GROUNDSOURCE_FLOOD_PERIOD <- as.Date(c("2000-01-01", "2026-01-22"))

#' Whether each flood window lies inside the Groundsource record
#'
#' @param start,end `Date` vectors of flood-window bounds.
#' @return A logical vector; `FALSE` where a bound is missing.
#' @keywords internal
#' @noRd
flood_window_recorded <- function(start, end) {
  !is.na(start) &
    !is.na(end) &
    start >= GROUNDSOURCE_FLOOD_PERIOD[1L] &
    end <= GROUNDSOURCE_FLOOD_PERIOD[2L]
}

#' Short names of the flood shares used in yes/no panel labels
#' @keywords internal
#' @noRd
FLOOD_REPORT_SHARE_LABELS <- c(
  PROP_AREA_IN_FLOOD_REPORT = "area in flood report",
  PROP_POP_IN_FLOOD_REPORT = "population in flood report"
)

#' Whether a resolved metric is drawn as a yes/no flood-report panel
#' @keywords internal
#' @noRd
plot_is_flood_binary <- function(spec) {
  isTRUE(spec$binary) && identical(as.character(spec$table), "flood")
}

#' Labels for the two classes of a yes/no panel, in FALSE, TRUE order
#' @keywords internal
#' @noRd
plot_binary_labels <- function(spec) {
  if (plot_is_flood_binary(spec)) {
    c("No flood reported", "Flood reported")
  } else {
    c("Not exposed", "Exposed")
  }
}

#' Panel label of a yes/no flood-report panel, with its threshold
#' @keywords internal
#' @noRd
plot_flood_binary_label <- function(spec) {
  share <- FLOOD_REPORT_SHARE_LABELS[as.character(spec$metric)]
  if (is.na(share)) {
    share <- as.character(spec$metric)
  }
  paste0(
    "Flood reported (",
    share,
    " ",
    spec$operator,
    " ",
    plot_format_number(spec$threshold),
    ")"
  )
}

#' The flood-data note, as kept in the diagnostics of a result
#' @keywords internal
#' @noRd
FLOOD_REPORTS_NOTE <- paste(
  "The raw flood data from Groundsource consists of polygons of places named",
  "in news reports about flooding, rather than detailed flood maps. As noted",
  "by Groundsource, flood data is both spatially and temporally biased, such",
  "that flood events in regions and time periods with more accurate reporting",
  "are over-represented while those lacking in news coverage are",
  "under-represented. As such, care should be taken when interpreting and",
  "utilizing G-TROPIC flood exposure metrics. See ?gtropic_known_limitations."
)

#' Warn that flood rows describe news reports, once per session
#'
#' @param call Calling environment for warning reporting.
#' @param id Frequency identifier; tests pass their own.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
warn_flood_reports <- function(
  call = rlang::caller_env(),
  id = "gtropic_flood_reports"
) {
  gtropic_warn(
    c(
      "The raw flood data from Groundsource consists of polygons of places
       named in news reports about flooding, rather than detailed flood
       maps.",
      "!" = "As noted by Groundsource, flood data is both spatially and
             temporally biased, such that flood events in regions and time
             periods with more accurate reporting are over-represented while
             those lacking in news coverage are under-represented.",
      "i" = "As such, care should be taken when interpreting and utilizing
             G-TROPIC flood exposure metrics. See
             {.code ?gtropic_known_limitations}."
    ),
    class = "gtropic_warning_flood_reports",
    call = call,
    .frequency = "once",
    .frequency_id = id
  )
  invisible(NULL)
}

#' The flood-data note used in plot captions
#' @keywords internal
#' @noRd
plot_flood_reports_caption <- function() {
  paste0(
    "Flood data: places named in news reports (Groundsource), not flood maps.",
    "\nSpatially and temporally biased by news coverage; see ",
    "?gtropic_known_limitations."
  )
}

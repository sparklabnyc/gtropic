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

# ---------------------------------------------------------------------------
# Turning a query into a concrete list of files.
#
# Everything here is pure: given years, tables and a year index it produces a
# tibble of (dataset, table, year, directory_label, label). No network, no disk,
# no options lookups. That is what makes the planner testable under R CMD check,
# which runs without a reliable network.
# ---------------------------------------------------------------------------

#' Table path specifications
#'
#' Dataverse has no real directories; `directory_label` is just a string
#' attached to each file. These are the strings the backend publishes.
#'
#' @format A named list.
#' @keywords internal
#' @noRd
TABLE_SPEC <- list(
  pop = list(dir = "01_pop", yearly = TRUE),
  wind = list(dir = "02_wind/exposures", yearly = TRUE),
  zero_pairs = list(dir = "02_wind/zero_pairs", yearly = TRUE),
  storm_metadata = list(dir = "02_wind/storm_metadata", yearly = TRUE),
  tracks = list(dir = "02_wind/storm_tracks", yearly = TRUE),
  flood = list(dir = "03_flood", yearly = TRUE),
  precip = list(dir = "04_precip", yearly = TRUE),
  # All metadata/ files live in the historical dataset only, whatever year span
  # the query covers. There is exactly one copy of each and no divergence case.
  adm2 = list(
    dir = "metadata", label = "adm2.parquet",
    yearly = FALSE, dataset = "historical"
  ),
  codebook = list(
    dir = "metadata", label = "codebook.json",
    yearly = FALSE, dataset = "historical"
  ),
  geometry_full = list(
    dir = "metadata", label = "adm2_geometry.parquet",
    yearly = FALSE, dataset = "historical"
  ),
  geometry_simplified = list(
    dir = "metadata", label = "adm2_geometry_simplified.parquet",
    yearly = FALSE, dataset = "historical"
  )
)

#' All table names a user may request
#' @keywords internal
#' @noRd
VALID_TABLES <- c(
  "wind", "precip", "flood", "pop", "adm2", "codebook",
  "zero_pairs", "tracks", "geometry", "storm_metadata"
)

#' Tables whose presence implies storm linkage
#' @keywords internal
#' @noRd
STORM_LINKED_TABLES <- c("wind", "precip", "flood", "zero_pairs", "tracks")

#' Normalise and validate the `tables` argument
#'
#' `storm_metadata` is always included when any storm-linked table is returned:
#' the results are uninterpretable without storm names and genesis dates, and
#' the file is small. `zero_pairs`, `tracks` and `geometry` are opt-in only.
#'
#' @param tables User-supplied table names.
#' @param call Calling environment.
#'
#' @return A character vector of validated table names.
#' @keywords internal
#' @noRd
normalize_tables <- function(tables, call = rlang::caller_env()) {
  tables <- unique(as.character(tables))

  bad <- setdiff(tables, VALID_TABLES)
  if (length(bad) > 0L) {
    cli::cli_abort(
      c("Unknown table{?s} in {.arg tables}: {.val {bad}}.",
        i = "Valid values are {.val {VALID_TABLES}}."
      ),
      call = call
    )
  }

  if (any(tables %in% STORM_LINKED_TABLES) &&
    !"storm_metadata" %in% tables) {
    tables <- c(tables, "storm_metadata")
  }

  tables
}

#' Does this query need zero-pairs data internally?
#'
#' STORM_DIST_KM for far-field ADM2s exists ONLY in zero_pairs. An ADM2 several
#' hundred kilometres inland that received heavy rain but no measurable wind
#' appears nowhere else -- and those are exactly the compound-hazard cases the
#' dataset exists to support. So whenever precip or flood is requested, the
#' zero-pairs files are downloaded and read regardless of `tables`.
#'
#' `"zero_pairs" %in% tables` controls only whether the table is *returned to
#' the user*, never whether it is fetched.
#'
#' @param tables Normalised table names.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
needs_zero_pairs <- function(tables) {
  any(c("precip", "flood", "zero_pairs") %in% tables)
}

#' Build the file plan
#'
#' @param years_by_table Named list mapping each yearly table to the integer
#'   years it must be read for.
#' @param tables Normalised table names (drives the non-yearly entries).
#' @param year_index Tibble of `year` and `dataset` from discovery.
#' @param geometry_resolution `"simplified"` or `"full"`.
#' @param call Calling environment.
#'
#' @return A tibble with `dataset`, `table`, `year`, `directory_label`,
#'   `label`.
#' @keywords internal
#' @noRd
plan_files <- function(years_by_table,
                       tables,
                       year_index,
                       geometry_resolution = "simplified",
                       call = rlang::caller_env()) {
  rows <- list()

  for (tbl in names(years_by_table)) {
    spec <- TABLE_SPEC[[tbl]]
    if (is.null(spec) || !isTRUE(spec$yearly)) next

    years <- sort(unique(as.integer(years_by_table[[tbl]])))
    if (length(years) == 0L) next

    idx <- match(years, year_index$year)
    available <- !is.na(idx)

    if (any(!available)) {
      # Not an error: asking for 2030 should narrow the answer, not kill the
      # query. The user is told which years exist.
      cli::cli_warn(c(
        "No published {.field {tbl}} data for
         year{?s} {.val {years[!available]}}.",
        i = "Available years: {min(year_index$year)}-{max(year_index$year)}."
      ))
    }

    years <- years[available]
    if (length(years) == 0L) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      dataset = year_index$dataset[match(years, year_index$year)],
      table = tbl,
      year = years,
      directory_label = spec$dir,
      label = paste0(years, ".parquet")
    )
  }

  # Non-yearly tables. `geometry` maps to one of two published files.
  static <- intersect(tables, c("adm2", "codebook", "geometry"))
  for (tbl in static) {
    key <- if (identical(tbl, "geometry")) {
      paste0("geometry_", geometry_resolution)
    } else {
      tbl
    }
    spec <- TABLE_SPEC[[key]]

    rows[[length(rows) + 1L]] <- tibble::tibble(
      dataset = spec$dataset,
      table = tbl,
      year = NA_integer_,
      directory_label = spec$dir,
      label = spec$label
    )
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      dataset = character(0), table = character(0), year = integer(0),
      directory_label = character(0), label = character(0)
    ))
  }

  dplyr::distinct(dplyr::bind_rows(rows))
}

#' Compute the years each table must be scanned for
#'
#' A direct consequence of the backend assigning rows by genesis year:
#'
#' * `storm_metadata`, `wind`, `zero_pairs`, `tracks` -- the genesis-year span
#'   only, with no plus-or-minus one. Adding a margin would pull in storms that
#'   cannot be in the answer.
#' * `precip` and `flood` -- the years spanned by the *actual computed windows*.
#'   Boundary crossing falls out of the arithmetic; nothing is hardcoded.
#' * `flood` with `flood_match = "overlap"` -- one additional leading year,
#'   because a flood already in progress when the storm arrived may have started
#'   in the previous calendar year.
#'
#' @param storm_years Integer years of the genesis span.
#' @param window_years Integer years spanned by the computed precip/flood
#'   windows, or `NULL` before the windows are known.
#' @param tables Normalised table names.
#' @param flood_match `"start"` or `"overlap"`.
#' @param pop_years Integer years the returned data spans.
#'
#' @return A named list of integer vectors.
#' @keywords internal
#' @noRd
years_for_tables <- function(storm_years,
                             window_years = NULL,
                             tables = character(0),
                             flood_match = "start",
                             pop_years = NULL) {
  out <- list()

  if ("storm_metadata" %in% tables || any(tables %in% STORM_LINKED_TABLES)) {
    out$storm_metadata <- storm_years
  }
  if ("wind" %in% tables) out$wind <- storm_years
  if ("tracks" %in% tables) out$tracks <- storm_years
  if (needs_zero_pairs(tables)) out$zero_pairs <- storm_years

  if ("precip" %in% tables && !is.null(window_years)) {
    out$precip <- window_years$precip
  }

  if ("flood" %in% tables && !is.null(window_years)) {
    flood_years <- window_years$flood
    if (identical(flood_match, "overlap") && length(flood_years) > 0L) {
      flood_years <- seq.int(min(flood_years) - 1L, max(flood_years))
    }
    out$flood <- flood_years
  }

  if ("pop" %in% tables && !is.null(pop_years)) out$pop <- pop_years

  lapply(compact(out), function(y) sort(unique(as.integer(y))))
}

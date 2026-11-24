# ---------------------------------------------------------------------------
# Reading published files.
#
# arrow::open_dataset() cannot read an https:// URL. Arrow's filesystem layer
# supports local paths and S3/GCS URIs only; there is no generic HTTP
# filesystem. The working pattern is therefore:
#
#   path <- dv_get_file(id, dest = <file>)       # streamed to disk
#   tbl <- read_parquet_file(path, cols)         # Arrow Table
#   out <- tibble::as_tibble(tbl)                # then filtered in R
#
# Column projection saves memory and R-side conversion time. It does NOT save
# bandwidth: Dataverse serves whole files regardless.
# ---------------------------------------------------------------------------

#' Read a parquet file into an Arrow Table
#'
#' The file is read rather than memory-mapped: a download that is not cached
#' is deleted as soon as it has been read, and Windows cannot delete a file
#' that is still mapped.
#'
#' @param path Parquet file.
#' @param col_select Character vector of columns to project, or `NULL` for all.
#'   Columns absent from the file are dropped with a warning rather than an
#'   error, so a schema addition upstream never breaks a query.
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return An `arrow::Table`.
#' @keywords internal
#' @noRd
read_parquet_file <- function(path, col_select = NULL, label = "file",
                              call = rlang::caller_env()) {
  tbl <- tryCatch(
    arrow::read_parquet(path, as_data_frame = FALSE, mmap = FALSE),
    error = function(e) {
      cli::cli_abort(
        c("Could not read {.path {label}} as parquet.",
          x = "{conditionMessage(e)}",
          i = "The download may be corrupt. If a cache is enabled, clearing it
               with {.code gtropic_cache_clear()} will force a fresh copy."
        ),
        call = call
      )
    }
  )

  if (!is.null(col_select)) {
    present <- intersect(col_select, names(tbl))
    missing <- setdiff(col_select, names(tbl))
    if (length(missing) > 0L) {
      cli::cli_warn(
        c("Column{?s} {.field {missing}} not found in {.path {label}}.",
          i = "Continuing without {?it/them}; the published schema may have
               changed."
        )
      )
    }
    if (length(present) > 0L) tbl <- tbl[, present, drop = FALSE]
  }

  tbl
}

#' Read a JSON file into a nested list
#'
#' @param path JSON file.
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return A nested list.
#' @keywords internal
#' @noRd
read_json_file <- function(path, label = "file", call = rlang::caller_env()) {
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        c("Could not parse {.path {label}} as JSON.", x = "{conditionMessage(e)}"),
        call = call
      )
    }
  )
}

#' Read a GeoParquet file into an sf object
#'
#' `sf` and `sfarrow` are Suggests, not Imports: only the `polygon` filter and
#' the `geometry` table need them. Users who do not need geometry can omit it
#' from `tables`, avoiding a hard dependency on a heavy geospatial toolchain.
#'
#' @param path GeoParquet file.
#' @param label Path used in messages.
#' @param call Calling environment.
#'
#' @return An `sf` object in EPSG:4326.
#' @keywords internal
#' @noRd
read_geoparquet_file <- function(path, label = "file",
                                 call = rlang::caller_env()) {
  check_installed_geo(call = call)

  geometry <- tryCatch(
    sfarrow::st_read_parquet(path),
    error = function(e) {
      cli::cli_abort(
        c("Could not read {.path {label}} as GeoParquet.",
          x = "{conditionMessage(e)}"
        ),
        call = call
      )
    }
  )

  # The published ADM2 coordinates are WGS 84. Some sfarrow/Arrow version
  # combinations do not restore the GeoParquet CRS metadata, so enforce the
  # reader's EPSG:4326 contract at this single I/O boundary.
  crs <- sf::st_crs(geometry)
  if (is.na(crs)) {
    geometry <- sf::st_set_crs(geometry, 4326)
  } else if (!isTRUE(crs == sf::st_crs(4326))) {
    geometry <- sf::st_transform(geometry, 4326)
  }
  geometry
}

#' Normalize date-only columns from published tables
#'
#' Parquet source schemas currently mix ISO date strings and Arrow date values.
#' Normalizing at the I/O boundary gives identically named join keys one stable
#' R type without altering datetime strings that carry meaningful UTC offsets.
#'
#' @param x A data frame.
#' @param label Table or path used in error messages.
#' @param call Calling environment.
#'
#' @return `x` with recognized date-only columns converted to `Date`.
#' @keywords internal
#' @noRd
normalize_table_types <- function(x, label = "table",
                                  call = rlang::caller_env()) {
  date_cols <- intersect(
    c(
      "STORM_GENESIS_DATE_UTC",
      "LOCAL_DATE_STORM_CLOSEST",
      "LOCAL_DATE",
      "FLOOD_START_DATE",
      "FLOOD_END_DATE",
      "PRECIP_WINDOW_START",
      "PRECIP_WINDOW_END",
      "FLOOD_WINDOW_START",
      "FLOOD_WINDOW_END"
    ),
    names(x)
  )

  for (column in date_cols) {
    x[[column]] <- parse_published_date(
      x[[column]],
      column = column, label = label, call = call
    )
  }
  x
}

#' Parse one published date-only column
#'
#' @param x A `Date` or character vector.
#' @param column Column name.
#' @param label Table or path used in error messages.
#' @param call Calling environment.
#'
#' @return A `Date` vector.
#' @keywords internal
#' @noRd
parse_published_date <- function(x, column, label = "table",
                                 call = rlang::caller_env()) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (!is.character(x)) {
    cli::cli_abort(
      c(
        "Could not parse {.field {column}} in {.path {label}} as a date.",
        x = "Expected ISO dates stored as character values or Arrow dates, not {.cls {class(x)}}."
      ),
      call = call
    )
  }

  valid_shape <- is.na(x) | grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  invalid <- !valid_shape | (!is.na(x) & is.na(parsed))
  if (any(invalid)) {
    bad <- unique(x[invalid])
    cli::cli_abort(
      c(
        "Could not parse {.field {column}} in {.path {label}} as a date.",
        x = "Invalid value{?s}: {.val {utils::head(bad, 3)}}.",
        i = "Expected complete ISO dates in {.val YYYY-MM-DD} form."
      ),
      call = call
    )
  }

  parsed
}

#' Which optional geospatial packages are not installed?
#'
#' @return Character vector of missing package names (empty when all are
#'   installed).
#' @keywords internal
#' @noRd
geo_packages_missing <- function() {
  pkgs <- c("sf", "sfarrow")
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

#' Default tables for a query that did not name any
#'
#' Every table, except `"geometry"` when the packages it needs are not
#' installed: a first query should work with only the hard dependencies. Says
#' so once per session, unless `quiet`.
#'
#' @param quiet Suppress the message.
#' @return Character vector of table names.
#' @keywords internal
#' @noRd
default_tables <- function(quiet = FALSE) {
  missing <- geo_packages_missing()
  if (length(missing) == 0L) {
    return(VALID_TABLES)
  }
  if (!isTRUE(quiet)) {
    cli::cli_inform(
      c(
        "i" = "Leaving out the {.val geometry} table: it needs the
               {.pkg {missing}} package{?s}.",
        " " = "Install {cli::qty(length(missing))}{?it/them} to include ADM2
               boundaries by default, or name {.val geometry} in {.arg tables}
               to require {?it/them}."
      ),
      .frequency = "once",
      .frequency_id = "gtropic_default_tables_geometry"
    )
  }
  setdiff(VALID_TABLES, "geometry")
}

#' Require the optional geospatial packages
#'
#' @param what Description of the feature requiring them.
#' @param call Calling environment.
#'
#' @return Invisibly `TRUE`; aborts when the packages are absent.
#' @keywords internal
#' @noRd
check_installed_geo <- function(what = "this feature",
                                call = rlang::caller_env()) {
  missing <- geo_packages_missing()
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }

  cli::cli_abort(
    c("{what} requires the {.pkg {missing}} package{?s}.",
      i = 'Install with {.code install.packages(c({paste0(\'"\', missing, \'"\', collapse = ", ")}))}.',
      i = "Omit {.val geometry} from {.arg tables} if you do not need geospatial
           tooling."
    ),
    call = call
  )
}

#' Normalise a collected table for the result
#'
#' @param x A data frame.
#'
#' @return A tibble.
#' @keywords internal
#' @noRd
finalize_table <- function(x) {
  tibble::as_tibble(normalize_table_types(x))
}

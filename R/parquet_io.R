# ---------------------------------------------------------------------------
# Reading published files.
#
# arrow::open_dataset() cannot read an https:// URL. Arrow's filesystem layer
# supports local paths and S3/GCS URIs only; there is no generic HTTP
# filesystem. The working pattern is therefore:
#
#   path <- dv_get_file(id, dest = <file>)       # streamed to disk
#   tbl <- read_parquet_file(path, cols, keep)   # projected, filtered Table
#   out <- tibble::as_tibble(tbl)                # only the rows we need
#
# Only the requested columns are decoded, and rows are filtered on the Arrow
# side, one row group at a time, before anything is converted to R. A query
# for one storm in one country therefore materialises its own rows, not the
# global year-file. None of this saves bandwidth: Dataverse serves whole
# files regardless.
# ---------------------------------------------------------------------------

#' Read a parquet file into an Arrow Table
#'
#' The file is read rather than memory-mapped: a download that is not cached
#' is deleted as soon as it has been read, and Windows cannot delete a file
#' that is still mapped.
#'
#' Rows are filtered with Table-level kernels, row group by row group in file
#' order, so the result has the same rows in the same order as filtering the
#' whole table in R would give (invariant G6). A multi-threaded Acero query
#' would not guarantee the order.
#'
#' @param path Parquet file.
#' @param col_select Character vector of columns to project, or `NULL` for all.
#'   Columns absent from the file are dropped with a warning rather than an
#'   error, so a schema addition upstream never breaks a query.
#' @param keep Named list of row filters, `list(<column> = <values>)`, applied
#'   as `column %in% values`. `NULL` entries, and columns absent from the
#'   file, are skipped silently. Use only for identifier columns whose type is
#'   the same in every year-file (`STORM_ID`, `ADM2_ID`).
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return An `arrow::Table`.
#' @keywords internal
#' @noRd
read_parquet_file <- function(path, col_select = NULL, keep = NULL,
                              label = "file", call = rlang::caller_env()) {
  tbl <- tryCatch(
    read_parquet_table(path, col_select = col_select, keep = keep),
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
    missing <- setdiff(col_select, names(tbl))
    if (length(missing) > 0L) {
      n <- length(missing)
      cli::cli_warn(
        c("{cli::qty(n)}Column{?s} {.field {missing}} not found in {.path {label}}.",
          i = "{cli::qty(n)}Continuing without {?it/them}; the published
               schema may have changed."
        )
      )
    }
    present <- intersect(col_select, names(tbl))
    if (length(present) > 0L) tbl <- tbl[, present, drop = FALSE]
  }

  tbl
}

#' Read the projected, filtered columns of a parquet file
#'
#' @inheritParams read_parquet_file
#' @return An `arrow::Table` holding the projection (in file order) plus the
#'   filter columns; `read_parquet_file()` puts the projection in order.
#' @keywords internal
#' @noRd
read_parquet_table <- function(path, col_select = NULL, keep = NULL) {
  file <- arrow::ReadableFile$create(path)
  on.exit(file$close(), add = TRUE)
  reader <- arrow::ParquetFileReader$create(file)
  columns <- names(reader$GetSchema())

  present <- intersect(col_select, columns)
  keep <- compact(keep[intersect(names(keep), columns)])
  # Decode the projection plus the filter columns, and nothing else.
  read_cols <- if (length(present) > 0L) union(present, names(keep)) else columns
  indices <- match(read_cols, columns) - 1L

  if (length(keep) == 0L || reader$num_row_groups == 0L) {
    return(reader$ReadTable(indices))
  }
  # One row group at a time, so the whole file is never decoded at once.
  parts <- lapply(seq_len(reader$num_row_groups) - 1L, function(i) {
    arrow_keep_rows(reader$ReadRowGroup(i, indices), keep)
  })
  do.call(arrow::concat_tables, parts)
}

#' Keep the rows of an Arrow Table whose identifiers are in a set
#'
#' `is_in` with `skip_nulls = FALSE` has the semantics of R's `%in%`: a
#' missing value matches only when `values` contains `NA`.
#'
#' @param tbl An `arrow::Table`.
#' @param keep Named list of `column = values`, every column present in `tbl`.
#'
#' @return The filtered `arrow::Table`, rows in their original order.
#' @keywords internal
#' @noRd
arrow_keep_rows <- function(tbl, keep) {
  for (column in names(keep)) {
    values <- unique(keep[[column]])
    value_set <- tryCatch(
      arrow::Array$create(values, type = tbl$schema[[column]]$type),
      # e.g. a dictionary column: let the kernel cast the plain values.
      error = function(e) arrow::Array$create(values)
    )
    mask <- arrow::call_function("is_in", tbl[[column]],
      options = list(value_set = value_set, skip_nulls = FALSE)
    )
    tbl <- tbl$Filter(mask)
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

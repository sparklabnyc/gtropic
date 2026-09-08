# ---------------------------------------------------------------------------
# Reading published files.
#
# arrow::open_dataset() cannot read an https:// URL. Arrow's filesystem layer
# supports local paths and S3/GCS URIs only; there is no generic HTTP
# filesystem. The working pattern is therefore:
#
#   raw <- dv_get_file(id)                       # raw vector, nothing on disk
#   tbl <- read_parquet_raw(raw, cols)           # Arrow Table
#   out <- tbl |> filter(...) |> collect()       # materialise only what we need
#
# Column projection saves memory and R-side conversion time. It does NOT save
# bandwidth: Dataverse serves whole files regardless.
# ---------------------------------------------------------------------------

#' Read parquet bytes into an Arrow Table
#'
#' @param raw Raw vector of parquet bytes.
#' @param col_select Character vector of columns to project, or `NULL` for all.
#'   Columns absent from the file are dropped with a warning rather than an
#'   error, so a schema addition upstream never breaks a query.
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return An `arrow::Table`.
#' @keywords internal
#' @noRd
read_parquet_raw <- function(raw, col_select = NULL, label = "file",
                             call = rlang::caller_env()) {
  tbl <- tryCatch(
    arrow::read_parquet(raw, as_data_frame = FALSE),
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

#' Read JSON bytes into a nested list
#'
#' @param raw Raw vector.
#' @param label Path used in error messages.
#' @param call Calling environment.
#'
#' @return A nested list.
#' @keywords internal
#' @noRd
read_json_raw <- function(raw, label = "file", call = rlang::caller_env()) {
  tryCatch(
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        c("Could not parse {.path {label}} as JSON.", x = "{conditionMessage(e)}"),
        call = call
      )
    }
  )
}

#' Read GeoParquet bytes into an sf object
#'
#' `sf` and `sfarrow` are Suggests, not Imports: only the `polygon` filter and
#' the `geometry` table need them, and hard-depending would push a heavy
#' geospatial toolchain onto every user of a package that is mostly about
#' tabular exposure data.
#'
#' @param raw Raw vector of GeoParquet bytes.
#' @param label Path used in messages.
#' @param call Calling environment.
#'
#' @return An `sf` object in EPSG:4326.
#' @keywords internal
#' @noRd
read_geoparquet_raw <- function(raw, label = "file",
                                call = rlang::caller_env()) {
  check_installed_geo(call = call)

  # sfarrow reads from a path, so the bytes touch disk briefly. This is a
  # tempfile, never the cache and never the installation directory.
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)
  writeBin(raw, tf)

  tryCatch(
    sfarrow::st_read_parquet(tf),
    error = function(e) {
      cli::cli_abort(
        c("Could not read {.path {label}} as GeoParquet.",
          x = "{conditionMessage(e)}"
        ),
        call = call
      )
    }
  )
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
  missing <- c("sf", "sfarrow")[
    !vapply(c("sf", "sfarrow"), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }

  cli::cli_abort(
    c("{what} requires the {.pkg {missing}} package{?s}.",
      i = 'Install with {.code install.packages(c({paste0(\'"\', missing, \'"\', collapse = ", ")}))}.',
      i = "These are optional because most gtropic queries need no geospatial
           tooling."
    ),
    call = call
  )
}

#' Materialise an Arrow Table according to the `as` argument
#'
#' @param x An Arrow Table or data frame.
#' @param as `"tibble"` or `"arrow"`.
#'
#' @return A tibble, or the Arrow object unchanged.
#' @keywords internal
#' @noRd
finalize_table <- function(x, as = c("tibble", "arrow")) {
  as <- match.arg(as)
  if (identical(as, "arrow")) {
    return(x)
  }
  if (inherits(x, c("Table", "ArrowTabular", "arrow_dplyr_query", "Dataset"))) {
    return(tibble::as_tibble(dplyr::collect(x)))
  }
  tibble::as_tibble(x)
}

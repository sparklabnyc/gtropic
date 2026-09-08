#' Construct a gtropic_data object
#'
#' Deliberately a plain named list underneath, so `res$precip` and
#' `res[["links"]]` work exactly as users expect without any method dispatch.
#' Everything else travels as attributes.
#'
#' @param tables Named list of tibbles or Arrow objects.
#' @param manifest A `gtropic_manifest`.
#' @param query The matched call.
#' @param filters_resolved List of resolved filter state.
#' @param warnings Structured record of warnings raised.
#'
#' @return An object of class `gtropic_data`.
#' @keywords internal
#' @noRd
new_gtropic_data <- function(tables,
                             manifest,
                             query = NULL,
                             filters_resolved = list(),
                             warnings = list()) {
  structure(
    tables,
    manifest = manifest,
    query = query,
    filters_resolved = filters_resolved,
    warnings = warnings,
    retrieved_at = Sys.time(),
    gtropic_version = as.character(utils::packageVersion("gtropic")),
    class = c("gtropic_data", "list")
  )
}

#' Extract metadata from a G-TROPIC result
#'
#' @description
#' These accessor functions retrieve provenance, resolved query settings, and
#' recorded diagnostic information from an object returned by [gtropic_data()].
#'
#' `gtropic_manifest()` identifies the source datasets and files used in the
#' retrieval, including their versions and checksums. Supplying the manifest to
#' a later [gtropic_data()] call requests those same source files, provided they
#' remain available.
#'
#' `gtropic_filters()` reports the filters as resolved by the package, including
#' selected ADM2 and storm identifiers, genesis-date bounds, years represented
#' by the selected storms, and precipitation and flood window settings. These
#' values support transparent reporting and quality assurance in downstream
#' analyses.
#'
#' `gtropic_warnings()` returns diagnostic details retained with the result.
#' This can include complete values that were abbreviated in a console warning,
#' such as ADM2 units whose polygon membership varied over time. It is not a log
#' of every warning that may have been emitted during retrieval.
#'
#' @param x A `gtropic_data` object.
#'
#' @return `gtropic_manifest()` returns an object of class `gtropic_manifest`.
#'   `gtropic_filters()` returns a named list of resolved query settings.
#'   `gtropic_warnings()` returns a named list of recorded diagnostics, which is
#'   empty when none were retained.
#'
#' @examples
#' \donttest{
#' res <- gtropic_data(WHO_ENTITY = "Philippines", date_range = c(2015, 2016))
#' manifest <- gtropic_manifest(res)
#' filters <- gtropic_filters(res)
#' diagnostics <- gtropic_warnings(res)
#' }
#'
#' @seealso [gtropic_data()]
#' @export
gtropic_manifest <- function(x) {
  stopifnot(inherits(x, "gtropic_data"))
  attr(x, "manifest")
}

#' @rdname gtropic_manifest
#' @export
gtropic_filters <- function(x) {
  stopifnot(inherits(x, "gtropic_data"))
  attr(x, "filters_resolved")
}

#' @rdname gtropic_manifest
#' @export
gtropic_warnings <- function(x) {
  stopifnot(inherits(x, "gtropic_data"))
  attr(x, "warnings")
}

#' Print a G-TROPIC data result
#'
#' Displays a concise overview of a G-TROPIC retrieval, including source dataset
#' versions, resolved genesis period, numbers of storms and ADM2 units, and the
#' dimensions of each returned table. Printing does not modify or materialize
#' the result.
#'
#' @param x A `gtropic_data` object.
#' @param ... Additional arguments passed to the print method. Currently
#'   ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#' }
#'
#' @export
print.gtropic_data <- function(x, ...) {
  mf <- attr(x, "manifest")
  fl <- attr(x, "filters_resolved")

  cli::cli_h1("G-TROPIC data")

  cli::cli_bullets(c(
    "*" = "Historical dataset: version {.val {mf$historical_version}}",
    "*" = if (!is.na(mf$current_doi)) {
      "Current-year dataset: version {.val {mf$current_version}}"
    },
    "*" = if (!is.null(fl$date_start)) {
      "Genesis dates: {.val {format(fl$date_start)}} to
       {.val {format(fl$date_end)}}"
    } else {
      "Genesis dates: all available"
    },
    "*" = "Storms: {length(fl$storm_ids %||% character(0))}",
    "*" = "ADM2 units: {length(fl$adm2_ids %||% character(0))}"
  ))

  cli::cli_h3("Tables")
  for (nm in names(x)) {
    cli::cli_li("{.field {nm}}: {table_dim_label(x[[nm]])}")
  }

  n_warn <- length(attr(x, "warnings"))
  cli::cli_text("")
  cli::cli_alert_info(
    "Use {.code summary()} for detail{if (n_warn > 0)
     glue::glue('; {n_warn} warning(s) recorded, see gtropic_warnings()')}."
  )

  invisible(x)
}

#' Summarize a G-TROPIC data result
#'
#' Displays an extended overview of a G-TROPIC retrieval. The output includes
#' dataset provenance, retrieval time and size, returned table dimensions,
#' resolved geographic and storm filters, hazard-specific temporal and spatial
#' windows, and any diagnostics retained with the result.
#'
#' @param object A `gtropic_data` object.
#' @param ... Additional arguments passed to the summary method. Currently
#'   ignored.
#'
#' @return `object`, invisibly.
#'
#' @examples
#' \donttest{
#' res <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#' summary(res)
#' }
#'
#' @export
summary.gtropic_data <- function(object, ...) {
  mf <- attr(object, "manifest")
  fl <- attr(object, "filters_resolved")
  wn <- attr(object, "warnings")

  cli::cli_h1("G-TROPIC data summary")

  cli::cli_h2("Sources")
  cli::cli_bullets(c(
    "*" = "Server: {.val {mf$server}}",
    "*" = "Historical: {.val {mf$historical_doi}} v{mf$historical_version}",
    "*" = if (!is.na(mf$current_doi)) {
      "Current-year: {.val {mf$current_doi}} v{mf$current_version}"
    },
    "*" = "Files retrieved: {nrow(mf$files)}
           ({format_bytes(sum(mf$files$size_bytes, na.rm = TRUE))})",
    "*" = "Retrieved at: {format(attr(object, 'retrieved_at'), usetz = TRUE)}"
  ))

  cli::cli_h2("Tables")
  for (nm in names(object)) {
    cli::cli_li("{.field {nm}}: {table_dim_label(object[[nm]])}")
  }

  cli::cli_h2("Resolved filters")
  cli::cli_bullets(c(
    "*" = "ADM2 units: {length(fl$adm2_ids %||% character(0))}",
    "*" = "Storms: {length(fl$storm_ids %||% character(0))}",
    "*" = if (!is.null(fl$date_start)) {
      "Genesis window: {format(fl$date_start)} to {format(fl$date_end)}"
    },
    "*" = "Precipitation window: -{fl$precip_days_before}/
           +{fl$precip_days_after} days within {fl$precip_radius_km} km",
    "*" = "Flood window: -{fl$flood_days_before}/+{fl$flood_days_after} days
           within {fl$flood_radius_km} km (match: {.val {fl$flood_match}})"
  ))

  if (length(wn) > 0L) {
    cli::cli_h2("Warnings")
    for (nm in names(wn)) {
      cli::cli_li("{.field {nm}}: {truncate_display(as.character(wn[[nm]]), 5)}")
    }
  }

  invisible(object)
}

#' Describe a table's dimensions for printing
#'
#' Arrow objects are not materialised just to print a row count; for a lazy
#' query we say so rather than triggering the computation the user was trying to
#' defer.
#'
#' @param x A table.
#' @return A single string.
#' @keywords internal
#' @noRd
table_dim_label <- function(x) {
  if (is.null(x)) {
    return("empty")
  }
  if (inherits(x, "list") && !is.data.frame(x)) {
    return(glue::glue("{length(x)} entries (list)"))
  }
  if (inherits(x, "arrow_dplyr_query") || inherits(x, "Dataset")) {
    return("lazy Arrow query (not yet collected)")
  }
  if (inherits(x, c("Table", "ArrowTabular"))) {
    return(glue::glue("{nrow(x)} x {ncol(x)} (Arrow Table)"))
  }
  glue::glue("{nrow(x)} rows x {ncol(x)} columns")
}

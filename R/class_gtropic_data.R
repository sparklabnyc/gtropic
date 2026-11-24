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
#' @param availability Structured source availability and coverage metadata.
#' @param retrieved_at Manifest-level source acquisition time.
#'
#' @return An object of class `gtropic_data`.
#' @keywords internal
#' @noRd
new_gtropic_data <- function(tables,
                             manifest,
                              query = NULL,
                              filters_resolved = list(),
                              warnings = list(),
                              availability = NULL,
                              retrieved_at = Sys.time()) {
  structure(
    tables,
    manifest = manifest,
    query = query,
    filters_resolved = filters_resolved,
    warnings = warnings,
    availability = availability,
    retrieved_at = retrieved_at,
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
#' by the selected storms, the wind radius, and precipitation and flood window
#' settings. These values support transparent reporting and quality assurance in
#' downstream analyses. Resolved storm identifiers are unique `STORM_ID` values,
#' the preferred keys for reproducible joins and grouping.
#'
#' `gtropic_diagnostics()` returns diagnostic details retained with the result.
#' This can include complete values that were abbreviated in a console warning,
#' such as ADM2 units whose polygon membership varied over time. It is not a log
#' of every warning that may have been emitted during retrieval.
#'
#' `gtropic_availability()` returns structured source update schedules, known
#' coverage, and the requested time range used for availability diagnostics.
#'
#' @param x A `gtropic_data` object.
#'
#' @return `gtropic_manifest()` returns an object of class `gtropic_manifest`.
#'   `gtropic_filters()` returns a named `gtropic_filters` list of resolved
#'   query settings. List elements retain their complete values; printing shows
#'   a compact preview.
#'   `gtropic_diagnostics()` returns a named list of recorded diagnostics,
#'   which is empty when none were retained. `gtropic_availability()` returns a
#'   named list of source availability and coverage metadata.
#'
#' @examples
#' \donttest{
#' res <- gtropic_data(WHO_ENTITY = "Philippines", date_range = c(2015, 2016))
#' manifest <- gtropic_manifest(res)
#' filters <- gtropic_filters(res)
#' diagnostics <- gtropic_diagnostics(res)
#' }
#'
#' @seealso [gtropic_data()]
#' @export
gtropic_manifest <- function(x) {
  check_gtropic_data(x)
  attr(x, "manifest")
}

#' @rdname gtropic_manifest
#' @export
gtropic_filters <- function(x) {
  check_gtropic_data(x)
  structure(attr(x, "filters_resolved"), class = c("gtropic_filters", "list"))
}

#' @rdname gtropic_manifest
#' @export
gtropic_diagnostics <- function(x) {
  check_gtropic_data(x)
  attr(x, "warnings")
}

#' @rdname gtropic_manifest
#' @export
gtropic_availability <- function(x) {
  check_gtropic_data(x)
  attr(x, "availability")
}

#' Print resolved G-TROPIC filters
#'
#' Displays each resolved filter with a compact value preview. Long vectors,
#' such as selected ADM2 identifiers, show at most `n` values and state how many
#' values were omitted. The complete values remain available through ordinary
#' list access.
#'
#' @param x A `gtropic_filters` object returned by [gtropic_filters()].
#' @param n Maximum number of values to display for each filter.
#' @param ... Additional arguments passed to the print method. Currently
#'   ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' res <- gtropic_data(WHO_ENTITY = "Philippines", date_range = c(2015, 2016))
#' print(gtropic_filters(res))
#' }
#'
#' @export
print.gtropic_filters <- function(x, n = 10L, ...) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 1L || n != floor(n)) {
    cli::cli_abort("{.arg n} must be a positive whole number.")
  }
  n <- as.integer(n)

  cli::cli_h1("G-TROPIC resolved filters")
  if (length(x) == 0L) {
    cli::cli_text("No resolved filters.")
    return(invisible(x))
  }

  filter_names <- names(x)
  for (i in seq_along(x)) {
    value <- x[[i]]
    nm <- filter_names[[i]]
    if (is.null(value)) {
      cli::cli_li("{.field {nm}}: NULL")
      next
    }

    n_values <- length(value)
    value_label <- if (n_values == 1L) "value" else "values"
    preview <- format_filter_preview(value, n)
    n_omitted <- max(n_values - n, 0L)
    omitted <- if (n_omitted > 0L) {
      omitted_label <- if (n_omitted == 1L) "value" else "values"
      paste0(" (", n_omitted, " ", omitted_label, " omitted)")
    } else {
      ""
    }
    cli::cli_li("{.field {nm}} ({n_values} {value_label}): {preview}{omitted}")
  }

  invisible(x)
}

#' Format a resolved filter value for compact printing
#' @param x Filter value.
#' @param n Maximum number of values to include.
#' @return A single character string.
#' @keywords internal
#' @noRd
format_filter_preview <- function(x, n) {
  if (length(x) == 0L) {
    return("<empty>")
  }

  values <- if (is.character(x)) {
    ifelse(is.na(x), "NA", encodeString(x, quote = "\""))
  } else {
    out <- format(x, trim = TRUE, justify = "none")
    out[is.na(x)] <- "NA"
    out
  }
  paste0("[", paste(utils::head(values, n), collapse = ", "), "]")
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
  datasets <- manifest_datasets(mf)

  cli::cli_h1("G-TROPIC data")

  cli_dataset_bullets(datasets)
  cli::cli_bullets(c(
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
     glue::glue('; {n_warn} warning(s) recorded, see gtropic_diagnostics()')}."
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
  datasets <- manifest_datasets(mf)

  cli::cli_h1("G-TROPIC data summary")

  cli::cli_h2("Sources")
  cli::cli_bullets(c("*" = "Server: {.val {mf$server}}"))
  cli_dataset_bullets(datasets, compact = TRUE)
  cli::cli_bullets(c(
    "*" = "Files retrieved: {nrow(mf$files)}
           ({format_bytes(sum(mf$files$size_bytes, na.rm = TRUE))})",
    "*" = "Retrieved at: {format(attr(object, 'retrieved_at'), usetz = TRUE)}"
  ))

  cli::cli_h2("Tables")
  for (nm in names(object)) {
    cli::cli_li("{.field {nm}}: {table_dim_label(object[[nm]])}")
  }

  cli::cli_h2("Resolved filters")
  wind_radius_km <- fl$wind_radius_km %||% Inf
  wind_radius <- if (is.infinite(wind_radius_km)) {
    "unbounded"
  } else {
    paste0(wind_radius_km, " km")
  }
  cli::cli_bullets(c(
    "*" = "ADM2 units: {length(fl$adm2_ids %||% character(0))}",
    "*" = "Storms: {length(fl$storm_ids %||% character(0))}",
    "*" = if (!is.null(fl$date_start)) {
      "Genesis window: {format(fl$date_start)} to {format(fl$date_end)}"
    },
    "*" = "Wind radius: {wind_radius}",
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

#' Print one bullet per source dataset
#'
#' The DOI and version come from the server or from a user-supplied manifest.
#' They are interpolated as data into a literal template; pasting them into a
#' `cli` template would evaluate any `{...}` they contain. (`cli_bullets_raw()`
#' is not used because cli 3.6.6 prints braces in its text doubled.)
#'
#' @param datasets Output of `manifest_datasets()`.
#' @param compact Use the short `"v1.0"` form instead of `"(version 1.0)"`.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
#' @noRd
cli_dataset_bullets <- function(datasets, compact = FALSE) {
  for (i in seq_len(nrow(datasets))) {
    role <- datasets$source_role[i]
    doi <- datasets$dataset_doi[i]
    ver <- datasets$dataset_version[i]
    if (compact) {
      cli::cli_bullets(c("*" = "{role} dataset: {doi} v{ver}"))
    } else {
      cli::cli_bullets(c("*" = "{role} dataset: {doi} (version {ver})"))
    }
  }
  invisible(NULL)
}

#' Return datasets actually represented in a manifest
#' @keywords internal
#' @noRd
manifest_datasets <- function(manifest) {
  datasets <- tibble::as_tibble(manifest$datasets)
  datasets$source_role <- manifest_source_role(datasets$source_role)
  datasets$source_role[is.na(datasets$source_role)] <- "Dataset"
  manifest_order_datasets(datasets)
}

#' Describe a table's dimensions for printing
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
  glue::glue("{nrow(x)} rows x {ncol(x)} columns")
}

#' Abort unless `x` is a `gtropic_data` object
#'
#' @param x Object to check.
#' @param arg Argument name, for the error message.
#' @param call Calling environment for error reporting.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_gtropic_data <- function(x, arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (!inherits(x, "gtropic_data")) {
    cli::cli_abort(
      c("{.arg {arg}} must be a {.cls gtropic_data} object returned by
         {.fn gtropic_data}.",
        x = "Got {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  invisible(x)
}

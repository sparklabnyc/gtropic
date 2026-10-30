# ---------------------------------------------------------------------------
# The version manifest.
#
# A single `version` argument cannot describe a query, because a query can span
# two datasets whose version numbers advance independently. The manifest records
# exactly which file, at which version, with which checksum, went into a result.
# Handing it back to gtropic_data() reproduces that result byte for byte.
# ---------------------------------------------------------------------------

#' Start an empty manifest
#'
#' @param discovery Output of `discover_datasets()`.
#'
#' @return An object of class `gtropic_manifest`.
#' @keywords internal
#' @noRd
manifest_new <- function(discovery) {
  structure(
    list(
      files = tibble::tibble(
        source_role = character(0),
        dataset_doi = character(0),
        dataset_version = character(0),
        directory_label = character(0),
        label = character(0),
        file_id = integer(0),
        checksum = character(0),
        checksum_type = character(0),
        size_bytes = numeric(0),
        downloaded_at = as.POSIXct(character(0))
      ),
      server = discovery$server,
      datasets = tibble::tibble(
        source_role = character(0),
        dataset_doi = character(0),
        dataset_version = character(0)
      ),
      resolved_query = NULL,
      availability = NULL,
      retrieved_at = as.POSIXct(NA),
      schema_version = 1L,
      gtropic_version = as.character(utils::packageVersion("gtropic"))
    ),
    class = "gtropic_manifest"
  )
}

#' Record a retrieved file in the manifest
#'
#' @param manifest A `gtropic_manifest`.
#' @param doi DOI the file came from.
#' @param version Concrete version label (never `":latest"`).
#' @param row One-row listing tibble from `dv_resolve_file()`.
#'
#' @return The updated manifest.
#' @keywords internal
#' @noRd
manifest_add <- function(manifest, doi, version, row, downloaded_at,
                         source_role = NULL) {
  entry <- tibble::tibble(
    source_role = manifest_source_role(source_role),
    dataset_doi = doi,
    dataset_version = as.character(version),
    directory_label = row$directory_label,
    label = row$label,
    file_id = as.integer(row$file_id),
    checksum = row$checksum,
    checksum_type = row$checksum_type,
    size_bytes = as.numeric(row$size_bytes),
    downloaded_at = as.POSIXct(downloaded_at, origin = "1970-01-01", tz = "UTC")
  )

  manifest$files <- dplyr::distinct(
    dplyr::bind_rows(manifest$files, entry),
    .data$source_role, .data$dataset_doi, .data$dataset_version,
    .data$directory_label, .data$label,
    .keep_all = TRUE
  )

  manifest
}

#' Stamp a manifest as complete
#'
#' @param manifest A `gtropic_manifest`.
#' @return The finalised manifest.
#' @keywords internal
#' @noRd
manifest_finalize <- function(manifest, resolved_query = NULL,
                              availability = NULL) {
  manifest$datasets <- dplyr::distinct(
    manifest$files[, c("source_role", "dataset_doi", "dataset_version"), drop = FALSE]
  )
  manifest$datasets <- manifest_order_datasets(manifest$datasets)
  manifest$resolved_query <- resolved_query %||% manifest$resolved_query
  manifest$availability <- availability %||% manifest$availability

  # This is deliberately the latest *original file download*, rather than the
  # time this result was assembled. It remains stable when a cached result is
  # read again, while still stating when its complete source set was acquired.
  downloaded <- manifest$files$downloaded_at
  manifest$retrieved_at <- if (length(downloaded) > 0L && any(!is.na(downloaded))) {
    max(downloaded, na.rm = TRUE)
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  manifest
}

#' Convert an internal dataset key to its human-readable role
#' @param source_role Dataset key or display role.
#' @return A display role, or `NA` when no role is available.
#' @keywords internal
#' @noRd
manifest_source_role <- function(source_role) {
  if (is.null(source_role)) return(NA_character_)
  role <- tolower(as.character(source_role))
  out <- rep(NA_character_, length(role))
  out[role == "historical"] <- "Historical"
  out[role %in% c("current", "current-year")] <- "Current-year"
  out
}

#' Order source datasets consistently for human-facing output
#' @keywords internal
#' @noRd
manifest_order_datasets <- function(datasets) {
  if (nrow(datasets) < 2L || !"source_role" %in% names(datasets)) {
    return(datasets)
  }
  order_key <- match(datasets$source_role, c("Historical", "Current-year"))
  datasets[order(is.na(order_key), order_key, seq_len(nrow(datasets))), , drop = FALSE]
}

#' Validate a user-supplied manifest
#'
#' @param manifest Object passed to the `manifest` argument.
#' @param call Calling environment.
#'
#' @return The manifest, invisibly.
#' @keywords internal
#' @noRd
manifest_validate <- function(manifest, call = rlang::caller_env()) {
  if (!inherits(manifest, "gtropic_manifest")) {
    cli::cli_abort(
      c("{.arg manifest} must be a manifest produced by
         {.code gtropic_manifest()}.",
        x = "Got {.cls {class(manifest)[1]}}."
      ),
      call = call
    )
  }

  required <- c(
    "source_role", "dataset_doi", "dataset_version", "directory_label",
    "label", "file_id", "checksum", "checksum_type", "size_bytes"
  )
  missing <- setdiff(required, names(manifest$files))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg manifest} is missing required source-file column{?s} {.field {missing}}.",
      call = call
    )
  }

  dataset_required <- c("source_role", "dataset_doi", "dataset_version")
  missing_datasets <- setdiff(dataset_required, names(manifest$datasets))
  if (length(missing_datasets) > 0L) {
    cli::cli_abort(
      "{.arg manifest} is missing required dataset column{?s} {.field {missing_datasets}}.",
      call = call
    )
  }

  if (nrow(manifest$files) == 0L) {
    cli::cli_abort("{.arg manifest} records no files.", call = call)
  }

  if (is.null(manifest$resolved_query)) {
    cli::cli_abort(
      "{.arg manifest} does not contain a canonical resolved query.",
      call = call
    )
  }

  query_required <- manifest_query_fields()
  missing_query <- setdiff(query_required, names(manifest$resolved_query))
  if (length(missing_query) > 0L) {
    cli::cli_abort(
      c("{.arg manifest} has an incomplete canonical query.",
        x = "Missing field{?s}: {.field {missing_query}}."
      ),
      call = call
    )
  }

  invisible(manifest)
}

#' Fields that define a data-shaped G-TROPIC query
#' @keywords internal
#' @noRd
manifest_query_fields <- function() {
  c(
    "geo_filters", "storm_filters", "storm_name_years", "wind_radius_km",
    "polygon", "reproject", "date_range", "date_list", "date_format",
    "tables", "precip_days_before", "precip_days_after", "precip_radius_km",
    "flood_days_before", "flood_days_after", "flood_radius_km", "flood_match",
    "geometry_resolution", "resolved_adm2_ids", "resolved_storm_ids",
    "storm_metadata_years"
  )
}

#' Look up a file in a manifest
#'
#' Used on the reproducibility path: instead of resolving against a live
#' listing, we resolve against what was recorded. A file that is no longer
#' available, or whose checksum has changed, is an error -- silently
#' substituting the current version would destroy the guarantee the manifest
#' exists to provide.
#'
#' @param manifest A `gtropic_manifest`.
#' @param directory_label,label File path components.
#'
#' @return A one-row tibble, or `NULL`.
#' @keywords internal
#' @noRd
manifest_lookup <- function(manifest, directory_label, label) {
  hit <- manifest$files[
    manifest$files$directory_label == directory_label &
      manifest$files$label == label, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) {
    return(NULL)
  }
  hit[1L, , drop = FALSE]
}

#' Print a G-TROPIC version manifest
#'
#' Displays the dataset server, DOI and version information for datasets actually
#' used, file count and total size, retrieval time, and package version recorded for a G-TROPIC
#' retrieval. A manifest can be supplied to `gtropic_data()` to request the same
#' published source files in a subsequent analysis.
#'
#' @param x A `gtropic_manifest` object returned by `gtropic_manifest()`.
#' @param ... Additional arguments passed to the print method. Currently
#'   ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' result <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = c(2005, 2005))
#' gtropic_manifest(result)
#' }
#'
#' @seealso `gtropic_manifest()`, `gtropic_data()`
#' @export
print.gtropic_manifest <- function(x, ...) {
  cli::cli_h1("gtropic version manifest")
  datasets <- manifest_datasets(x)
  datasets <- datasets[!is.na(datasets$dataset_doi) & nzchar(datasets$dataset_doi), , drop = FALSE]
  cli::cli_bullets(c(
    "*" = "Server: {.val {x$server}}",
    stats::setNames(
      paste0(datasets$source_role, " dataset: ", datasets$dataset_doi, " (version ",
        datasets$dataset_version, ")"),
      rep("*", nrow(datasets))
    ),
    "*" = "Files recorded: {nrow(x$files)}",
    "*" = "Total size: {format_bytes(sum(x$files$size_bytes, na.rm = TRUE))}",
    "*" = "Retrieved: {format(x$retrieved_at, usetz = TRUE)}",
    "*" = "Package version: {.val {x$gtropic_version}}"
  ))
  cli::cli_text("")
  cli::cli_alert_info(
    "Pass this object as {.arg manifest} to {.code gtropic_data()} to
     reproduce the same retrieval."
  )
  invisible(x)
}

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
        dataset_doi = character(0),
        dataset_version = character(0),
        directory_label = character(0),
        label = character(0),
        file_id = integer(0),
        md5 = character(0),
        size_bytes = numeric(0)
      ),
      server = discovery$server,
      historical_doi = discovery$historical$doi,
      historical_version = discovery$historical$version,
      current_doi = discovery$current$doi %||% NA_character_,
      current_version = discovery$current$version %||% NA_character_,
      retrieved_at = as.POSIXct(NA),
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
manifest_add <- function(manifest, doi, version, row) {
  entry <- tibble::tibble(
    dataset_doi = doi,
    dataset_version = as.character(version),
    directory_label = row$directory_label,
    label = row$label,
    file_id = as.integer(row$file_id),
    md5 = row$md5,
    size_bytes = as.numeric(row$size_bytes)
  )

  manifest$files <- dplyr::distinct(
    dplyr::bind_rows(manifest$files, entry),
    .data$dataset_doi, .data$dataset_version,
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
manifest_finalize <- function(manifest) {
  manifest$retrieved_at <- Sys.time()
  manifest
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
    "dataset_doi", "dataset_version", "directory_label", "label",
    "file_id", "md5", "size_bytes"
  )
  missing <- setdiff(required, names(manifest$files))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c("{.arg manifest} is missing required column{?s} {.field {missing}}.",
        i = "It may have been produced by an incompatible package version."
      ),
      call = call
    )
  }

  if (nrow(manifest$files) == 0L) {
    cli::cli_abort("{.arg manifest} records no files.", call = call)
  }

  invisible(manifest)
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
#' Displays the dataset server, DOI and version information, file count and
#' total size, retrieval time, and package version recorded for a G-TROPIC
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
  cli::cli_bullets(c(
    "*" = "Server: {.val {x$server}}",
    "*" = "Historical: {.val {x$historical_doi}} (version
           {.val {x$historical_version}})",
    "*" = if (!is.na(x$current_doi)) {
      "Current-year: {.val {x$current_doi}} (version {.val {x$current_version}})"
    },
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

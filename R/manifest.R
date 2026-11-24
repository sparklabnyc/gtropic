# ---------------------------------------------------------------------------
# The version manifest.
#
# A single `version` argument cannot describe a query, because a query can span
# two datasets whose version numbers advance independently. The manifest records
# exactly which file, at which version, with which checksum, went into a result.
# Handing it back to gtropic_data() reproduces that result byte for byte.
# ---------------------------------------------------------------------------

#' Format version of `gtropic_manifest` objects
#'
#' Bump when the structure changes. Manifests with any other version are
#' rejected (pre-1.0: no reading of older versions).
#' @keywords internal
#' @noRd
MANIFEST_SCHEMA_VERSION <- 1L

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
      schema_version = MANIFEST_SCHEMA_VERSION,
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
#' A manifest may come from someone else, so its contents are checked, not
#' only its shape: the server is a host name, file ids are positive whole
#' numbers, labels and directories are plain relative names, checksums are
#' hexadecimal digests, DOIs and versions look like Dataverse ones, and the
#' resolved query has the types a direct call would produce. Manifests made by
#' [gtropic_manifest()] always pass.
#'
#' @param manifest Object passed to the `manifest` argument.
#' @param call Calling environment.
#'
#' @return The manifest, with its server name normalised, invisibly.
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

  # Manifests created before geometry repair was configurable replay with the
  # current default (`make_valid = TRUE`). New manifests record it explicitly.
  query_required <- setdiff(manifest_query_fields(), "make_valid")
  missing_query <- setdiff(query_required, names(manifest$resolved_query))
  if (length(missing_query) > 0L) {
    cli::cli_abort(
      c("{.arg manifest} has an incomplete canonical query.",
        x = "Missing field{?s}: {.field {missing_query}}."
      ),
      call = call
    )
  }
  schema <- manifest$schema_version
  if (!is.numeric(schema) || length(schema) != 1L || is.na(schema) ||
    schema != MANIFEST_SCHEMA_VERSION) {
    cli::cli_abort(
      c("{.arg manifest} has an unsupported format version.",
        x = "Expected {.val {MANIFEST_SCHEMA_VERSION}}, got
             {.val {format(schema)}}.",
        i = "Re-create it with this version of gtropic."
      ),
      class = "gtropic_error_manifest",
      call = call
    )
  }

  manifest$server <- tryCatch(
    validate_server(manifest$server, arg = "manifest$server", call = NULL),
    error = function(e) {
      cli::cli_abort("{.arg manifest} names an invalid server.",
        parent = e, class = "gtropic_error_manifest", call = call
      )
    }
  )

  files <- manifest$files
  id <- files$file_id
  id_ok <- if (is.numeric(id)) {
    !is.na(id) & id > 0 & id == round(id)
  } else {
    rep(FALSE, length(id))
  }
  manifest_check_field(id, id_ok, "file_id", call)
  manifest_check_field(
    files$label, grepl(paste0("^", MANIFEST_PATH_SEGMENT, "$"), files$label),
    "label", call
  )
  manifest_check_field(
    files$directory_label,
    grepl(
      paste0("^(", MANIFEST_PATH_SEGMENT, "(/", MANIFEST_PATH_SEGMENT, ")*)?$"),
      files$directory_label
    ),
    "directory_label", call
  )
  checksum <- files$checksum
  manifest_check_field(
    checksum, is.na(checksum) | checksum == "" | is_hex_checksum(checksum),
    "checksum", call
  )
  doi <- c(files$dataset_doi, manifest$datasets$dataset_doi)
  manifest_check_field(
    doi, grepl("^doi:10\\.[0-9]{4,9}/[A-Za-z0-9._;()/:-]+$", doi),
    "dataset_doi", call
  )
  version <- c(files$dataset_version, manifest$datasets$dataset_version)
  manifest_check_field(
    version,
    grepl("^[0-9]+(\\.[0-9]+)?$", version) | version %in% c(":latest", ":draft"),
    "dataset_version", call
  )

  tryCatch(
    manifest_validate_query(manifest$resolved_query),
    error = function(e) {
      cli::cli_abort("{.arg manifest} has an invalid resolved query.",
        parent = e, class = "gtropic_error_manifest", call = call
      )
    }
  )

  invisible(manifest)
}

#' One path component of a manifest file: no separators, no leading dot
#' @keywords internal
#' @noRd
MANIFEST_PATH_SEGMENT <- "[A-Za-z0-9][A-Za-z0-9._-]*"

#' Abort unless every value of a manifest field is valid
#'
#' @param values Field values.
#' @param ok Logical vector, `TRUE` where the value is valid.
#' @param field Field name, for the message.
#' @param call Calling environment.
#'
#' @return Invisibly `TRUE`.
#' @keywords internal
#' @noRd
manifest_check_field <- function(values, ok, field, call) {
  ok <- !is.na(ok) & ok
  if (length(ok) == length(values) && all(ok)) {
    return(invisible(TRUE))
  }
  bad <- unique(as.character(values[!ok]))
  cli::cli_abort(
    c("{.arg manifest} has an invalid {.field {field}}.",
      x = "Offending value{?s}: {.val {truncate_display(bad, 5L)}}.",
      i = "Manifests made by {.code gtropic_manifest()} always pass this
           check; this one may have been edited."
    ),
    class = "gtropic_error_manifest",
    call = call
  )
}

#' Check the types of a manifest's resolved query
#'
#' Applies the same validators as a direct call to `gtropic_data()`, so a
#' replayed query can only contain values a real call could have produced.
#'
#' @param q The `resolved_query` list.
#'
#' @return Invisibly `TRUE`; errors name the offending field.
#' @keywords internal
#' @noRd
manifest_validate_query <- function(q) {
  is_chr <- function(x) is.character(x) && !anyNA(x)
  is_years <- function(x) {
    is.numeric(x) && !anyNA(x) && all(x == round(x))
  }
  is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
  is_filters <- function(x, allowed) {
    is.list(x) && all(names(x) %in% allowed) &&
      all(vapply(x, is_chr, logical(1)))
  }
  expect <- function(ok, field) {
    if (!isTRUE(ok)) {
      cli::cli_abort("{.field {field}} has an unexpected type or value.", call = NULL)
    }
  }

  expect(is_filters(q$geo_filters, c(
    "ADM2_ID", "ADM2_NAME", "ADM2_GROUP", "WHO_ENTITY", "WHO_REGION"
  )), "geo_filters")
  expect(is_filters(q$storm_filters, c("STORM_ID", "STORM_NAME", "USA_ATCF_ID")), "storm_filters")
  expect(is.null(q$storm_name_years) || is_years(q$storm_name_years), "storm_name_years")
  check_wind_radius_km(q$wind_radius_km, call = NULL)
  expect(is.null(q$polygon) || inherits(q$polygon, c("sf", "sfc")), "polygon")
  expect(is_flag(q$reproject), "reproject")
  expect(
    is.null(q$date_range) ||
      (is.atomic(q$date_range) && length(q$date_range) %in% 1:2),
    "date_range"
  )
  expect(is.null(q$date_list) || is.atomic(q$date_list), "date_list")
  expect(
    is.null(q$date_format) ||
      (is.character(q$date_format) && length(q$date_format) == 1L),
    "date_format"
  )
  expect(is_chr(q$tables), "tables")
  normalize_tables(q$tables, call = NULL)
  for (field in c(
    "precip_days_before", "precip_days_after", "flood_days_before",
    "flood_days_after"
  )) {
    check_nonneg_int(q[[field]], field, call = NULL)
  }
  check_positive_num(q$precip_radius_km, "precip_radius_km", call = NULL)
  check_positive_num(q$flood_radius_km, "flood_radius_km", call = NULL)
  expect(
    is.character(q$flood_match) && length(q$flood_match) == 1L &&
      q$flood_match %in% c("start", "overlap"),
    "flood_match"
  )
  expect(
    is.character(q$geometry_resolution) &&
      length(q$geometry_resolution) == 1L &&
      q$geometry_resolution %in% c("simplified", "full"),
    "geometry_resolution"
  )
  if ("make_valid" %in% names(q)) check_flag(q$make_valid, "make_valid", call = NULL)
  expect(is.null(q$resolved_adm2_ids) || is_chr(q$resolved_adm2_ids), "resolved_adm2_ids")
  expect(is.null(q$resolved_storm_ids) || is_chr(q$resolved_storm_ids), "resolved_storm_ids")
  expect(
    is.null(q$storm_metadata_years) || is_years(q$storm_metadata_years),
    "storm_metadata_years"
  )
  invisible(TRUE)
}

#' Warn when a manifest points at a server other than the configured one
#'
#' Replay downloads from the server recorded in the manifest. Say so before any
#' request is made, naming both hosts.
#'
#' @param manifest A validated manifest.
#'
#' @return Invisibly `TRUE` if the servers differ.
#' @keywords internal
#' @noRd
manifest_warn_server <- function(manifest) {
  configured <- opt_server()
  recorded <- manifest$server
  differs <- !identical(tolower(recorded), tolower(configured))
  if (differs) {
    cli::cli_warn(c(
      "This manifest was made against {.val {recorded}}, not the configured
       server {.val {configured}}.",
      i = "Its files will be downloaded from {.val {recorded}}.",
      i = "Set {.code options(gtropic.server = \"{recorded}\")} to silence
           this warning if that is intended."
    ))
  }
  invisible(differs)
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
    "geometry_resolution", "make_valid", "resolved_adm2_ids", "resolved_storm_ids",
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
  cli::cli_bullets(c("*" = "Server: {.val {x$server}}"))
  cli_dataset_bullets(datasets)
  cli::cli_bullets(c(
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

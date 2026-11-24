#' Save and load a manifest as JSON
#'
#' @description
#' `gtropic_manifest_write()` saves the manifest of a result as a JSON file;
#' `gtropic_manifest_read()` reads one back as a `gtropic_manifest` that can be
#' passed to `gtropic_data(manifest = )`.
#'
#' A JSON manifest is plain text. It is the safe way to share a manifest:
#' reading an `.rds` file with [readRDS()] can run code placed in it by whoever
#' made it, but reading JSON cannot. It is also readable, and easy to keep in
#' version control next to an analysis. `gtropic_manifest_read()` checks every
#' field, and then applies the same validation as a replay, so a file that is
#' not a manifest made by gtropic is rejected with an error of class
#' `gtropic_error_manifest` (see [gtropic_conditions]).
#'
#' @param x A `gtropic_manifest`, or a `gtropic_data` result whose manifest is
#'   saved.
#' @param path Path of the JSON file.
#'
#' @return `gtropic_manifest_write()` returns `path`, invisibly.
#'   `gtropic_manifest_read()` returns a `gtropic_manifest`.
#'
#' @examples
#' path <- tempfile(fileext = ".json")
#' gtropic_manifest_write(gtropic_example(), path)
#' manifest <- gtropic_manifest_read(path)
#' manifest
#' unlink(path)
#'
#' @seealso [gtropic_manifest()], [gtropic_data()]
#' @export
gtropic_manifest_write <- function(x, path) {
  if (inherits(x, "gtropic_data")) x <- gtropic_manifest(x)
  manifest_validate(x)
  check_path_string(path)
  json <- jsonlite::toJSON(manifest_to_json(x),
    auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE,
    digits = NA
  )
  written <- cache_write_atomic(path, function(tmp) {
    writeLines(json, tmp, useBytes = TRUE)
  })
  if (!written) {
    cli::cli_abort("Could not write the manifest to {.path {path}}.")
  }
  invisible(path)
}

#' @rdname gtropic_manifest_write
#' @export
gtropic_manifest_read <- function(path) {
  check_path_string(path)
  if (!file.exists(path)) {
    cli::cli_abort("{.path {path}} does not exist.")
  }
  json <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      gtropic_abort("Could not read {.path {path}} as JSON.",
        parent = e, class = "gtropic_error_manifest"
      )
    }
  )
  manifest_validate(manifest_from_json(json))
}

#' Validate a single file path argument
#' @keywords internal
#' @noRd
check_path_string <- function(path, arg = rlang::caller_arg(path),
                              call = rlang::caller_env()) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
    !nzchar(path)) {
    cli::cli_abort("{.arg {arg}} must be a single file path.", call = call)
  }
  invisible(path)
}

# --- writing -----------------------------------------------------------------

#' Columns of the manifest tables and their types
#' @keywords internal
#' @noRd
MANIFEST_FILE_FIELDS <- c(
  source_role = "character", dataset_doi = "character",
  dataset_version = "character", directory_label = "character",
  label = "character", file_id = "integer", checksum = "character",
  checksum_type = "character", size_bytes = "double", downloaded_at = "time"
)

#' @keywords internal
#' @noRd
MANIFEST_DATASET_FIELDS <- c(
  source_role = "character", dataset_doi = "character",
  dataset_version = "character"
)

#' A timestamp as ISO 8601 UTC text, keeping fractional seconds
#' @keywords internal
#' @noRd
json_time <- function(x) {
  out <- format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  out[is.na(x)] <- NA_character_
  out
}

#' Dates as ISO 8601 text and infinite numbers as "Inf", recursively
#' @keywords internal
#' @noRd
json_plain <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.character(as.Date(x)))
  }
  if (inherits(x, "Date")) {
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  if (is.numeric(x) && any(is.infinite(x))) {
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, json_plain))
  }
  x
}

#' The JSON representation of a manifest, as a list
#' @keywords internal
#' @noRd
manifest_to_json <- function(m) {
  files <- as.data.frame(m$files)[names(MANIFEST_FILE_FIELDS)]
  files$downloaded_at <- json_time(files$downloaded_at)
  query <- m$resolved_query
  if (!is.null(query$polygon)) {
    cli::cli_abort(
      "A manifest whose query still holds a {.arg polygon} cannot be saved as
       JSON."
    )
  }
  list(
    format = "gtropic_manifest",
    schema_version = m$schema_version,
    gtropic_version = m$gtropic_version,
    server = m$server,
    retrieved_at = json_time(m$retrieved_at),
    datasets = as.data.frame(m$datasets)[names(MANIFEST_DATASET_FIELDS)],
    files = files,
    resolved_query = json_plain(query),
    availability = json_plain(m$availability)
  )
}

# --- reading -----------------------------------------------------------------

#' Abort while reading a manifest file
#' @keywords internal
#' @noRd
abort_manifest_json <- function(field, call = rlang::caller_env()) {
  gtropic_abort(
    c("The manifest file has an invalid {.field {field}}.",
      i = "Files written by {.fn gtropic_manifest_write} always pass this
           check; this one may have been edited."
    ),
    class = "gtropic_error_manifest", call = call
  )
}

#' Convert a JSON array (a list from `read_json()`) to an atomic vector
#'
#' @param x The parsed JSON value: `NULL`, a scalar or a list of scalars.
#' @param type `"character"`, `"integer"`, `"double"` (`"Inf"` allowed),
#'   `"logical"`, `"date"`, `"time"` or `"date_or_number"`.
#' @param field Name for the error message.
#' @return The vector; `null` elements become `NA`.
#' @keywords internal
#' @noRd
json_vector <- function(x, type, field) {
  if (is.list(x)) {
    if (!all(vapply(x, function(e) is.null(e) || (is.atomic(e) && length(e) == 1L), logical(1)))) {
      abort_manifest_json(field)
    }
    x <- lapply(x, function(e) if (is.null(e)) NA else e)
    x <- if (length(x) == 0L) NULL else unlist(x, use.names = FALSE)
  }
  if (!is.null(x) && !is.atomic(x)) abort_manifest_json(field)
  ok_chr <- function(v) is.character(v) || all(is.na(v))
  out <- switch(type,
    character = if (is.null(x)) character(0) else if (ok_chr(x)) as.character(x),
    logical = if (is.logical(x)) x,
    integer = if (is.null(x)) {
      integer(0)
    } else if (is.numeric(x) || all(is.na(x))) {
      if (all(is.na(x) | x == round(x))) as.integer(x)
    },
    double = if (is.null(x)) {
      numeric(0)
    } else if (is.numeric(x) || all(is.na(x))) {
      as.numeric(x)
    } else if (is.character(x) && all(x %in% c("Inf", "-Inf", NA))) {
      as.numeric(x)
    },
    date = if (is.null(x)) {
      as.Date(character(0))
    } else if (ok_chr(x)) {
      d <- as.Date(as.character(x), optional = TRUE)
      if (!anyNA(d[!is.na(x)])) d
    },
    time = if (is.null(x)) {
      as.POSIXct(character(0), tz = "UTC")
    } else if (ok_chr(x)) {
      t <- as.POSIXct(as.character(x), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      if (!anyNA(t[!is.na(x)])) t
    },
    date_or_number = if (is.numeric(x)) as.numeric(x) else if (ok_chr(x)) as.character(x)
  )
  if (is.null(out)) abort_manifest_json(field)
  out
}

#' Rebuild a manifest table from an array of JSON records
#' @keywords internal
#' @noRd
json_table <- function(records, fields, field) {
  if (!is.list(records) || !is.null(names(records))) abort_manifest_json(field)
  for (r in records) {
    if (!is.list(r) || !setequal(names(r), names(fields))) {
      abort_manifest_json(field)
    }
  }
  cols <- lapply(names(fields), function(nm) {
    json_vector(lapply(records, `[[`, nm), fields[[nm]], paste0(field, "$", nm))
  })
  tibble::as_tibble(stats::setNames(cols, names(fields)))
}

#' Rebuild the resolved query, field by field
#' @keywords internal
#' @noRd
json_query <- function(q) {
  if (!is.list(q) || is.null(names(q))) abort_manifest_json("resolved_query")
  filters <- function(x, field) {
    if (is.null(x) || length(x) == 0L) {
      return(list())
    }
    if (!is.list(x) || is.null(names(x))) abort_manifest_json(field)
    lapply(stats::setNames(names(x), names(x)), function(nm) {
      json_vector(x[[nm]], "character", paste0(field, "$", nm))
    })
  }
  optional <- function(x, type, field) {
    if (is.null(x)) NULL else json_vector(x, type, field)
  }
  types <- c(
    storm_name_years = "integer", wind_radius_km = "double",
    reproject = "logical", date_range = "date_or_number",
    date_list = "date_or_number", date_format = "character",
    tables = "character", precip_days_before = "double",
    precip_days_after = "double", precip_radius_km = "double",
    flood_days_before = "double", flood_days_after = "double",
    flood_radius_km = "double", flood_match = "character",
    geometry_resolution = "character", make_valid = "logical",
    resolved_adm2_ids = "character", resolved_storm_ids = "character",
    storm_metadata_years = "integer"
  )
  out <- list()
  for (nm in intersect(manifest_query_fields(), names(q))) {
    value <- q[[nm]]
    out[nm] <- list(switch(nm,
      geo_filters = filters(value, "resolved_query$geo_filters"),
      storm_filters = filters(value, "resolved_query$storm_filters"),
      polygon = if (is.null(value)) NULL else abort_manifest_json("resolved_query$polygon"),
      optional(value, types[[nm]], paste0("resolved_query$", nm))
    ))
  }
  out
}

#' Rebuild the availability record
#'
#' It is informational (a replay recomputes it), so it is restored by key:
#' known date and integer fields get their types, everything else is text or
#' logical.
#' @keywords internal
#' @noRd
json_availability <- function(x, key = "availability") {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !is.null(names(x))) {
    return(lapply(stats::setNames(names(x), names(x)), function(nm) {
      json_availability(x[[nm]], nm)
    }))
  }
  dates <- c(
    "assessed_on", "start", "end", "available_from", "available_through"
  )
  integers <- c(
    "provisional_lag_days", "best_track_finalization_years",
    "typical_lag_days"
  )
  flat <- if (is.list(x)) unlist(x) else x
  type <- if (key %in% dates) {
    "date"
  } else if (key %in% integers) {
    "integer"
  } else if (is.logical(flat) && length(flat) > 0L) {
    "logical"
  } else {
    "character"
  }
  json_vector(x, type, paste0("availability$", key))
}

#' Rebuild a `gtropic_manifest` from parsed JSON
#'
#' Types are restored field by field; content is checked afterwards by
#' `manifest_validate()`.
#' @keywords internal
#' @noRd
manifest_from_json <- function(j) {
  if (!is.list(j) || is.null(names(j)) ||
    !identical(j$format, "gtropic_manifest")) {
    gtropic_abort(
      "The file is not a manifest written by {.fn gtropic_manifest_write}.",
      class = "gtropic_error_manifest"
    )
  }
  scalar <- function(x, type, field) {
    v <- json_vector(x, type, field)
    if (length(v) != 1L) abort_manifest_json(field)
    v
  }
  structure(
    list(
      files = json_table(j$files, MANIFEST_FILE_FIELDS, "files"),
      server = scalar(j$server, "character", "server"),
      datasets = json_table(j$datasets, MANIFEST_DATASET_FIELDS, "datasets"),
      resolved_query = if (is.null(j$resolved_query)) {
        NULL
      } else {
        json_query(j$resolved_query)
      },
      availability = json_availability(j$availability),
      retrieved_at = scalar(j$retrieved_at %||% NA, "time", "retrieved_at"),
      schema_version = scalar(j$schema_version, "integer", "schema_version"),
      gtropic_version = scalar(j$gtropic_version, "character", "gtropic_version")
    ),
    class = "gtropic_manifest"
  )
}

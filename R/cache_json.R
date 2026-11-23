# ---------------------------------------------------------------------------
# Cache metadata on disk.
#
# The cache index (`index.json`), the discovery snapshot (`discovery.json`)
# and the persisted cache setting (`config.json`) are JSON, never R
# serialisation. These files can sit on a shared disk, and reading an R
# serialisation file someone else wrote can run their code (CVE-2024-27322 for
# readRDS() on R < 4.4.0). Every reader goes through `cache_read_json()`:
# values are coerced field by field to the expected type and length, unknown
# fields are dropped, and anything unexpected makes the whole file count as
# absent.
# ---------------------------------------------------------------------------

#' Read a JSON metadata file through a schema
#'
#' @param path File to read.
#' @param schema Function taking the parsed JSON (`simplifyVector = FALSE`) and
#'   returning the R value; it signals an error for anything unexpected.
#' @param empty Value returned when the file is missing or invalid.
#' @param warn Warn (once per file and session) when the file is invalid.
#'
#' @return The value built by `schema`, or `empty`.
#' @keywords internal
#' @noRd
cache_read_json <- function(path, schema, empty = NULL, warn = TRUE) {
  if (!file.exists(path)) {
    return(empty)
  }
  out <- tryCatch(
    schema(jsonlite::read_json(path, simplifyVector = FALSE)),
    error = function(e) e
  )
  if (!inherits(out, "error")) {
    return(out)
  }
  if (warn) {
    cli::cli_warn(
      c("Ignoring {.path {path}}: it is not valid gtropic metadata.",
        x = "{conditionMessage(out)}"
      ),
      .frequency = "once",
      .frequency_id = paste0("gtropic_json_", path)
    )
  }
  empty
}

#' Write a JSON metadata file
#'
#' Written to a temporary file next to `path` and renamed into place, so a
#' reader never sees a half-written file.
#'
#' @param path Destination.
#' @param value List or data frame; scalars must be wrapped in
#'   [jsonlite::unbox()].
#'
#' @return `TRUE` on success, `FALSE` if the file could not be put in place.
#' @keywords internal
#' @noRd
cache_write_json <- function(path, value) {
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(
    value, tmp,
    dataframe = "rows", na = "null", null = "null", digits = NA
  )
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    return(FALSE)
  }
  TRUE
}

# --- typed fields -------------------------------------------------------------
#
# Field types: "chr", "num", "int", "lgl", "time" (ISO-8601 UTC text, whole
# seconds). A trailing "?" allows JSON null or an absent field, read as NA.

#' @keywords internal
#' @noRd
json_fail <- function(what) {
  cli::cli_abort("{what}", call = NULL)
}

#' @keywords internal
#' @noRd
json_is_object <- function(x) {
  is.list(x) && (length(x) == 0L || !is.null(names(x)))
}

#' @keywords internal
#' @noRd
json_is_array <- function(x) is.list(x) && is.null(names(x))

#' Format and parse cache timestamps
#'
#' Whole seconds in UTC, so a time read back is identical to the one written.
#' @keywords internal
#' @noRd
cache_time <- function(x) {
  as.POSIXct(floor(as.numeric(x)), origin = "1970-01-01", tz = "UTC")
}

#' @keywords internal
#' @noRd
format_cache_time <- function(x) {
  format(cache_time(x), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @keywords internal
#' @noRd
parse_cache_time <- function(x) {
  out <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", x)] <- NA
  out
}

#' Coerce a list of JSON scalars to one typed vector
#'
#' @param values List of parsed JSON values (`NULL` for null or absent).
#' @param type Field type, see above.
#' @param field Field name, for the error message.
#'
#' @return An atomic vector of `length(values)`.
#' @keywords internal
#' @noRd
json_column <- function(values, type, field) {
  nullable <- endsWith(type, "?")
  base <- sub("?", "", type, fixed = TRUE)
  is_null <- vapply(values, is.null, logical(1))
  if (any(is_null) && !nullable) {
    json_fail(paste0("Field '", field, "' is missing or null."))
  }
  present <- values[!is_null]
  # Check each element's JSON type before flattening, so that a mixed array
  # such as ["a", 5] is not silently coerced.
  is_type <- switch(base,
    chr = ,
    time = is.character,
    num = ,
    int = is.numeric,
    lgl = is.logical,
    json_fail(paste0("Unknown field type '", type, "'."))
  )
  scalar <- vapply(
    present, function(v) is.atomic(v) && length(v) == 1L && is_type(v),
    logical(1)
  )
  if (!all(scalar)) {
    json_fail(paste0("Field '", field, "' is not a single ", base, " value."))
  }
  flat <- unlist(present, use.names = FALSE)
  ok <- length(flat) == 0L || switch(base,
    chr = TRUE,
    num = all(is.finite(flat)),
    int = all(is.finite(flat)) && all(flat == round(flat)) &&
      all(abs(flat) <= .Machine$integer.max),
    lgl = !anyNA(flat),
    time = !anyNA(parse_cache_time(flat))
  )
  if (!isTRUE(ok)) {
    json_fail(paste0("Field '", field, "' has an invalid ", base, " value."))
  }
  out <- switch(base,
    chr = rep(NA_character_, length(values)),
    num = rep(NA_real_, length(values)),
    int = rep(NA_integer_, length(values)),
    lgl = rep(NA, length(values)),
    time = as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
  )
  if (length(flat) > 0L) {
    out[!is_null] <- switch(base,
      chr = as.character(flat),
      num = as.numeric(flat),
      int = as.integer(flat),
      lgl = as.logical(flat),
      time = parse_cache_time(flat)
    )
  }
  out
}

#' Read one scalar field of a JSON object
#' @keywords internal
#' @noRd
json_scalar <- function(x, type, field) json_column(list(x), type, field)

#' Read a JSON array of records into a tibble
#'
#' @param x Parsed JSON value.
#' @param fields Named character vector of field types. Other fields are
#'   dropped.
#' @param what Name of the array, for the error message.
#'
#' @return A tibble with one column per field, in the order of `fields`.
#' @keywords internal
#' @noRd
json_records <- function(x, fields, what) {
  if (!json_is_array(x) || !all(vapply(x, json_is_object, logical(1)))) {
    json_fail(paste0("'", what, "' is not an array of records."))
  }
  cols <- lapply(names(fields), function(f) {
    json_column(lapply(x, function(r) r[[f]]), fields[[f]], f)
  })
  names(cols) <- names(fields)
  tibble::as_tibble(cols)
}

#' Read a JSON array of whole numbers
#' @keywords internal
#' @noRd
json_integers <- function(x, what) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!json_is_array(x)) {
    json_fail(paste0("'", what, "' is not an array."))
  }
  json_column(x, "int", what)
}

# --- schemas ------------------------------------------------------------------

#' Fields of a cache index entry
#' @keywords internal
#' @noRd
CACHE_INDEX_FIELDS <- c(
  server = "chr", doi = "chr", version = "chr", directory_label = "chr",
  label = "chr", checksum = "chr", size_bytes = "num", fetched_at = "time"
)

#' Fields of a file listing row, as returned by `dv_list_files()`
#' @keywords internal
#' @noRd
LISTING_FIELDS <- c(
  file_id = "int?", label = "chr?", directory_label = "chr?",
  checksum = "chr?", checksum_type = "chr?", size_bytes = "num?",
  content_type = "chr?"
)

#' @keywords internal
#' @noRd
json_check_servers <- function(servers) {
  for (s in unique(servers)) validate_server(s, call = NULL)
  invisible(TRUE)
}

#' Schema of `index.json`: an array of index entries
#' @keywords internal
#' @noRd
cache_index_schema <- function(x) {
  idx <- json_records(x, CACHE_INDEX_FIELDS, "index")
  if (!all(is_hex_checksum(idx$checksum))) {
    json_fail("An index entry has an invalid checksum.")
  }
  json_check_servers(idx$server)
  idx
}

#' Schema of `config.json`: `{"cache_enabled": true|false}`
#' @keywords internal
#' @noRd
cache_config_schema <- function(x) {
  if (!json_is_object(x)) json_fail("The settings file is not an object.")
  json_scalar(x$cache_enabled, "lgl", "cache_enabled")
}

#' Schema of one dataset slot in a discovery snapshot
#' @keywords internal
#' @noRd
discovery_slot_schema <- function(x, what) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!json_is_object(x)) json_fail(paste0("'", what, "' is not an object."))
  listing <- json_records(x$listing, LISTING_FIELDS, paste0(what, " listing"))
  checksum <- listing$checksum
  if (!all(is.na(checksum) | is_hex_checksum(checksum))) {
    json_fail(paste0("The ", what, " listing has an invalid checksum."))
  }
  list(
    doi = json_scalar(x$doi, "chr", "doi"),
    version = json_scalar(x$version, "chr", "version"),
    listing = listing,
    years = json_integers(x$years, paste0(what, " years"))
  )
}

#' Schema of `discovery.json`
#' @keywords internal
#' @noRd
cache_discovery_schema <- function(x) {
  if (!json_is_object(x) || !json_is_object(x$discovery)) {
    json_fail("The snapshot is not an object.")
  }
  d <- x$discovery
  year_index <- json_records(d$year_index, c(year = "int", dataset = "chr"), "year_index")
  if (!all(year_index$dataset %in% c("historical", "current"))) {
    json_fail("The year index names an unknown dataset.")
  }
  server <- json_scalar(x$server, "chr", "server")
  discovery_server <- json_scalar(d$server, "chr", "server")
  json_check_servers(c(server, discovery_server))
  list(
    saved_at = json_scalar(x$saved_at, "time", "saved_at"),
    server = server,
    collection = json_scalar(x$collection, "chr", "collection"),
    discovery = list(
      server = discovery_server,
      historical = discovery_slot_schema(d$historical, "historical"),
      current = discovery_slot_schema(d$current, "current"),
      year_index = year_index
    )
  )
}

#' Convert a discovery result to its JSON form
#' @keywords internal
#' @noRd
discovery_to_json <- function(discovery) {
  slot <- function(s) {
    if (is.null(s)) {
      return(NULL)
    }
    list(
      doi = jsonlite::unbox(s$doi),
      version = jsonlite::unbox(s$version),
      listing = as.data.frame(s$listing)[intersect(names(LISTING_FIELDS), names(s$listing))],
      years = if (is.null(s$years)) NULL else as.integer(s$years)
    )
  }
  list(
    server = jsonlite::unbox(discovery$server),
    historical = slot(discovery$historical),
    current = slot(discovery$current),
    year_index = as.data.frame(discovery$year_index)
  )
}

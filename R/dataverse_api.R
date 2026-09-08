# ---------------------------------------------------------------------------
# Thin wrappers over the `dataverse` client.
#
# Two invariants are enforced here and relied on everywhere else:
#
#  1. Dataverse has no real directories. Every file carries a `label` (e.g.
#     "1980.parquet") and a `directoryLabel` (e.g. "02_wind/exposures").
#     Labels are NOT unique within a dataset -- "1980.parquet" appears under
#     exposures, zero_pairs, storm_metadata and storm_tracks alike. All
#     resolution therefore matches on the (directoryLabel, label) PAIR and then
#     downloads by numeric id. `dataverse::get_file_by_name()` must never be
#     used: it matches on label alone and will silently return the wrong file.
#
#  2. Network failure is expected, not exceptional. Every call is wrapped so
#     users see an explanation naming the server and the likely cause instead of
#     a raw HTTP condition.
# ---------------------------------------------------------------------------

#' Retry an expression with bounded exponential backoff
#'
#' @param expr Expression to evaluate.
#' @param max_tries Maximum attempts.
#' @param what Short description used in the failure message.
#' @param quiet Suppress the retry notices.
#' @param call Calling environment for error reporting.
#'
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
with_retry <- function(expr,
                       max_tries = opt_max_tries(),
                       what = "request",
                       quiet = FALSE,
                       call = rlang::caller_env()) {
  expr <- rlang::enquo(expr)
  last_err <- NULL

  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(rlang::eval_tidy(expr), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }
    last_err <- result

    if (attempt < max_tries) {
      # Exponential backoff, capped. Politeness to the server matters more than
      # shaving a second off a failing request.
      wait <- min(2^(attempt - 1), 8)
      inform_unless_quiet(
        "Retrying {what} ({attempt}/{max_tries - 1} retries used);
         waiting {wait}s.",
        quiet = quiet
      )
      Sys.sleep(wait)
    }
  }

  abort_dataverse(last_err, what = what, call = call)
}

#' Turn a low-level failure into an informative message
#'
#' @param err The captured condition.
#' @param what Description of the operation that failed.
#' @param call Calling environment.
#'
#' @return Never returns.
#' @keywords internal
#' @noRd
abort_dataverse <- function(err, what = "request", call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Could not complete {what} against {.val {opt_server()}}.",
      x = "{conditionMessage(err)}",
      i = "Common causes: no network connection; the DOI has moved; the
           requested dataset version does not exist; or the server is down.",
      i = "Check {.code getOption('gtropic.server')} and the DOI options
           (see {.code ?gtropic_options})."
    ),
    call = call
  )
}

#' List the files in a Dataverse dataset version
#'
#' @param doi Dataset DOI, e.g. `"doi:10.70122/FK2/SWEYST"`.
#' @param version Version string; `":latest"` for the newest published version.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#'
#' @return A tibble with columns `file_id`, `label`, `directory_label`, `md5`,
#'   `size_bytes`, `content_type`.
#' @keywords internal
#' @noRd
dv_list_files <- function(doi,
                          version = ":latest",
                          server = opt_server(),
                          quiet = FALSE) {
  raw_files <- with_retry(
    dataverse::dataset_files(
      dataset = doi,
      version = version,
      server = server,
      key = dv_key()
    ),
    what = glue::glue("file listing for {doi}"),
    quiet = quiet
  )

  if (length(raw_files) == 0L) {
    return(dv_empty_listing())
  }

  # `dataset_files()` returns a list of per-file records whose shape differs a
  # little between Dataverse releases: the checksum may be `md5` or nested under
  # `checksum`. Pull defensively rather than assuming one layout.
  rows <- lapply(raw_files, function(f) {
    df <- f$dataFile %||% list()
    tibble::tibble(
      file_id = as.integer(df$id %||% NA_integer_),
      label = as.character(f$label %||% df$filename %||% NA_character_),
      directory_label = as.character(f$directoryLabel %||% ""),
      md5 = as.character(df$md5 %||% df$checksum$value %||% NA_character_),
      size_bytes = as.numeric(df$filesize %||% NA_real_),
      content_type = as.character(df$contentType %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
dv_empty_listing <- function() {
  tibble::tibble(
    file_id = integer(0),
    label = character(0),
    directory_label = character(0),
    md5 = character(0),
    size_bytes = numeric(0),
    content_type = character(0)
  )
}

#' Resolve a (directory_label, label) pair to a single listing row
#'
#' @param listing Output of `dv_list_files()`.
#' @param directory_label Directory label to match, e.g. `"02_wind/exposures"`.
#' @param label File label to match, e.g. `"1980.parquet"`.
#' @param doi DOI, used only in messages.
#' @param call Calling environment.
#'
#' @return A one-row tibble from `listing`, or `NULL` when absent.
#' @keywords internal
#' @noRd
dv_resolve_file <- function(listing, directory_label, label, doi = NULL,
                            call = rlang::caller_env()) {
  hit <- listing[
    listing$directory_label == directory_label & listing$label == label, ,
    drop = FALSE
  ]

  if (nrow(hit) == 0L) {
    return(NULL)
  }

  if (nrow(hit) > 1L) {
    # A duplicated pair means the upstream publication contract has broken.
    # Guessing would make results irreproducible, so refuse.
    cli::cli_abort(
      c("Ambiguous file in {.val {doi %||% 'dataset'}}.",
        x = "{nrow(hit)} files share the path
             {.path {file.path(directory_label, label)}}.",
        i = "This indicates a publication error upstream; please report it."
      ),
      call = call
    )
  }

  hit
}

#' Download a file's raw bytes
#'
#' Returns a raw vector rather than writing to disk. `format = "original"` is
#' passed defensively: parquet is not ingested by Dataverse, but if the server
#' ever decided otherwise we want the bytes we published, not a derived TSV.
#'
#' @param file_id Numeric Dataverse file id.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#'
#' @return A raw vector.
#' @keywords internal
#' @noRd
dv_get_file <- function(file_id, server = opt_server(), quiet = FALSE) {
  with_retry(
    dataverse::get_file(
      file = file_id,
      server = server,
      format = "original",
      key = dv_key()
    ),
    what = glue::glue("download of file {file_id}"),
    quiet = quiet
  )
}

#' List datasets in a Dataverse collection
#'
#' @param collection Collection alias.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#'
#' @return A tibble with a `doi` column (possibly zero rows).
#' @keywords internal
#' @noRd
dv_list_datasets <- function(collection = opt_collection(),
                             server = opt_server(),
                             quiet = FALSE) {
  contents <- with_retry(
    dataverse::dataverse_contents(
      dataverse = collection,
      server = server,
      key = dv_key()
    ),
    what = glue::glue("listing of collection {collection}"),
    quiet = quiet
  )

  datasets <- Filter(
    function(x) identical(x$type, "dataset") || inherits(x, "dataverse_dataset"),
    contents
  )

  if (length(datasets) == 0L) {
    return(tibble::tibble(doi = character(0)))
  }

  dois <- vapply(datasets, function(d) {
    # Dataverse reports either a ready-made `persistentUrl` or the components.
    if (!is.null(d$persistentUrl)) {
      sub("^https?://doi\\.org/", "doi:", d$persistentUrl)
    } else if (!is.null(d$protocol)) {
      paste0(d$protocol, ":", d$authority, "/", d$identifier)
    } else {
      NA_character_
    }
  }, character(1))

  tibble::tibble(doi = dois[!is.na(dois)])
}

#' Retrieve the published version label of a dataset
#'
#' The manifest must record a concrete version, never `":latest"`, or the
#' reproducibility guarantee is empty.
#'
#' @param doi Dataset DOI.
#' @param version Requested version.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#'
#' @return A character scalar such as `"2.0"`.
#' @keywords internal
#' @noRd
dv_version_label <- function(doi,
                             version = ":latest",
                             server = opt_server(),
                             quiet = FALSE) {
  ds <- with_retry(
    dataverse::get_dataset(
      dataset = doi,
      version = version,
      server = server,
      key = dv_key()
    ),
    what = glue::glue("metadata for {doi}"),
    quiet = quiet
  )

  major <- ds$versionNumber %||% ds$majorVersion
  minor <- ds$versionMinorNumber %||% ds$minorVersion

  if (is.null(major)) {
    return(as.character(version))
  }
  paste0(major, ".", minor %||% 0L)
}

#' API key, if the user has one
#'
#' No key is needed for the demo repository, but honouring `DATAVERSE_KEY`
#' means the same code works against a restricted staging repository. The
#' Dataverse client expects a character scalar either way, so we pass an empty
#' string when no key is configured.
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
dv_key <- function() {
  key <- Sys.getenv("DATAVERSE_KEY", unset = "")
  if (nzchar(key)) key else ""
}

#' MD5 of a raw vector
#'
#' [tools::md5sum()] only operates on files, so the bytes go through a
#' temporary file. This avoids taking on a hashing dependency (`digest`,
#' `openssl`) purely for checksum verification.
#'
#' @param raw A raw vector.
#' @return Character scalar, lower-case hex.
#' @keywords internal
#' @noRd
md5_raw <- function(raw) {
  tf <- tempfile(fileext = ".bin")
  on.exit(unlink(tf), add = TRUE)
  writeBin(raw, tf)
  unname(tools::md5sum(tf))
}

#' Verify downloaded bytes against the published checksum
#'
#' @param raw Raw vector as downloaded.
#' @param expected_md5 MD5 recorded in the Dataverse listing.
#' @param label Path used in messages.
#' @param call Calling environment.
#'
#' @return Invisibly `TRUE`; aborts on mismatch.
#' @keywords internal
#' @noRd
verify_md5 <- function(raw, expected_md5, label = "file",
                       call = rlang::caller_env()) {
  if (is.na(expected_md5) || !nzchar(expected_md5)) {
    # Nothing published to check against; not an error, but worth knowing.
    cli::cli_warn(
      c("No checksum published for {.path {label}}; integrity not verified.")
    )
    return(invisible(TRUE))
  }

  got <- md5_raw(raw)
  if (!identical(tolower(got), tolower(expected_md5))) {
    cli::cli_abort(
      c("Checksum mismatch for {.path {label}}.",
        x = "Expected {.val {expected_md5}} but the download hashed to
             {.val {got}}.",
        i = "The download was likely truncated. Try again; if it persists the
             published file may have changed."
      ),
      call = call
    )
  }

  invisible(TRUE)
}

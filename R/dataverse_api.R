# ---------------------------------------------------------------------------
# The Dataverse transport: four read-only endpoints of the native API, called
# directly through httr2.
#
# Three invariants are enforced here and relied on everywhere else:
#
#  1. Dataverse has no real directories. Every file carries a `label` (e.g.
#     "1980.parquet") and a `directoryLabel` (e.g. "02_wind/exposures").
#     Labels are NOT unique within a dataset -- "1980.parquet" appears under
#     exposures, zero_pairs, storm_metadata and storm_tracks alike. All
#     resolution therefore matches on the (directoryLabel, label) PAIR and then
#     downloads by numeric id.
#
#  2. Network failure is expected, not exceptional. Every call is wrapped so
#     users see an explanation naming the server and the likely cause instead of
#     a raw HTTP condition.
#
#  3. A downloaded file never sits in memory whole. It is written to disk as it
#     arrives, hashed from disk, and read from disk.
# ---------------------------------------------------------------------------

#' Turn a low-level failure into an informative message
#'
#' @param err The captured condition.
#' @param what Description of the operation that failed.
#' @param server Host the request was sent to.
#' @param call Calling environment.
#'
#' @return Never returns.
#' @keywords internal
#' @noRd
abort_dataverse <- function(err, what = "request", server = opt_server(),
                            call = rlang::caller_env()) {
  gtropic_abort(
    c(
      "Could not complete {what} against {.val {server}}.",
      x = "{conditionMessage(err)}",
      i = "Common causes: no network connection; the DOI has moved; the
           requested dataset version does not exist; or the server is down.",
      i = "Check {.code getOption('gtropic.server')} and the DOI options
           (see {.code ?gtropic_options})."
    ),
    class = "gtropic_error_network",
    call = call
  )
}

#' Base URL of the Dataverse native API on a host
#'
#' Requests always use HTTPS; `server` has already passed `validate_server()`.
#'
#' @param server Dataverse host.
#' @return A character scalar.
#' @keywords internal
#' @noRd
dv_api_url <- function(server) {
  paste0("https://", server, "/api")
}

#' The User-Agent sent with every request
#' @keywords internal
#' @noRd
gtropic_user_agent <- function() {
  paste0(
    "gtropic/", utils::packageVersion("gtropic"),
    " (https://github.com/sparklabnyc/gtropic)"
  )
}

#' Percent-encode one URL path segment
#'
#' Collection aliases and version strings come from options and user input.
#' Encoding keeps them inside their own path segment; `:` stays as it is
#' because Dataverse spells `:persistentId` and `:latest` with it.
#'
#' @param x Character scalar.
#' @keywords internal
#' @noRd
dv_path_segment <- function(x) {
  gsub("%3A", ":", utils::URLencode(as.character(x), reserved = TRUE),
    fixed = TRUE
  )
}

#' A request with the settings every gtropic request shares
#'
#' Only transient failures are retried, up to `getOption("gtropic.max_tries")`
#' attempts in all: a lost or refused connection, a timeout, and HTTP 408,
#' 429 and 5xx responses. Anything else (a mistyped DOI, a missing version)
#' fails at once.
#'
#' @param url Full URL.
#' @keywords internal
#' @noRd
gtropic_request <- function(url) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, gtropic_user_agent())
  req <- httr2::req_retry(req,
    max_tries = opt_max_tries(),
    retry_on_failure = TRUE,
    is_transient = dv_is_transient,
    backoff = dv_backoff,
    after = dv_retry_after
  )
  httr2::req_error(req, body = dv_error_body)
}

#' Is an HTTP response worth retrying?
#'
#' @param resp An `httr2` response.
#' @keywords internal
#' @noRd
dv_is_transient <- function(resp) {
  status <- httr2::resp_status(resp)
  status %in% c(408L, 429L) || status >= 500L
}

#' Seconds to wait before retry `i`: 1, 2, 4, then 8 at most
#'
#' Politeness to the server matters more than shaving a second off a failing
#' request.
#'
#' @param i Number of attempts made so far.
#' @keywords internal
#' @noRd
dv_backoff <- function(i) {
  min(2^(i - 1), 8)
}

#' Seconds a server asked us to wait (`Retry-After`), at most 60
#'
#' @param resp An `httr2` response.
#' @return A number, or `NA` to use `dv_backoff()`.
#' @keywords internal
#' @noRd
dv_retry_after <- function(resp) {
  wait <- httr2::resp_retry_after(resp)
  if (is.na(wait)) NA_real_ else min(max(wait, 0), 60)
}

#' Run a request and report a failure in gtropic's terms
#'
#' Retries happen inside `httr2` (see `gtropic_request()`); each is announced
#' unless `quiet`. Any failure becomes one `gtropic_error_network` error that
#' names the server; errors gtropic raised itself (such as the download size
#' bound) pass through unchanged.
#'
#' @param expr The request and its processing.
#' @param what Description of the operation, used in messages.
#' @param server Host the request is sent to.
#' @param quiet Suppress retry notices.
#' @param call Calling environment for error reporting.
#'
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
dv_perform <- function(expr, what, server, quiet = FALSE,
                       call = rlang::caller_env()) {
  max_tries <- opt_max_tries()
  # httr2 announces its own waits too, with a progress bar when interactive.
  # The notice below says which request is waiting; httr2's is dropped.
  old <- options(httr2_progress = FALSE)
  on.exit(options(old), add = TRUE)
  tryCatch(
    withCallingHandlers(
      expr,
      httr2_retry = function(cnd) {
        # Also signalled after the last attempt, when nothing is retried.
        if (cnd$tries >= max_tries) {
          return()
        }
        wait <- round(cnd$delay, 1)
        inform_unless_quiet(
          "Retrying {what} ({cnd$tries}/{max_tries - 1} retries used);
           waiting {wait}s.",
          quiet = quiet
        )
      },
      message = function(m) {
        if (grepl("for retry backoff", conditionMessage(m), fixed = TRUE)) {
          invokeRestart("muffleMessage")
        }
      }
    ),
    error = function(e) {
      if (inherits(e, "gtropic_error")) {
        stop(e)
      }
      abort_dataverse(e, what = what, server = server, call = call)
    }
  )
}

#' A request to the Dataverse native API
#'
#' The API token is attached only here, and only for the configured server
#' (`dv_key()`).
#'
#' @param server Dataverse host.
#' @param ... Path segments after `/api`, already encoded.
#' @keywords internal
#' @noRd
dv_request <- function(server, ...) {
  req <- gtropic_request(paste(c(dv_api_url(server), ...), collapse = "/"))
  key <- dv_key(server)
  if (nzchar(key)) {
    req <- httr2::req_headers_redacted(req, `X-Dataverse-key` = key)
  }
  req
}

#' The error text Dataverse puts in a JSON error response
#'
#' @param resp An `httr2` response.
#' @return A character vector (possibly empty) for [httr2::req_error()].
#' @keywords internal
#' @noRd
dv_error_body <- function(resp) {
  if (!identical(httr2::resp_content_type(resp), "application/json")) {
    return(character(0))
  }
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) NULL
  )
  msg <- if (is.list(body)) body$message else NULL
  if (is.character(msg) && length(msg) == 1L && !is.na(msg)) msg else character(0)
}

#' Perform a metadata request and return its `data` element
#'
#' @param req Request from `dv_request()`.
#' @param what Description of the operation, used in messages.
#' @param server Host the request is sent to.
#' @param quiet Suppress retry notices.
#' @param call Calling environment for error reporting.
#'
#' @return The parsed `data` element of the Dataverse response.
#' @keywords internal
#' @noRd
dv_get_json <- function(req, what, server, quiet, call = rlang::caller_env()) {
  dv_perform(
    {
      resp <- httr2::req_perform(req)
      body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      if (!identical(body$status, "OK")) {
        status <- body$status %||% "none"
        cli::cli_abort("Dataverse reported status {.val {status}}.")
      }
      body$data
    },
    what = what, server = server, quiet = quiet, call = call
  )
}

#' List the files in a Dataverse dataset version
#'
#' @param doi Dataset DOI, e.g. `"doi:10.7910/DVN/EAVWC3"`.
#' @param version Version string; `":latest"` for the newest published version.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#'
#' @return A tibble with columns `file_id`, `label`, `directory_label`,
#'   `checksum`, `checksum_type`, `size_bytes`, `content_type`.
#' @keywords internal
#' @noRd
dv_list_files <- function(doi,
                          version = ":latest",
                          server = opt_server(),
                          quiet = FALSE) {
  req <- dv_request(
    server, "datasets", ":persistentId", "versions",
    dv_path_segment(version), "files"
  )
  req <- httr2::req_url_query(req, persistentId = doi)
  raw_files <- dv_get_json(req,
    what = glue::glue("file listing for {doi}"),
    server = server, quiet = quiet
  )

  dv_listing_from_records(raw_files)
}

#' Build a file listing from Dataverse file records
#'
#' Kept separate from the transport so it can be tested without a server.
#'
#' @param raw_files List of per-file records as returned by the Dataverse API.
#'
#' @return A tibble as described for `dv_list_files()`.
#' @keywords internal
#' @noRd
dv_listing_from_records <- function(raw_files) {
  if (length(raw_files) == 0L) {
    return(dv_empty_listing())
  }

  # The file records differ a little between Dataverse releases: the checksum
  # may be supplied in a legacy `md5` field or nested under `checksum`.
  # Preserve its declared algorithm.
  rows <- lapply(raw_files, function(f) {
    df <- f$dataFile %||% list()
    checksum_record <- df$checksum %||% list()
    checksum_value <- if (is.list(checksum_record)) {
      checksum_record$value %||% NA_character_
    } else {
      checksum_record
    }
    checksum_type <- if (is.list(checksum_record)) {
      checksum_record$type %||% NA_character_
    } else {
      NA_character_
    }
    tibble::tibble(
      file_id = as.integer(df$id %||% NA_integer_),
      label = as.character(f$label %||% df$filename %||% NA_character_),
      directory_label = as.character(f$directoryLabel %||% ""),
      checksum = as.character(df$md5 %||% checksum_value),
      checksum_type = as.character(
        if (!is.null(df$md5)) "MD5" else checksum_type
      ),
      size_bytes = as.numeric(df$filesize %||% NA_real_),
      content_type = as.character(df$contentType %||% NA_character_)
    )
  })

  out <- dplyr::bind_rows(rows)
  # Checksums later name files in the cache. Anything that is not a plain
  # hexadecimal digest is treated as "no checksum published".
  out$checksum[!is_hex_checksum(out$checksum)] <- NA_character_
  out
}

#' @keywords internal
#' @noRd
dv_empty_listing <- function() {
  tibble::tibble(
    file_id = integer(0),
    label = character(0),
    directory_label = character(0),
    checksum = character(0),
    checksum_type = character(0),
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

#' Download a file to disk
#'
#' The body is written to `dest` as it arrives; nothing is held in memory.
#' `format = "original"` is passed defensively: parquet is not ingested by
#' Dataverse, but if the server ever decided otherwise we want the bytes we
#' published, not a derived TSV.
#'
#' Dataverse usually answers with a redirect to its storage (for Harvard
#' Dataverse, Amazon S3). Redirects are followed here rather than by curl so
#' that the API token, which belongs to the Dataverse host, is never sent to
#' the storage host, and so a redirect cannot downgrade the request to plain
#' HTTP.
#'
#' The transfer is bounded (see `download_limit()`): a server that sends more
#' than the file's listed size allows is stopped, and the partial file
#' removed, with an error of class `gtropic_error_download_size`.
#'
#' @param file_id Numeric Dataverse file id.
#' @param server Dataverse host.
#' @param quiet Suppress progress messages.
#' @param dest Path to write. It must not exist yet; on failure it is removed.
#' @param size_bytes The file's size in the listing (or manifest), or `NA`
#'   when none is given.
#'
#' @return `dest`, invisibly.
#' @keywords internal
#' @noRd
dv_get_file <- function(file_id, server = opt_server(), quiet = FALSE,
                        dest = tempfile(fileext = ".download"),
                        size_bytes = NA_real_) {
  req <- dv_request(server, "access", "datafile", dv_file_id(file_id))
  req <- httr2::req_url_query(req, format = "original")
  what <- glue::glue("download of file {file_id}")
  done <- FALSE
  on.exit(if (!done) unlink(dest), add = TRUE)
  dv_perform(
    dv_download(req, dest, bound = download_bound(size_bytes, what)),
    what = what, server = server, quiet = quiet
  )
  done <- TRUE
  invisible(dest)
}

#' Most bytes a download may deliver when the listing gives no size (8 GB)
#'
#' A fixed ceiling, deliberately not an option. Files with a listed size are
#' bounded by that size instead.
#' @keywords internal
#' @noRd
DOWNLOAD_CEILING_BYTES <- 8 * 1024^3

#' The most bytes a download may deliver
#'
#' A file listed with a size may exceed it by 1 MB or 1 %, whichever is more,
#' to allow for a listing that rounds. A file listed without a size is bounded
#' by `DOWNLOAD_CEILING_BYTES`.
#'
#' @param size_bytes Listed size, or `NA`.
#' @return A number of bytes.
#' @keywords internal
#' @noRd
download_limit <- function(size_bytes) {
  size <- suppressWarnings(as.numeric(size_bytes))
  if (length(size) != 1L || !is.finite(size) || size < 0) {
    return(DOWNLOAD_CEILING_BYTES)
  }
  size + max(1024^2, 0.01 * size)
}

#' The size bound for one download
#'
#' @param size_bytes Listed size, or `NA`.
#' @param what Description of the download, used in the error.
#' @return A list with `limit` (bytes), `listed` (`size_bytes` as a number)
#'   and `what`.
#' @keywords internal
#' @noRd
download_bound <- function(size_bytes, what = "download") {
  listed <- suppressWarnings(as.numeric(size_bytes))
  if (length(listed) != 1L || !is.finite(listed) || listed < 0) listed <- NA_real_
  list(limit = download_limit(listed), listed = listed, what = what)
}

#' Abort a download that exceeds its bound
#'
#' @param bound Output of `download_bound()`.
#' @param call Calling environment.
#' @keywords internal
#' @noRd
abort_download_size <- function(bound, call = rlang::caller_env()) {
  what <- bound$what
  limit <- format_bytes(bound$limit)
  listed <- format_bytes(bound$listed)
  gtropic_abort(
    c(
      "Stopped the {what}: the server sent more than {limit}.",
      i = if (is.na(bound$listed)) {
        "The file listing gives no size, so a download may be at most {limit}."
      } else {
        "The file is listed as {listed}."
      },
      i = "The partial file was removed. If the file was republished, list
           the dataset again; otherwise report it to the dataset maintainers."
    ),
    class = "gtropic_error_download_size",
    call = call
  )
}

#' A Dataverse file id as a URL path segment
#'
#' @param file_id Whole positive number.
#' @keywords internal
#' @noRd
dv_file_id <- function(file_id, call = rlang::caller_env()) {
  if (!is.numeric(file_id) || length(file_id) != 1L || is.na(file_id) ||
    file_id < 1 || file_id != round(file_id)) {
    cli::cli_abort("A Dataverse file id must be a whole positive number.",
      call = call
    )
  }
  format(file_id, scientific = FALSE, trim = TRUE)
}

#' Perform a download request, following redirects, and write the body
#'
#' @param req Request from `dv_request()`.
#' @param dest Destination path.
#' @param bound Size bound from `download_bound()`.
#' @param max_redirects Redirects followed before giving up.
#'
#' @return `dest`, invisibly.
#' @keywords internal
#' @noRd
dv_download <- function(req, dest, bound = download_bound(NA),
                        max_redirects = 5L) {
  for (hop in seq_len(max_redirects + 1L)) {
    resp <- httr2::req_perform_connection(
      httr2::req_options(req, followlocation = 0L)
    )
    status <- httr2::resp_status(resp)
    if (status < 300L || status >= 400L) {
      on.exit(close(resp), add = TRUE)
      return(invisible(dv_write_body(resp, dest, bound)))
    }
    location <- httr2::resp_header(resp, "Location")
    from <- resp$url
    close(resp)
    req <- dv_redirect_request(location, from)
  }
  cli::cli_abort("Too many redirects (more than {max_redirects}).")
}

#' The request that follows a redirect
#'
#' Carries the shared settings but no API token: the target is the storage
#' host, not the Dataverse host the token belongs to.
#'
#' @param location Value of the `Location` header.
#' @param from URL that answered with the redirect.
#' @keywords internal
#' @noRd
dv_redirect_request <- function(location, from) {
  if (is.null(location) || !nzchar(location)) {
    cli::cli_abort("The server sent a redirect without a location.")
  }
  url <- httr2::url_parse(location, base_url = from)
  if (identical(httr2::url_parse(from)$scheme, "https") &&
    !identical(url$scheme, "https")) {
    cli::cli_abort("Refusing a redirect from HTTPS to {.val {url$scheme}}.")
  }
  gtropic_request(httr2::url_build(url))
}

#' Write a response body to a file in chunks, within a size bound
#'
#' A live response is read from its connection as it arrives. A complete
#' response (as replayed by `httptest2` or `httr2` mocks) is written as is.
#' A declared `Content-Length` over the bound stops the transfer before any
#' of the body is read; otherwise the bytes are counted as they arrive.
#'
#' @param resp An `httr2` response from [httr2::req_perform_connection()].
#' @param dest Destination path.
#' @param bound Size bound from `download_bound()`.
#'
#' @return `dest`.
#' @keywords internal
#' @noRd
dv_write_body <- function(resp, dest, bound = download_bound(NA)) {
  declared <- suppressWarnings(
    as.numeric(httr2::resp_header(resp, "Content-Length", default = NA))
  )
  if (isTRUE(declared > bound$limit)) {
    abort_download_size(bound)
  }

  con <- file(dest, open = "wb")
  on.exit(close(con), add = TRUE)
  if (is.raw(resp$body)) {
    if (length(resp$body) > bound$limit) abort_download_size(bound)
    writeBin(resp$body, con)
    return(dest)
  }
  received <- 0
  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = 256)
    if (length(chunk) == 0L) {
      if (httr2::resp_stream_is_complete(resp)) break
      next
    }
    received <- received + length(chunk)
    if (received > bound$limit) abort_download_size(bound)
    writeBin(chunk, con)
  }
  dest
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
  req <- dv_request(
    server, "dataverses", dv_path_segment(collection), "contents"
  )
  contents <- dv_get_json(req,
    what = glue::glue("listing of collection {collection}"),
    server = server, quiet = quiet
  )

  datasets <- Filter(function(x) identical(x$type, "dataset"), contents)

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
  req <- dv_request(
    server, "datasets", ":persistentId", "versions", dv_path_segment(version)
  )
  # Only the version numbers are needed; the file list is fetched separately.
  req <- httr2::req_url_query(req, persistentId = doi, excludeFiles = "true")
  ds <- dv_get_json(req,
    what = glue::glue("metadata for {doi}"),
    server = server, quiet = quiet
  )
  major <- ds$versionNumber %||% ds$majorVersion
  minor <- ds$versionMinorNumber %||% ds$minorVersion

  if (is.null(major)) {
    return(as.character(version))
  }
  paste0(major, ".", minor %||% 0L)
}

#' API key for a Dataverse host, if the user has one
#'
#' `DATAVERSE_KEY` is sent only to the configured server
#' (`getOption("gtropic.server")`). A manifest can name another host, and the
#' token must not leak to it; for any other host an empty key is returned and,
#' if a token is set, a warning says so once per host and session.
#'
#' @param server Host the request is sent to.
#'
#' @return A character scalar: the token, or `""`.
#' @keywords internal
#' @noRd
dv_key <- function(server) {
  key <- Sys.getenv("DATAVERSE_KEY", unset = "")
  if (!nzchar(key)) {
    return("")
  }
  configured <- opt_server()
  if (!identical(tolower(server), tolower(configured))) {
    cli::cli_warn(
      c("Not sending {.envvar DATAVERSE_KEY} to {.val {server}}.",
        i = "Set {.code options(gtropic.server = \"{server}\")} to authenticate
             against it."
      ),
      .frequency = "once",
      .frequency_id = paste0("gtropic_dv_key_", tolower(server))
    )
    return("")
  }
  key
}

#' Is a string a hexadecimal digest of a supported length?
#'
#' MD5, SHA-1, SHA-256 and SHA-512 digests are 32, 40, 64 and 128 hexadecimal
#' characters. Checksums are used to build cache file names, so nothing else
#' (in particular no path separator or `..`) may pass.
#'
#' @param x Character vector.
#' @return Logical vector, `FALSE` for `NA`.
#' @keywords internal
#' @noRd
is_hex_checksum <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("^[0-9a-fA-F]+$", x) &
    nchar(x) %in% c(32L, 40L, 64L, 128L)
}

#' Identify the hashing algorithm used by a published checksum
#'
#' Prefer the type in a current file listing; otherwise infer it from the
#' standard hexadecimal digest length.
#'
#' @param checksum Published checksum value.
#' @param checksum_type Optional algorithm published alongside `checksum`.
#' @keywords internal
#' @noRd
checksum_algorithm <- function(checksum, checksum_type = NA_character_) {
  if (!is.na(checksum_type) && nzchar(checksum_type)) {
    type <- toupper(gsub("[-_ ]", "", checksum_type))
    algorithms <- c(MD5 = "md5", SHA1 = "sha1", SHA256 = "sha256", SHA512 = "sha512")
    if (!type %in% names(algorithms)) {
      cli::cli_abort("Unsupported Dataverse checksum algorithm {.val {checksum_type}}.")
    }
    return(unname(algorithms[[type]]))
  }

  if (is.null(checksum) || is.na(checksum) || !nzchar(checksum)) {
    return("md5")
  }

  n <- nchar(checksum, type = "chars")
  if (identical(n, 32L)) return("md5")
  if (identical(n, 40L)) return("sha1")
  if (identical(n, 64L)) return("sha256")
  if (identical(n, 128L)) return("sha512")
  cli::cli_abort(
    "Unsupported published checksum with {n} hexadecimal characters."
  )
}

#' Hash a downloaded file using the algorithm of a published checksum
#'
#' The file is read in chunks by the hashing code, never into an R vector.
#'
#' @param path File to hash.
#' @param checksum Published checksum value.
#' @param checksum_type Optional declared checksum algorithm.
#' @keywords internal
#' @noRd
checksum_file <- function(path, checksum = NULL, checksum_type = NA_character_) {
  digest::digest(
    path, algo = checksum_algorithm(checksum, checksum_type), file = TRUE
  )
}

#' Verify a downloaded file against the published checksum
#'
#' @param path The downloaded file.
#' @param expected_checksum Checksum recorded in the Dataverse listing.
#' @param checksum_type Optional checksum algorithm recorded in the listing.
#' @param label Path used in messages.
#' @param call Calling environment.
#'
#' @return Invisibly `TRUE`; aborts on mismatch.
#' @keywords internal
#' @noRd
verify_checksum <- function(path, expected_checksum, label = "file",
                            call = rlang::caller_env(),
                            checksum_type = NA_character_) {
  if (is.na(expected_checksum) || !nzchar(expected_checksum)) {
    # Nothing published to check against; not an error, but worth knowing.
    cli::cli_warn(
      c("No checksum published for {.path {label}}; integrity not verified.")
    )
    return(invisible(TRUE))
  }

  got <- checksum_file(path, expected_checksum, checksum_type)
  if (!identical(tolower(got), tolower(expected_checksum))) {
    gtropic_abort(
      c("Checksum mismatch for {.path {label}}.",
        x = "Expected {.val {expected_checksum}} but the download hashed to
             {.val {got}}.",
        i = "The download was likely truncated. Try again; if it persists the
             published file may have changed."
      ),
      class = "gtropic_error_checksum",
      call = call
    )
  }

  invisible(TRUE)
}

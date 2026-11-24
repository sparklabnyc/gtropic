# Helpers for the fetcher tests (test-fetcher.R) and the live tests
# (test-live-dataverse.R).

#' Skip unless live Dataverse tests were requested
#'
#' Live tests contact the configured Dataverse server. They never run on CRAN
#' and run elsewhere only with `GTROPIC_LIVE_TESTS=true` (the weekly live
#' workflow sets it).
#' @noRd
skip_if_no_live_tests <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(
    !identical(Sys.getenv("GTROPIC_LIVE_TESTS"), "true"),
    "Live Dataverse tests run only with GTROPIC_LIVE_TESTS=true"
  )
  testthat::skip_if_offline(opt_server())
}

# Hash bytes the way the package hashes a downloaded file.
checksum_raw <- function(raw, checksum = NULL, checksum_type = NA_character_) {
  digest::digest(
    raw, algo = checksum_algorithm(checksum, checksum_type), serialize = FALSE
  )
}

# Read a whole file back as bytes.
read_bytes <- function(path) {
  readBin(path, "raw", n = file.size(path))
}

# Write bytes to a temporary file that is removed with the calling frame.
local_bytes_file <- function(bytes, env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = env)
  writeBin(bytes, path)
  path
}

# A stand-in for dv_get_file(): "download" bytes to `dest`.
serve_bytes <- function(bytes, dest) {
  writeBin(bytes, dest)
  invisible(dest)
}

# Store bytes in the cache the way a verified download is stored.
cache_put_bytes <- function(cache, key, bytes, checksum = NA_character_, ...) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  cache_put(cache, key, path, checksum, ...)
}

fetcher_listing <- function(years = 2001:2005, directory = "yearly") {
  bytes <- lapply(years, function(year) {
    charToRaw(sprintf('{"year":%d}', year))
  })
  listing <- tibble::tibble(
    file_id = seq_along(years),
    label = paste0(years, ".parquet"),
    directory_label = directory,
    checksum = vapply(bytes, checksum_raw, character(1)),
    checksum_type = "MD5",
    size_bytes = vapply(bytes, length, integer(1)),
    content_type = "application/octet-stream"
  )
  list(listing = listing, bytes = bytes)
}

fetcher_discovery <- function(listing, current = NULL) {
  years <- listing_years(listing)
  historical <- list(
    doi = "doi:10.0000/HIST",
    version = "1.0",
    listing = listing,
    years = years
  )
  out <- list(
    server = "example.invalid",
    historical = historical,
    current = current,
    year_index = tibble::tibble(
      year = years,
      dataset = rep("historical", length(years))
    )
  )
  out
}

fetcher_cache <- function(dir) {
  list(enabled = TRUE, dir = dir)
}

fetcher_key <- function(version = "1.0", label = "2001.parquet") {
  list(
    server = "example.invalid",
    doi = "doi:10.0000/HIST",
    version = version,
    directory_label = "yearly",
    label = label
  )
}

# --- a local stand-in for a Dataverse server --------------------------------

#' Serve a webfakes app in place of the Dataverse host
#'
#' Requests that would go to `https://<server>/api` go to the app instead, and
#' retries wait 0.05 s instead of 1 s or more. Skipped on CRAN: the app runs
#' in a separate process.
#' @noRd
local_dataverse_app <- function(app, env = parent.frame()) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("webfakes")
  # Several threads, so a deliberately stalled response does not hold up the
  # next request.
  web <- webfakes::local_app_process(app,
    opts = webfakes::server_opts(num_threads = 4),
    .local_envir = env
  )
  testthat::local_mocked_bindings(
    dv_api_url = function(server) sub("/$", "", web$url("/api")),
    dv_backoff = function(i) 0.05,
    .env = env
  )
  web
}

#' A webfakes app with a few Dataverse-like routes
#'
#' `GET /hits/<name>` reports how often route `<name>` was requested.
#' @noRd
dataverse_test_app <- function() {
  app <- webfakes::new_app()
  app$locals$hits <- list()
  count <- function(req, name) {
    n <- req$app$locals$hits[[name]]
    n <- if (is.null(n)) 1L else n + 1L
    req$app$locals$hits[[name]] <- n
    n
  }
  dv_json <- function(res, status, body) {
    res$set_status(status)$set_type("application/json")$send(body)
  }
  app$get("/hits/:name", function(req, res) {
    n <- req$app$locals$hits[[req$params$name]]
    res$send_json(if (is.null(n)) 0L else n, auto_unbox = TRUE)
  })
  # 503 the first time, then an empty collection.
  app$get("/api/dataverses/flaky/contents", function(req, res) {
    if (count(req, "flaky") == 1L) {
      dv_json(res, 503L, '{"status":"ERROR","message":"Try again later"}')
    } else {
      dv_json(res, 200L, '{"status":"OK","data":[]}')
    }
  })
  # 3 MB sent in chunks, with no Content-Length.
  app$get("/api/access/datafile/99", function(req, res) {
    res$set_type("application/octet-stream")
    for (i in 1:3) res$send_chunk(as.raw(rep(1L, 1024^2)))
  })
  # Answers after 3 s.
  app$get("/api/dataverses/slow/contents", function(req, res) {
    if (is.null(res$locals$waited)) {
      res$locals$waited <- TRUE
      return(res$delay(3))
    }
    dv_json(res, 200L, '{"status":"OK","data":[]}')
  })
  # Sends 1 byte, then nothing for 3 s. (curl averages the transfer speed
  # over a few seconds, so a larger first chunk would hide a short stall.)
  app$get("/api/access/datafile/98", function(req, res) {
    if (is.null(res$locals$sent)) {
      res$locals$sent <- TRUE
      res$set_type("application/octet-stream")
      res$send_chunk(as.raw(1))
      return(res$delay(3))
    }
    res$send_chunk(as.raw(2))
  })
  # Sends 5 chunks of 100 bytes, 0.4 s apart: slow but steady.
  app$get("/api/access/datafile/97", function(req, res) {
    n <- res$locals$n
    n <- if (is.null(n)) 0L else n
    if (n == 0L) res$set_type("application/octet-stream")
    res$send_chunk(as.raw(rep(n + 1L, 100)))
    res$locals$n <- n + 1L
    if (n < 4L) res$delay(0.4)
  })
  app$get("/api/dataverses/missing/contents", function(req, res) {
    count(req, "missing")
    dv_json(res, 404L, '{"status":"ERROR","message":"Collection not found"}')
  })
  app
}

#' How often a route of `dataverse_test_app()` was requested
#' @noRd
app_hits <- function(web, name) {
  as.integer(httr2::resp_body_json(
    httr2::req_perform(httr2::request(web$url(paste0("/hits/", name))))
  ))
}

#' A mocked streaming response, as `httr2::req_perform_connection()` returns
#' @noRd
streaming_response <- function(req, bytes, headers = list()) {
  httr2::new_response(
    "GET", req$url, 200L,
    headers = headers,
    body = httr2::StreamingBody$new(rawConnection(bytes))
  )
}

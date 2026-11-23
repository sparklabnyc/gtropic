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

# dataverse 0.3.16 builds requests with httr. This test-only transport keeps
# dataverse's URL construction and parsing while making the final call through
# httr2, which allows httptest2 to record and replay the API responses.
dataverse_httptest_transport <- function(url, ..., key = NULL, as = "text") {
  dots <- list(...)
  request <- httr2::request(url)

  if (!is.null(dots$query) && length(dots$query) > 0L) {
    request <- do.call(httr2::req_url_query, c(list(request), dots$query))
  }
  if (!is.null(key) && nzchar(key)) {
    request <- httr2::req_headers(request, `X-Dataverse-key` = key)
  }

  response <- httr2::req_perform(request)
  httr2::resp_check_status(response)
  if (identical(as, "raw")) {
    httr2::resp_body_raw(response)
  } else {
    httr2::resp_body_string(response)
  }
}

local_dataverse_recordings <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    api_get_impl = dataverse_httptest_transport,
    api_get_session_cache = dataverse_httptest_transport,
    api_get_disk_cache = dataverse_httptest_transport,
    .package = "dataverse",
    .env = .env
  )
  withr::local_options(
    list(DATAVERSE_USE_CACHE = "none", gtropic.max_tries = 1L),
    .local_envir = .env
  )
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

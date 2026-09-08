skip_fetcher_integration <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline("demo.dataverse.org")
}

sanitize_mock_url <- function(req) {
  mock <- httptest2::build_mock_url(req)
  mock <- gsub("/-persistentId-", "/persistentId-", mock, fixed = TRUE)
  mock <- gsub("/-latest", "/latest", mock, fixed = TRUE)
  mock
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

with_fetcher_mock_dir <- function(expr) {
  eval.parent(substitute(expr))
}

fetcher_listing <- function(years = 2001:2005, directory = "yearly") {
  bytes <- lapply(years, function(year) {
    charToRaw(sprintf('{"year":%d}', year))
  })
  listing <- tibble::tibble(
    file_id = seq_along(years),
    label = paste0(years, ".parquet"),
    directory_label = directory,
    md5 = vapply(bytes, md5_raw, character(1)),
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
    server = "demo.dataverse.org",
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
    server = "demo.dataverse.org",
    doi = "doi:10.0000/HIST",
    version = version,
    directory_label = "yearly",
    label = label
  )
}

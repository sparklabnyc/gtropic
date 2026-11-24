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

# Cache integrity and on-disk format (Plan 05 SEC-04, SEC-06, SEC-09).

# A checksum that walks out of `blobs/` and is exactly 32 characters long, so
# it also looks like an MD5 digest to the hashing code.
traversal_checksum <- function() paste0("../", strrep("v", 29L))

tampered_index <- function(dir, checksum) {
  key <- fetcher_key()
  tibble::tibble(
    server = key$server, doi = key$doi, version = key$version,
    directory_label = key$directory_label, label = key$label,
    checksum = checksum, size_bytes = 8,
    fetched_at = as.POSIXct("2000-01-01", tz = "UTC")
  )
}

test_that("cache_blob_path() accepts only hexadecimal digests", {
  dir <- withr::local_tempdir()
  md5 <- strrep("A", 32L)
  expect_identical(
    cache_blob_path(dir, md5),
    file.path(dir, "blobs", paste0(strrep("a", 32L), ".blob"))
  )
  expect_identical(cache_blob_path(dir, character(0)), character(0))
  for (bad in c(traversal_checksum(), "", NA, strrep("g", 32L), "/etc/passwd")) {
    expect_error(cache_blob_path(dir, bad), "invalid checksum")
  }
})

test_that("a tampered index cannot make the cache read or delete other files", {
  dir <- withr::local_tempdir()
  victim <- file.path(dir, paste0(strrep("v", 29L), ".blob"))
  writeLines("not a cache file", victim)
  cache_index_write(dir, tampered_index(dir, traversal_checksum()))
  cache <- fetcher_cache(dir)
  key <- fetcher_key()

  got <- try(suppressWarnings(cache_get(cache, key)), silent = TRUE)
  expect_false(is.raw(got))
  expect_true(file.exists(victim))

  expect_false(isTRUE(try(cache_has(cache, key), silent = TRUE)))

  withr::local_options(gtropic.cache_dir = dir)
  try(
    suppressWarnings(suppressMessages(gtropic_cache_prune(older_than_days = 0))),
    silent = TRUE
  )
  expect_true(file.exists(victim))
})

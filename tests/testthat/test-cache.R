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
  expect_false(is.character(got))
  expect_true(file.exists(victim))

  expect_false(isTRUE(try(cache_has(cache, key), silent = TRUE)))

  withr::local_options(gtropic.cache_dir = dir)
  try(
    suppressWarnings(suppressMessages(gtropic_cache_prune(older_than_days = 0))),
    silent = TRUE
  )
  expect_true(file.exists(victim))
})

# --- JSON metadata (SEC-04) ---------------------------------------------------

populated_index <- function() {
  tibble::tibble(
    server = c("example.invalid", "dataverse.harvard.edu"),
    doi = c("doi:10.0000/HIST", "doi:10.7910/DVN/EAVWC3"),
    version = c("1.0", "14.2"),
    directory_label = c("yearly", "02_wind/exposures"),
    label = c("2001.parquet", "2005.parquet"),
    checksum = c(strrep("a", 32L), strrep("b", 64L)),
    size_bytes = c(10, 12345678901),
    fetched_at = as.POSIXct(c("2026-01-02 03:04:05", "2025-12-31 23:59:59"), tz = "UTC")
  )
}

write_index_json <- function(dir, text) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(text, file.path(dir, "index.json"))
}

test_that("the cache index round-trips through index.json", {
  dir <- withr::local_tempdir()
  idx <- populated_index()

  cache_index_write(dir, idx)

  expect_true(file.exists(file.path(dir, "index.json")))
  expect_identical(cache_index_read(dir), idx)
  expect_identical(attr(cache_index_read(dir)$fetched_at, "tzone"), "UTC")
  expect_identical(cache_index_read(withr::local_tempdir()), cache_index_empty())
})

test_that("an invalid index.json reads as an empty index, with a warning", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  good <- jsonlite::toJSON(
    within(as.data.frame(populated_index()), fetched_at <- format_cache_time(fetched_at)),
    dataframe = "rows", digits = NA
  )
  bad <- list(
    malformed = "{not json",
    not_array = '{"server": "example.invalid"}',
    wrong_type = sub('"size_bytes":10', '"size_bytes":"10"', good, fixed = TRUE),
    missing_field = sub('"doi":"doi:10.0000/HIST",', "", good, fixed = TRUE),
    traversal = sub(strrep("a", 32L), "../../x", good, fixed = TRUE),
    bad_server = sub("example.invalid", "evil.example/x", good, fixed = TRUE),
    bad_time = sub("2026-01-02T03:04:05Z", "yesterday", good, fixed = TRUE),
    mixed_types = sub('"label":"2001.parquet"', '"label":5', good, fixed = TRUE)
  )
  for (nm in names(bad)) {
    dir <- withr::local_tempdir()
    write_index_json(dir, bad[[nm]])
    expect_warning(idx <- cache_index_read(dir), "not valid gtropic metadata", label = nm)
    expect_identical(idx, cache_index_empty(), label = nm)
  }
})

test_that("unknown fields in index.json are dropped", {
  dir <- withr::local_tempdir()
  rows <- as.data.frame(populated_index())
  rows$fetched_at <- format_cache_time(rows$fetched_at)
  rows$extra <- "ignored"
  write_index_json(dir, jsonlite::toJSON(rows, dataframe = "rows", digits = NA))

  expect_identical(cache_index_read(dir), populated_index())
})

test_that("old R serialisation files in the cache are never read", {
  dir <- withr::local_tempdir()
  flag <- withr::local_tempfile()
  # A file whose deserialisation would have side effects is simply ignored.
  saveRDS(populated_index(), file.path(dir, "index.rds"))
  saveRDS(list(discovery = "x", saved_at = Sys.time()), file.path(dir, "discovery.rds"))

  expect_identical(cache_index_read(dir), cache_index_empty())
  expect_error(
    cache_discovery_read(dir, "example.invalid", "x", list(historical = ":latest")),
    "needs a persisted discovery snapshot"
  )
  expect_false(file.exists(flag))
})

test_that("cache hits report the download time stored in the index", {
  dir <- withr::local_tempdir()
  cache <- fetcher_cache(dir)
  bytes <- charToRaw("bytes")
  stored <- cache_put_bytes(cache, fetcher_key(), bytes, checksum_raw(bytes),
    downloaded_at = as.POSIXct("2026-01-02 03:04:05.75", tz = "UTC")
  )$downloaded_at

  expect_identical(stored, as.POSIXct("2026-01-02 03:04:05", tz = "UTC"))
  expect_identical(cache_get_record(cache, fetcher_key())$downloaded_at, stored)
})

test_that("an invalid discovery snapshot cannot be used offline", {
  dir <- withr::local_tempdir()
  discovery <- fetcher_discovery(fetcher_listing()$listing)
  cache_discovery_write(dir, discovery, "x")
  path <- file.path(dir, "discovery.json")
  good <- readLines(path, warn = FALSE)
  read <- function() {
    cache_discovery_read(dir, "example.invalid", "x", list(historical = ":latest"))
  }
  expect_true(is.list(read()))

  edits <- list(
    server = c("example.invalid", "evil.example/x"),
    checksum = c(fetcher_listing()$listing$checksum[1], "../../x"),
    dataset = c('"dataset":"historical"', '"dataset":"elsewhere"'),
    malformed = c("{", "[")
  )
  for (nm in names(edits)) {
    writeLines(sub(edits[[nm]][1], edits[[nm]][2], good, fixed = TRUE), path)
    expect_error(read(), "unreadable", label = nm)
  }
})

test_that("the persisted cache setting is JSON and validated on read", {
  config_dir <- withr::local_tempdir()
  local_mocked_bindings(cache_config_dir = function() config_dir)
  withr::local_options(rlib_warning_verbosity = "verbose")

  cache_config_write(FALSE)
  expect_identical(
    jsonlite::read_json(file.path(config_dir, "config.json")),
    list(cache_enabled = FALSE)
  )
  expect_false(cache_config_read())

  writeLines('{"cache_enabled": "yes"}', file.path(config_dir, "config.json"))
  expect_warning(expect_null(cache_config_read()), "not valid gtropic metadata")
  writeLines("not json", file.path(config_dir, "config.json"))
  expect_warning(expect_null(cache_config_read()), "not valid gtropic metadata")
})

test_that("cache directories are private to the user", {
  skip_on_os("windows")
  dir <- file.path(withr::local_tempdir(), "cache")
  cache_ensure_dirs(dir)
  mode <- function(p) format(file.info(p)$mode)
  expect_identical(mode(dir), "700")
  expect_identical(mode(file.path(dir, "blobs")), "700")
})

test_that("a cache directory others can write to is reported once", {
  skip_on_os("windows")
  withr::local_options(rlib_warning_verbosity = "verbose")
  dir <- withr::local_tempdir()
  Sys.chmod(dir, "0777", use_umask = FALSE)
  withr::local_options(gtropic.cache_dir = dir, gtropic.cache_enabled = TRUE)

  expect_warning(cache_resolve(), "writable by other users")
  Sys.chmod(dir, "0700", use_umask = FALSE)
  expect_no_warning(cache_resolve())
})

# --- temporary files (SEC-09) -------------------------------------------------

list_tmp <- function(dir) {
  list.files(dir, pattern = "tmp", all.files = TRUE, recursive = TRUE)
}

test_that("a file planted at a predictable temp name is not written through", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  victim <- withr::local_tempfile(lines = "keep me")
  dir.create(file.path(dir, "blobs"), recursive = TRUE)
  file.symlink(victim, file.path(dir, "index.json.tmp"))
  bytes <- charToRaw("bytes")
  checksum <- checksum_raw(bytes)
  file.symlink(victim, file.path(dir, "blobs", paste0(checksum, ".blob.tmp")))

  cache_put_bytes(fetcher_cache(dir), fetcher_key(), bytes, checksum)

  expect_identical(readLines(victim), "keep me")
  expect_identical(cache_index_read(dir)$checksum, checksum)
  expect_identical(read_bytes(cache_get(fetcher_cache(dir), fetcher_key())), bytes)
})

test_that("writes leave no temporary files behind", {
  dir <- withr::local_tempdir()
  bytes <- charToRaw("bytes")
  cache_put_bytes(fetcher_cache(dir), fetcher_key(), bytes, checksum_raw(bytes))
  cache_discovery_write(dir, fetcher_discovery(fetcher_listing()$listing), "x")

  expect_length(list_tmp(dir), 0L)
})

test_that("a failed rename is reported and cleaned up", {
  skip_on_os("windows")
  withr::local_options(rlib_warning_verbosity = "verbose")
  dir <- withr::local_tempdir()
  # A directory in the way makes the final rename fail.
  dir.create(file.path(dir, "index.json"), recursive = TRUE)

  expect_warning(cache_index_write(dir, populated_index()), "Could not update the cache index")
  expect_length(list_tmp(dir), 0L)
})

test_that("stale temporary files are removed when the cache is next written", {
  dir <- withr::local_tempdir()
  blobs <- file.path(dir, "blobs")
  dir.create(blobs)
  stale <- file.path(c(dir, blobs), ".tmp-stale")
  fresh <- file.path(dir, ".tmp-fresh")
  file.create(c(stale, fresh))
  Sys.setFileTime(stale, Sys.time() - 2 * 86400)
  rm(list = ls(.gtropic_session, pattern = "^tmp_swept"), envir = .gtropic_session)

  bytes <- charToRaw("bytes")
  cache_put_bytes(fetcher_cache(dir), fetcher_key(), bytes, checksum_raw(bytes))

  expect_false(any(file.exists(stale)))
  expect_true(file.exists(fresh))
})

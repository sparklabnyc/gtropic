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
  expect_null(got)
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
  expect_identical(cache_get(cache, fetcher_key())$downloaded_at, stored)
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
  expect_identical(read_bytes(cache_get(fetcher_cache(dir), fetcher_key())$path), bytes)
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

# --- one index read and write per query (PERF-04) ----------------------------

# Counts reads and writes of index.json.
local_index_io_counter <- function(env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$reads <- 0L
  counter$writes <- 0L
  read <- cache_index_read
  write <- cache_index_write
  local_mocked_bindings(
    cache_index_read = function(...) {
      counter$reads <- counter$reads + 1L
      read(...)
    },
    cache_index_write = function(...) {
      counter$writes <- counter$writes + 1L
      write(...)
    },
    .env = env
  )
  counter
}

test_that("a query reads the index once, and writes it at most once", {
  local_golden_collection()
  withr::local_options(gtropic.cache_dir = withr::local_tempdir())
  query <- function() {
    gtropic_data(
      STORM_ID = "2002BBB", tables = c("wind", "precip", "storm_metadata"),
      cache = TRUE, quiet = TRUE, confirm = FALSE
    )
  }

  cold <- local_index_io_counter()
  query()
  # Read once when first used, and once more to merge the query's entries
  # into whatever is on disk by then; written once.
  expect_identical(c(cold$reads, cold$writes), c(2L, 1L))

  warm <- local_index_io_counter()
  query()
  expect_identical(c(warm$reads, warm$writes), c(1L, 0L))
})

test_that("an interrupted query still records what it downloaded", {
  log <- local_golden_collection()
  dir <- withr::local_tempdir()
  withr::local_options(gtropic.cache_dir = dir)
  local_mocked_bindings(build_links = function(...) stop("interrupted"))

  expect_error(
    gtropic_data(
      STORM_ID = "2002BBB", tables = c("wind", "storm_metadata"),
      cache = TRUE, quiet = TRUE, confirm = FALSE
    ),
    "interrupted"
  )

  idx <- cache_index_read(dir)
  expect_gt(length(log$downloads), 0L)
  expect_identical(nrow(idx), length(log$downloads))
  expect_true(all(file.exists(cache_blob_path(dir, idx$checksum))))
})

index_entry <- function(label, checksum, version = "1.0") {
  tibble::tibble(
    server = "example.invalid", doi = "doi:10.0000/HIST", version = version,
    directory_label = "yearly", label = label, checksum = checksum,
    size_bytes = 1, fetched_at = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

test_that("flushing a query's index keeps entries other sessions added", {
  dir <- withr::local_tempdir()
  cache <- fetcher_cache(dir)
  cache$index <- cache_index_handle(dir)
  a <- strrep("a", 32L)
  b <- strrep("b", 32L)

  expect_identical(nrow(cache_index_get(cache)), 0L)
  cache_index_upsert(cache, index_entry("2001.parquet", a))
  # Meanwhile another R session stores a file.
  cache_index_write(dir, index_entry("2002.parquet", b))
  cache_index_flush(cache)

  expect_setequal(cache_index_read(dir)$label, c("2001.parquet", "2002.parquet"))
})

test_that("a query's drops and upserts reach the index in the order made", {
  dir <- withr::local_tempdir()
  a <- strrep("a", 32L)
  b <- strrep("b", 32L)
  cache_index_write(dir, dplyr::bind_rows(
    index_entry("2001.parquet", a), index_entry("2002.parquet", b)
  ))
  cache <- fetcher_cache(dir)
  cache$index <- cache_index_handle(dir)

  # Stored, then found corrupt: gone.
  cache_index_upsert(cache, index_entry("2003.parquet", a))
  cache_index_drop(cache, a)
  # Found corrupt, then downloaded again: present.
  cache_index_drop(cache, b)
  cache_index_upsert(cache, index_entry("2002.parquet", b))
  expect_identical(cache_index_get(cache)$label, "2002.parquet")
  cache_index_flush(cache)

  expect_identical(cache_index_read(dir)$label, "2002.parquet")
  expect_false(cache_index_flush(cache))
})

test_that("the download estimate checks the whole plan in one lookup", {
  dir <- withr::local_tempdir()
  cache <- fetcher_cache(dir)
  fixture <- fetcher_listing(2001:2004)
  discovery <- fetcher_discovery(fixture$listing)
  # 2001 cached with the listed checksum; 2002 cached with other bytes.
  cache_put_bytes(cache, fetcher_key(label = "2001.parquet"),
    fixture$bytes[[1]], fixture$listing$checksum[1]
  )
  other <- charToRaw("other")
  cache_put_bytes(cache, fetcher_key(label = "2002.parquet"), other, checksum_raw(other))
  plan <- tibble::tibble(
    dataset = c("historical", "historical", "historical", "current"),
    directory_label = "yearly",
    label = c("2001.parquet", "2002.parquet", "1999.parquet", "2003.parquet"),
    size_bytes = c(10, 20, 30, 40)
  )
  lookups <- 0L
  has <- cache_has
  local_mocked_bindings(cache_has = function(...) {
    lookups <<- lookups + 1L
    has(...)
  })

  estimate <- estimate_download(plan, cache, discovery)

  expect_identical(lookups, 1L)
  expect_identical(estimate$n_cached, 1L)
  expect_identical(estimate$n_files, 3L)
  expect_identical(estimate$bytes, 90)
})

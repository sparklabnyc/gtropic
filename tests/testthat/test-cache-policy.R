test_that("caching is enabled by default without creating a cache at load", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = NULL,
    gtropic.cache_dir = cache_dir
  ))
  local_mocked_bindings(cache_config_read = function() NULL)

  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_true(getOption("gtropic.cache_enabled"))
  expect_false(dir.exists(cache_dir))
})

test_that("cache = FALSE bypasses cache reads and writes", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = TRUE,
    gtropic.cache_dir = cache_dir
  ))

  resolved <- cache_resolve(FALSE)

  expect_identical(resolved, list(enabled = FALSE, dir = NULL))
  expect_false(dir.exists(cache_dir))
})

test_that("resolving the default cache does not create it", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = TRUE,
    gtropic.cache_dir = cache_dir
  ))

  resolved <- cache_resolve()

  expect_identical(resolved, list(enabled = TRUE, dir = cache_dir))
  expect_false(dir.exists(cache_dir))
})

test_that("cache enable and disable persist only when requested", {
  root <- withr::local_tempdir()
  cache_dir <- file.path(root, "cache")
  config_dir <- file.path(root, "config")
  withr::local_options(list(
    gtropic.cache_enabled = FALSE,
    gtropic.cache_dir = cache_dir
  ))
  local_mocked_bindings(cache_config_dir = function() config_dir)

  expect_silent(gtropic_cache_enable())
  expect_true(getOption("gtropic.cache_enabled"))
  expect_false(dir.exists(config_dir))

  expect_silent(gtropic_cache_disable(persist = TRUE))
  expect_false(getOption("gtropic.cache_enabled"))
  expect_true(file.exists(file.path(config_dir, "config.json")))
  expect_identical(cache_config_read(), FALSE)

  withr::local_options(list(gtropic.cache_enabled = NULL))
  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_false(getOption("gtropic.cache_enabled"))

  withr::local_options(list(gtropic.cache_enabled = TRUE))
  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_true(getOption("gtropic.cache_enabled"))

  expect_silent(gtropic_cache_enable(persist = TRUE))
  expect_identical(cache_config_read(), TRUE)
})

test_that("cache settings validate their inputs", {
  expect_error(gtropic_cache_enable(persist = NA), "persist")
  expect_error(gtropic_cache_disable(persist = "yes"), "persist")
  expect_error(gtropic_cache_enable(dir = ""), "dir")
})

test_that("gtropic_codebook forwards its cache override", {
  seen_cache <- NULL
  local_mocked_bindings(
    helper_context = function(version, cache) {
      seen_cache <<- cache
      list()
    },
    fetch_file = function(...) list()
  )

  gtropic_codebook(cache = FALSE)

  expect_false(seen_cache)
})

# --- automatic management (CRAN-11) -----------------------------------------

local_cache_index <- function(rows, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  cache_ensure_dirs(dir)
  idx <- cache_index_empty()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    checksum <- if ("checksum" %in% names(r)) {
      r$checksum
    } else {
      checksum_raw(charToRaw(paste(r$label, r$version)))
    }
    writeBin(charToRaw(strrep("x", r$size_bytes)), cache_blob_path(dir, checksum))
    idx <- dplyr::bind_rows(idx, tibble::tibble(
      server = "example.invalid", doi = "doi:10.0000/HIST",
      version = r$version, directory_label = "yearly", label = r$label,
      checksum = checksum, size_bytes = r$size_bytes,
      fetched_at = cache_time(r$fetched_at)
    ))
  }
  cache_index_write(dir, idx)
  dir
}

days_ago <- function(n) Sys.time() - n * 86400

test_that("entries older than gtropic.cache_max_age_days are removed", {
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2002.parquet"), version = "1.0",
    size_bytes = 10, fetched_at = c(days_ago(200), days_ago(1))
  ))
  old_blob <- cache_blob_path(dir, cache_index_read(dir)$checksum[1])
  withr::local_options(gtropic.cache_max_age_days = 180, gtropic.cache_max_gb = Inf)

  removed <- cache_manage(dir, quiet = TRUE)

  expect_identical(removed$label, "2001.parquet")
  expect_identical(cache_index_read(dir)$label, "2002.parquet")
  expect_false(file.exists(old_blob))
})

test_that("Inf switches each rule off", {
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2002.parquet"), version = "1.0",
    size_bytes = 100, fetched_at = c(days_ago(2000), days_ago(1))
  ))
  withr::local_options(gtropic.cache_max_age_days = Inf, gtropic.cache_max_gb = Inf)
  expect_identical(nrow(cache_manage(dir, quiet = TRUE)), 0L)
  expect_identical(nrow(cache_index_read(dir)), 2L)
})

test_that("the least recently downloaded entries go first beyond gtropic.cache_max_gb", {
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2002.parquet", "2003.parquet"), version = "1.0",
    size_bytes = 100, fetched_at = c(days_ago(3), days_ago(1), days_ago(2))
  ))
  withr::local_options(
    gtropic.cache_max_age_days = Inf, gtropic.cache_max_gb = 250 / 1024^3
  )

  removed <- cache_manage(dir, quiet = TRUE)

  expect_identical(removed$label, "2001.parquet")
  expect_setequal(cache_index_read(dir)$label, c("2002.parquet", "2003.parquet"))
})

test_that("entries superseded by a :latest retrieval are removed", {
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2001.parquet", "2002.parquet", "2003.parquet"),
    version = c("1.0", "2.0", "1.0", "unparsed"),
    size_bytes = 10, fetched_at = days_ago(1)
  ))
  withr::local_options(gtropic.cache_max_age_days = Inf, gtropic.cache_max_gb = Inf)
  latest <- tibble::tibble(
    server = "example.invalid", doi = "doi:10.0000/HIST",
    directory_label = "yearly", label = c("2001.parquet", "2003.parquet"),
    version = "2.0"
  )

  removed <- cache_manage(dir, latest = latest, quiet = TRUE)

  expect_identical(paste(removed$label, removed$version), "2001.parquet 1.0")
  left <- cache_index_read(dir)
  expect_setequal(
    paste(left$label, left$version),
    c("2001.parquet 2.0", "2002.parquet 1.0", "2003.parquet unparsed")
  )
})

test_that("files used by the retrieval are never removed", {
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2002.parquet"), version = "1.0",
    size_bytes = 100, fetched_at = c(days_ago(400), days_ago(1))
  ))
  withr::local_options(
    gtropic.cache_max_age_days = 180, gtropic.cache_max_gb = 50 / 1024^3
  )
  keep <- cache_key_string(
    "example.invalid", "doi:10.0000/HIST", "yearly", "2001.parquet", "1.0"
  )

  removed <- cache_manage(dir, keep = keep, quiet = TRUE)

  expect_identical(removed$label, "2002.parquet")
  expect_identical(cache_index_read(dir)$label, "2001.parquet")
})

test_that("a blob shared with a remaining entry is kept", {
  shared <- checksum_raw(charToRaw("shared"))
  dir <- local_cache_index(tibble::tibble(
    label = c("2001.parquet", "2001.parquet"), version = c("1.0", "2.0"),
    checksum = shared, size_bytes = 10, fetched_at = days_ago(1)
  ))
  withr::local_options(gtropic.cache_max_age_days = Inf, gtropic.cache_max_gb = Inf)
  latest <- tibble::tibble(
    server = "example.invalid", doi = "doi:10.0000/HIST",
    directory_label = "yearly", label = "2001.parquet", version = "2.0"
  )
  removed <- cache_manage(dir, latest = latest, quiet = TRUE)
  expect_identical(removed$version, "1.0")
  expect_true(file.exists(cache_blob_path(dir, shared)))
})

test_that("the first cached file of a session is announced once", {
  withr::defer(.gtropic_session$cache_notice_shown <- NULL)
  .gtropic_session$cache_notice_shown <- NULL
  cache <- fetcher_cache(withr::local_tempdir())
  raw <- charToRaw("bytes")

  expect_no_message(cache_put(cache, fetcher_key(label = "2001.parquet"), raw, quiet = TRUE))
  expect_message(
    cache_put(cache, fetcher_key(label = "2002.parquet"), raw),
    "Caching downloads in"
  )
  expect_no_message(cache_put(cache, fetcher_key(label = "2003.parquet"), raw))
})

test_that("a retrieval that writes to the cache prunes outdated versions", {
  log <- local_golden_collection()
  dir <- withr::local_tempdir()
  withr::local_options(
    gtropic.cache_dir = dir, gtropic.cache_max_age_days = Inf,
    gtropic.cache_max_gb = Inf
  )
  query <- function() {
    gtropic_data(
      STORM_ID = "2002BBB", tables = c("wind", "storm_metadata"),
      cache = TRUE, quiet = TRUE, confirm = FALSE
    )
  }
  first <- query()
  used <- gtropic_manifest(first)$files
  # Pretend an earlier version of one of those files is cached too.
  idx <- cache_index_read(dir)
  # A historical-dataset file (golden version 3.0), cached again at 1.0.
  stale <- idx[idx$version == golden_version[["historical"]], ][1, ]
  stale$version <- "1.0"
  stale$checksum <- checksum_raw(charToRaw("stale"))
  writeBin(charToRaw("stale"), cache_blob_path(dir, stale$checksum))
  cache_index_write(dir, dplyr::bind_rows(idx, stale))
  # Forget one other used file so the next retrieval writes to the cache again.
  idx <- cache_index_read(dir)
  forget <- which(idx$version != "1.0" | idx$doi != stale$doi)[2]
  cache_index_write(dir, idx[-forget, ])

  second <- query()

  left <- cache_index_read(dir)
  expect_false("1.0" %in% left$version[left$label == stale$label &
    left$directory_label == stale$directory_label &
    left$doi == stale$doi])
  expect_false(file.exists(cache_blob_path(dir, stale$checksum)))
  expect_true(all(paste(used$directory_label, used$label) %in%
    paste(left$directory_label, left$label)))
})

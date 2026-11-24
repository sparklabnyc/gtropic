# Fetcher tests that need no network: every transport call is mocked.
# Tests against the live Dataverse server are in test-live-dataverse.R.

test_that("yearly population rows are tagged with their source file year", {
  context <- list(
    discovery = list(
      year_index = tibble::tibble(
        year = 2025:2026,
        dataset = c("historical", "current")
      )
    )
  )

  local_mocked_bindings(
    fetch_file = function(dataset, directory_label, label, ctx, ...) {
      tibble::tibble(ADM2_ID = paste0(dataset, "-", label))
    }
  )

  pop <- fetch_yearly("pop", 2025:2026, context, year_col = "YEAR")

  expect_equal(pop$YEAR, 2025:2026)
  expect_equal(
    pop$ADM2_ID,
    c("historical-2025.parquet", "current-2026.parquet")
  )
})

test_that("failed automatic discovery warns when the configured DOIs work", {
  fallback <- list(source = "configured options")
  local_mocked_bindings(
    discover_datasets_impl = function(...) stop("collection unavailable"),
    discovery_from_options = function(...) fallback
  )

  expect_warning(
    result <- discover_datasets(refresh = TRUE, quiet = TRUE),
    "fell back to the configured DOI options"
  )
  expect_identical(result, fallback)
})

test_that("a fallback that fails too is reported once, as an error", {
  local_mocked_bindings(
    discover_datasets_impl = function(...) stop("collection unavailable"),
    discovery_from_options = function(...) {
      gtropic_abort("No connection.", class = "gtropic_error_network")
    }
  )

  expect_no_warning(expect_error(
    discover_datasets(refresh = TRUE, quiet = TRUE),
    "No connection",
    class = "gtropic_error_network"
  ))
})

test_that("checksum verification rejects changed bytes", {
  bytes <- charToRaw("published bytes")

  expect_invisible(verify_checksum(
    local_bytes_file(bytes), checksum_raw(bytes), "fixture.bin"
  ))
  expect_error(
    verify_checksum(
      local_bytes_file(charToRaw("changed bytes")), checksum_raw(bytes),
      "fixture.bin"
    ),
    "Checksum mismatch"
  )
})

test_that("checksum verification supports Dataverse SHA-256 digests", {
  bytes <- charToRaw("published bytes")
  checksum <- digest::digest(bytes, algo = "sha256", serialize = FALSE)

  expect_identical(checksum_algorithm(checksum), "sha256")
  expect_identical(checksum_algorithm(checksum, "SHA-256"), "sha256")
  expect_invisible(verify_checksum(
    local_bytes_file(bytes), checksum, "fixture.bin", checksum_type = "SHA-256"
  ))
  expect_error(
    verify_checksum(
      local_bytes_file(charToRaw("changed bytes")), checksum, "fixture.bin"
    ),
    "Checksum mismatch"
  )
})

test_that("a corrupt cached blob is invalidated", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- checksum_raw(bytes)
  cache_put_bytes(cache, key, bytes, checksum)

  blob <- file.path(cache_dir, "blobs", paste0(checksum, ".blob"))
  writeBin(charToRaw("corrupt"), blob)

  expect_warning(
    result <- cache_get(cache, key, expected_checksum = checksum),
    "failed checksum verification"
  )
  expect_null(result)
  expect_false(file.exists(blob))
  expect_false(cache_has(cache, key))
})

# Counts the hashes cache_get() computes, by wrapping checksum_file().
local_hash_counter <- function(env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  hash <- checksum_file
  local_mocked_bindings(
    checksum_file = function(...) {
      counter$n <- counter$n + 1L
      hash(...)
    },
    .env = env
  )
  counter
}

test_that("a cached blob is hashed once per session, not on every use", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- checksum_raw(bytes)
  cache_put_bytes(cache, key, bytes, checksum)
  # A blob this session did not store itself must be verified on first use.
  cache_blob_forget(cache_blob_path(cache_dir, checksum))
  hashes <- local_hash_counter()

  for (i in 1:3) {
    expect_identical(read_bytes(cache_get(cache, key, checksum)$path), bytes)
  }
  expect_identical(hashes$n, 1L)
})

test_that("a blob stored after a verified download needs no hash on first use", {
  cache <- fetcher_cache(withr::local_tempdir())
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- checksum_raw(bytes)
  cache_put_bytes(cache, key, bytes, checksum)
  hashes <- local_hash_counter()

  expect_identical(read_bytes(cache_get(cache, key, checksum)$path), bytes)
  expect_identical(hashes$n, 0L)
})

test_that("a verified blob that changes on disk is verified again", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- checksum_raw(bytes)
  cache_put_bytes(cache, key, bytes, checksum)
  blob <- cache_blob_path(cache_dir, checksum)
  expect_false(is.null(cache_get(cache, key, checksum)))

  # Same size, so only the modification time gives the change away.
  writeBin(charToRaw("other bytes"), blob)
  Sys.setFileTime(blob, Sys.time() + 60)

  expect_warning(
    result <- cache_get(cache, key, checksum),
    "failed checksum verification"
  )
  expect_null(result)
})

test_that("a session's verification holds only for the checksum it checked", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- checksum_raw(bytes)
  cache_put_bytes(cache, key, bytes, checksum)

  # The listing now publishes a different checksum for the same entry.
  other <- checksum_raw(charToRaw("new bytes"))
  expect_warning(
    expect_null(cache_get(cache, key, expected_checksum = other)),
    "failed checksum verification"
  )
})

test_that("a cached SHA-256 blob is verified using SHA-256", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  cache_put_bytes(cache, key, bytes, checksum)

  expect_identical(
    read_bytes(cache_get(cache, key, expected_checksum = checksum)$path), bytes
  )
})

test_that("cold, warm, and overlapping fetches download only missing years", {
  fixture <- fetcher_listing()
  discovery <- fetcher_discovery(fixture$listing)
  cache <- fetcher_cache(withr::local_tempdir())
  calls <- integer(0)

  local_mocked_bindings(
    dv_get_file = function(file_id, ..., dest) {
      calls <<- c(calls, file_id)
      serve_bytes(fixture$bytes[[file_id]], dest)
    }
  )

  fetch_year <- function(year) {
    context <- fetch_context(discovery, cache, quiet = TRUE)
    on.exit(cache_manage_ctx(context), add = TRUE)
    fetch_file(
      "historical", "yearly", paste0(year, ".parquet"), context,
      kind = "json"
    )
  }

  first <- lapply(2001:2003, fetch_year)
  warm <- lapply(2002:2003, fetch_year)
  overlap <- lapply(2002:2005, fetch_year)

  expect_equal(vapply(first, `[[`, integer(1), "year"), 2001:2003)
  expect_equal(vapply(warm, `[[`, integer(1), "year"), 2002:2003)
  expect_equal(vapply(overlap, `[[`, integer(1), "year"), 2002:2005)
  expect_equal(calls, 1:5)
})

test_that("content-addressed cache deduplicates bytes across versions", {
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  bytes <- charToRaw("same immutable file")
  checksum <- checksum_raw(bytes)

  cache_put_bytes(cache, fetcher_key(version = "1.0"), bytes, checksum)
  cache_put_bytes(cache, fetcher_key(version = "2.0"), bytes, checksum)

  expect_equal(nrow(cache_index_read(cache_dir)), 2L)
  expect_length(list.files(file.path(cache_dir, "blobs")), 1L)
  expect_identical(
    cache_get(cache, fetcher_key(version = "1.0"), checksum)$path,
    cache_get(cache, fetcher_key(version = "2.0"), checksum)$path
  )
})

test_that("metadata remains historical when current listing contains a copy", {
  historical_listing <- tibble::tibble(
    file_id = c(1L, 2L),
    label = c("2001.parquet", "codebook.json"),
    directory_label = c("yearly", "metadata"),
    checksum = c(checksum_raw(charToRaw("year")), checksum_raw(charToRaw('{"source":"historical"}'))),
    checksum_type = "MD5",
    size_bytes = c(4, 23),
    content_type = c("application/octet-stream", "application/json")
  )
  current_listing <- tibble::tibble(
    file_id = c(3L, 4L),
    label = c("2002.parquet", "codebook.json"),
    directory_label = c("yearly", "metadata"),
    checksum = c(checksum_raw(charToRaw("year")), checksum_raw(charToRaw('{"source":"current"}'))),
    checksum_type = "MD5",
    size_bytes = c(4, 20),
    content_type = c("application/octet-stream", "application/json")
  )

  local_mocked_bindings(
    dv_list_datasets = function(...) {
      tibble::tibble(doi = c("doi:10.0000/HIST", "doi:10.0000/CURR"))
    },
    dv_list_files = function(doi, ...) {
      if (identical(doi, "doi:10.0000/HIST")) historical_listing else current_listing
    },
    dv_version_label = function(doi, ...) {
      if (identical(doi, "doi:10.0000/HIST")) "4.0" else "2.0"
    },
    dv_get_file = function(file_id, ..., dest) {
      serve_bytes(charToRaw(if (identical(file_id, 2L)) {
        '{"source":"historical"}'
      } else {
        '{"source":"current"}'
      }), dest)
    }
  )

  warnings <- character(0)
  discovery <- withCallingHandlers(
    discover_datasets_impl(
      list(historical = ":latest", current = ":latest"),
      "example.invalid", "gtropic", TRUE
    ),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("current-year dataset now contains", warnings)))
  context <- fetch_context(
    discovery,
    list(enabled = FALSE, dir = NULL),
    quiet = TRUE
  )
  codebook <- fetch_file(
    "historical", "metadata", "codebook.json", context,
    kind = "json"
  )
  manifest <- context$manifest_out$value

  expect_equal(codebook$source, "historical")
  expect_equal(manifest$files$dataset_doi, "doi:10.0000/HIST")
  expect_equal(manifest$files$source_role, "Historical")
  expect_equal(manifest$files$dataset_version, "4.0")
  expect_equal(manifest$files$file_id, 2L)
})

test_that("unreachable servers produce a contextual Dataverse error", {
  skip_if_not_installed("httptest2")

  expect_error(
    httptest2::without_internet(
      dv_list_files(
        "doi:10.0000/UNREACHABLE",
        server = "unreachable.invalid",
        quiet = TRUE
      )
    ),
    "Could not complete file listing.*Common causes",
    class = "rlang_error"
  )
})

discovery_listing <- function(file_ids, years = 2001:2002) {
  labels <- c(paste0(years, ".parquet"), "codebook.json")
  tibble::tibble(
    file_id = as.integer(file_ids),
    label = labels,
    directory_label = c(rep("yearly", length(years)), "metadata"),
    checksum = vapply(paste(labels, file_ids), function(x) {
      checksum_raw(charToRaw(x))
    }, character(1), USE.NAMES = FALSE),
    checksum_type = "MD5",
    size_bytes = 10,
    content_type = "application/octet-stream"
  )
}

test_that("a pinned version lists the files of that version", {
  latest <- discovery_listing(901:903)
  pinned <- discovery_listing(101:103)
  local_mocked_bindings(
    dv_list_datasets = function(...) tibble::tibble(doi = "doi:10.0000/HIST"),
    dv_list_files = function(doi, version = ":latest", ...) {
      if (identical(version, "1.0")) pinned else latest
    },
    dv_version_label = function(doi, version = ":latest", ...) {
      if (identical(version, ":latest")) "2.0" else version
    }
  )

  discovery <- discover_datasets(
    list(historical = "1.0", current = ":latest"),
    server = "example.invalid", collection = "gtropic",
    refresh = TRUE, quiet = TRUE
  )

  expect_identical(discovery$historical$version, "1.0")
  expect_identical(discovery$historical$listing$file_id, 101:103)
  expect_identical(discovery$historical$listing$checksum, pinned$checksum)
  expect_identical(discovery$historical$years, 2001:2002)
})

test_that("a pinned version can cover different years than :latest", {
  latest <- discovery_listing(901:904, years = 2001:2003)
  pinned <- discovery_listing(101:102, years = 2001)
  local_mocked_bindings(
    dv_list_datasets = function(...) tibble::tibble(doi = "doi:10.0000/HIST"),
    dv_list_files = function(doi, version = ":latest", ...) {
      if (identical(version, "1.0")) pinned else latest
    },
    dv_version_label = function(doi, version = ":latest", ...) "1.0"
  )

  discovery <- discover_datasets_impl(
    list(historical = "1.0", current = ":latest"),
    "example.invalid", "gtropic", TRUE
  )

  expect_identical(discovery$historical$years, 2001L)
  expect_identical(discovery$year_index$year, 2001L)
})

test_that(":latest discovery lists each dataset once", {
  calls <- character(0)
  local_mocked_bindings(
    dv_list_datasets = function(...) {
      tibble::tibble(doi = c("doi:10.0000/HIST", "doi:10.0000/CURR"))
    },
    dv_list_files = function(doi, version = ":latest", ...) {
      calls <<- c(calls, paste(doi, version))
      if (identical(doi, "doi:10.0000/HIST")) {
        discovery_listing(1:3)
      } else {
        # The current-year dataset: one year-file, no metadata/ copy.
        discovery_listing(4:5, years = 2003)[1, ]
      }
    },
    dv_version_label = function(doi, ...) "1.0"
  )

  discover_datasets_impl(
    list(historical = ":latest", current = ":latest"),
    "example.invalid", "gtropic", TRUE
  )

  expect_identical(
    sort(calls),
    c("doi:10.0000/CURR :latest", "doi:10.0000/HIST :latest")
  )
})

test_that("the listed size bounds a download and nothing is cached", {
  listing <- fetcher_listing(2001)$listing
  listing$size_bytes <- 4
  discovery <- fetcher_discovery(listing)
  dir <- withr::local_tempdir()
  cache <- fetcher_cache(dir)
  httr2::local_mocked_responses(function(req) {
    httr2::response(200, body = as.raw(rep(1L, 2 * 1024^2)))
  })
  context <- fetch_context(discovery, cache, quiet = TRUE)

  expect_error(
    fetch_file("historical", "yearly", "2001.parquet", context, kind = "json"),
    class = "gtropic_error_download_size"
  )
  expect_identical(nrow(cache_index_read(dir)), 0L)
  expect_length(
    list.files(file.path(dir, "blobs"), all.files = TRUE, no.. = TRUE), 0L
  )
})

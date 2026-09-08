test_that("file resolution uses directory and label together", {
  skip_fetcher_integration()
  local_dataverse_recordings()

  with_fetcher_mock_dir({
    listing <- dv_list_files(
      "doi:10.70122/FK2/SWEYST",
      server = "demo.dataverse.org",
      quiet = TRUE
    )
  })

  paths <- c(
    "02_wind/exposures",
    "02_wind/zero_pairs",
    "02_wind/storm_metadata",
    "02_wind/storm_tracks"
  )
  resolved <- lapply(paths, function(path) {
    dv_resolve_file(listing, path, "1980.parquet")
  })

  expect_true(all(vapply(resolved, nrow, integer(1)) == 1L))
  expect_equal(
    vapply(resolved, `[[`, integer(1), "file_id"),
    c(2703825L, 2703963L, 2703871L, 2703917L)
  )
  expect_equal(vapply(resolved, `[[`, character(1), "directory_label"), paths)
})

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

test_that("current-year discovery identifies both datasets and year routing", {
  skip_fetcher_integration()
  local_dataverse_recordings()

  with_fetcher_mock_dir({
    discovery <- discover_datasets(
      server = "demo.dataverse.org",
      collection = "gtropic",
      refresh = TRUE,
      quiet = TRUE
    )
  })

  expect_equal(discovery$historical$doi, "doi:10.70122/FK2/SWEYST")
  expect_equal(discovery$current$doi, "doi:10.70122/FK2/DDSYYD")
  expect_equal(discovery$historical$years, 1980:2025)
  expect_equal(discovery$current$years, 2026L)
  expect_equal(
    discovery$year_index,
    tibble::tibble(
      year = 1980:2026,
      dataset = c(rep("historical", 46L), "current")
    )
  )
})

test_that("failed automatic discovery warns before using configured DOIs", {
  skip_fetcher_integration()
  fallback <- list(source = "configured options")
  local_mocked_bindings(
    discover_datasets_impl = function(...) stop("collection unavailable"),
    discovery_from_options = function(...) fallback
  )

  expect_warning(
    result <- discover_datasets(refresh = TRUE, quiet = TRUE),
    "falling back to the configured DOI options"
  )
  expect_identical(result, fallback)
})

test_that("MD5 verification rejects changed bytes", {
  skip_fetcher_integration()
  bytes <- charToRaw("published bytes")

  expect_invisible(verify_md5(bytes, md5_raw(bytes), "fixture.bin"))
  expect_error(
    verify_md5(charToRaw("changed bytes"), md5_raw(bytes), "fixture.bin"),
    "Checksum mismatch"
  )
})

test_that("a corrupt cached blob is invalidated", {
  skip_fetcher_integration()
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  key <- fetcher_key()
  bytes <- charToRaw("valid bytes")
  checksum <- md5_raw(bytes)
  cache_put(cache, key, bytes, checksum)

  blob <- file.path(cache_dir, "blobs", paste0(checksum, ".blob"))
  writeBin(charToRaw("corrupt"), blob)

  expect_warning(
    result <- cache_get(cache, key, expected_md5 = checksum),
    "failed checksum verification"
  )
  expect_null(result)
  expect_false(file.exists(blob))
  expect_false(cache_has(cache, key))
})

test_that("cold, warm, and overlapping fetches download only missing years", {
  skip_fetcher_integration()
  fixture <- fetcher_listing()
  discovery <- fetcher_discovery(fixture$listing)
  cache <- fetcher_cache(withr::local_tempdir())
  calls <- integer(0)

  local_mocked_bindings(
    dv_get_file = function(file_id, ...) {
      calls <<- c(calls, file_id)
      fixture$bytes[[file_id]]
    }
  )

  fetch_year <- function(year) {
    context <- fetch_context(discovery, cache, quiet = TRUE)
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
  skip_fetcher_integration()
  cache_dir <- withr::local_tempdir()
  cache <- fetcher_cache(cache_dir)
  bytes <- charToRaw("same immutable file")
  checksum <- md5_raw(bytes)

  cache_put(cache, fetcher_key(version = "1.0"), bytes, checksum)
  cache_put(cache, fetcher_key(version = "2.0"), bytes, checksum)

  expect_equal(nrow(cache_index_read(cache_dir)), 2L)
  expect_length(list.files(file.path(cache_dir, "blobs")), 1L)
  expect_identical(
    cache_get(cache, fetcher_key(version = "1.0"), checksum),
    cache_get(cache, fetcher_key(version = "2.0"), checksum)
  )
})

test_that("manifest replay retrieves byte-identical recorded input", {
  skip_fetcher_integration()
  local_dataverse_recordings()
  downloaded <- list()
  recorded_download <- dv_get_file
  local_mocked_bindings(
    dv_get_file = function(...) {
      raw <- recorded_download(...)
      downloaded[[length(downloaded) + 1L]] <<- raw
      raw
    }
  )

  with_fetcher_mock_dir({
    discovery <- discover_datasets(
      server = "demo.dataverse.org",
      collection = "gtropic",
      refresh = TRUE,
      quiet = TRUE
    )
    first_context <- fetch_context(
      discovery,
      list(enabled = FALSE, dir = NULL),
      quiet = TRUE
    )
    first <- fetch_file(
      "current", "02_wind/storm_metadata", "2026.parquet",
      first_context
    )
    manifest <- manifest_finalize(first_context$manifest_out$value)

    replay_context <- fetch_context(
      discovery,
      list(enabled = FALSE, dir = NULL),
      manifest_in = manifest,
      quiet = TRUE
    )
    replay <- fetch_file(
      "current", "02_wind/storm_metadata", "2026.parquet",
      replay_context
    )
  })

  expect_identical(
    tibble::as_tibble(dplyr::collect(first)),
    tibble::as_tibble(dplyr::collect(replay))
  )
  expect_length(downloaded, 2L)
  expect_identical(downloaded[[1L]], downloaded[[2L]])
  expect_equal(nrow(manifest$files), 1L)
  expect_equal(manifest$files$file_id, 2703775L)
  expect_equal(manifest$files$md5, md5_raw(downloaded[[1L]]))
})

test_that("metadata remains historical when current listing contains a copy", {
  skip_fetcher_integration()
  historical_listing <- tibble::tibble(
    file_id = c(1L, 2L),
    label = c("2001.parquet", "codebook.json"),
    directory_label = c("yearly", "metadata"),
    md5 = c(md5_raw(charToRaw("year")), md5_raw(charToRaw('{"source":"historical"}'))),
    size_bytes = c(4, 23),
    content_type = c("application/octet-stream", "application/json")
  )
  current_listing <- tibble::tibble(
    file_id = c(3L, 4L),
    label = c("2002.parquet", "codebook.json"),
    directory_label = c("yearly", "metadata"),
    md5 = c(md5_raw(charToRaw("year")), md5_raw(charToRaw('{"source":"current"}'))),
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
    dv_get_file = function(file_id, ...) {
      if (identical(file_id, 2L)) {
        charToRaw('{"source":"historical"}')
      } else {
        charToRaw('{"source":"current"}')
      }
    }
  )

  warnings <- character(0)
  discovery <- withCallingHandlers(
    discover_datasets_impl(
      list(historical = ":latest", current = ":latest"),
      "demo.dataverse.org", "gtropic", TRUE
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
  expect_equal(manifest$files$dataset_version, "4.0")
  expect_equal(manifest$files$file_id, 2L)
})

test_that("unreachable servers produce a contextual Dataverse error", {
  skip_fetcher_integration()
  local_dataverse_recordings()

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

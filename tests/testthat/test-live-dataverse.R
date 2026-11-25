# Live tests against the configured Dataverse server.
#
# Never run on CRAN. Elsewhere they run only with GTROPIC_LIVE_TESTS=true and a
# reachable server (skip_if_no_live_tests()). They check that the published
# collection still has the shape the package relies on, so they make a handful
# of small requests and download only small metadata files.

test_that("live: discovery identifies the historical and current-year datasets", {
  skip_if_no_live_tests()

  discovery <- discover_datasets(refresh = TRUE, quiet = TRUE)
  hist <- discovery$historical

  expect_identical(hist$doi, opt("doi_historical"))
  expect_identical(min(hist$years), 1980L)
  expect_identical(hist$years, seq(min(hist$years), max(hist$years)))
  expect_true(any(startsWith(hist$listing$directory_label, "metadata")))

  current_years <- setdiff(discovery$current$years %||% integer(0), hist$years)
  if (!is.null(discovery$current)) {
    expect_true(all(discovery$current$years > max(hist$years)))
  }
  expect_identical(discovery$year_index$year, c(hist$years, current_years))
  expect_identical(
    discovery$year_index$dataset,
    rep(
      c("historical", "current"),
      c(length(hist$years), length(current_years))
    )
  )
})

test_that("live: files resolve by directory and label together", {
  skip_if_no_live_tests()

  listing <- dv_list_files(opt("doi_historical"), quiet = TRUE)
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
  expect_identical(
    vapply(resolved, `[[`, character(1), "directory_label"),
    paths
  )
  # The same label exists in every directory, so the ids must all differ.
  expect_length(
    unique(vapply(resolved, `[[`, integer(1), "file_id")),
    length(paths)
  )
  expect_true(all(nzchar(vapply(resolved, `[[`, character(1), "checksum"))))
})

test_that("live: manifest replay retrieves byte-identical input", {
  skip_if_no_live_tests()
  downloaded <- list()
  live_download <- dv_get_file
  local_mocked_bindings(
    dv_get_file = function(...) {
      path <- live_download(...)
      downloaded[[length(downloaded) + 1L]] <<- read_bytes(path)
      invisible(path)
    }
  )
  no_cache <- list(enabled = FALSE, dir = NULL)
  path <- c("02_wind/storm_metadata", "1980.parquet")

  discovery <- discover_datasets(refresh = TRUE, quiet = TRUE)
  recorded <- dv_resolve_file(
    discovery$historical$listing,
    path[1],
    path[2],
    doi = discovery$historical$doi
  )
  first_context <- fetch_context(discovery, no_cache, quiet = TRUE)
  first <- fetch_file("historical", path[1], path[2], first_context)
  manifest <- manifest_finalize(ctx_manifest(first_context))

  replay_context <- fetch_context(
    discovery,
    no_cache,
    manifest_in = manifest,
    quiet = TRUE
  )
  replay <- fetch_file("historical", path[1], path[2], replay_context)

  expect_identical(
    tibble::as_tibble(dplyr::collect(first)),
    tibble::as_tibble(dplyr::collect(replay))
  )
  expect_length(downloaded, 2L)
  expect_identical(downloaded[[1L]], downloaded[[2L]])
  expect_identical(manifest$files$file_id, recorded$file_id)
  expect_identical(manifest$files$checksum, recorded$checksum)
  expect_identical(
    manifest$files$checksum,
    checksum_raw(downloaded[[1L]], recorded$checksum)
  )
})

test_that("live: every directory and metadata file the planner reads is published", {
  skip_if_no_live_tests()

  listing <- dv_list_files(opt("doi_historical"), quiet = TRUE)
  for (spec in TABLE_SPEC) {
    in_dir <- listing$directory_label == spec$dir
    if (isTRUE(spec$yearly)) {
      expect_true(
        any(in_dir & grepl("^[0-9]{4}\\.parquet$", listing$label)),
        label = paste("year-files in", spec$dir)
      )
    } else {
      expect_identical(
        sum(in_dir & listing$label == spec$label),
        1L,
        label = paste0(spec$dir, "/", spec$label)
      )
    }
  }
})

test_that("live: storm metadata has the columns the storm filters use", {
  skip_if_no_live_tests()

  discovery <- discover_datasets(refresh = TRUE, quiet = TRUE)
  ctx <- fetch_context(
    discovery,
    list(enabled = FALSE, dir = NULL),
    quiet = TRUE
  )
  x <- tibble::as_tibble(dplyr::collect(
    fetch_file("historical", "02_wind/storm_metadata", "1980.parquet", ctx)
  ))

  expect_gt(nrow(x), 0L)
  expect_type(x$STORM_ID, "character")
  expect_type(x$STORM_NAME, "character")
  expect_false(anyNA(x$STORM_ID))
  expect_false(anyDuplicated(x$STORM_ID) > 0L)
  # Published as ISO text or as Arrow dates, depending on the year.
  genesis <- x$STORM_GENESIS_DATE_UTC
  expect_true(inherits(genesis, "Date") || is.character(genesis))
  expect_false(anyNA(as.Date(genesis)))
  expect_true(all(format(as.Date(genesis), "%Y") == "1980"))
})

test_that("live: gtropic_storms() finds Hurricane Katrina", {
  skip_if_no_live_tests()
  withr::local_options(gtropic.quiet = TRUE)

  katrina <- gtropic_storms(STORM_NAME = "Katrina-2005")
  expect_identical(nrow(katrina), 1L)
  expect_true(nzchar(katrina$STORM_ID))
  expect_s3_class(katrina$STORM_GENESIS_DATE_UTC, "Date")
  expect_identical(format(katrina$STORM_GENESIS_DATE_UTC, "%Y"), "2005")
})

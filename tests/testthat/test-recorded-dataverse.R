# Replays responses recorded from Harvard Dataverse (Plan 06 CRAN-09 item 3).
#
# The recordings in tests/testthat/dv/ were made by
# data-raw/record-http-mocks.R and are stored under the test host "dv.test".
# They let discovery, listing parsing, version labels, downloads and checksum
# verification run on the real response formats with no network. Any request
# without a recording is an error, so nothing here can reach a server.

local_recorded_dataverse <- function(env = parent.frame()) {
  skip_if_not_installed("httptest2")
  withr::local_options(
    httptest2.mock.paths = test_path("dv"),
    gtropic.server = "dv.test",
    gtropic.collection = "gtropic",
    .local_envir = env
  )
}

test_that("discovery works on recorded Dataverse responses", {
  local_recorded_dataverse()
  discovery <- httptest2::with_mock_api(
    discover_datasets(refresh = TRUE, quiet = TRUE)
  )
  hist <- discovery$historical
  curr <- discovery$current

  expect_identical(discovery$server, "dv.test")
  expect_identical(hist$doi, "doi:10.7910/DVN/EAVWC3")
  expect_identical(curr$doi, "doi:10.7910/DVN/FZBAKO")
  expect_identical(hist$version, "1.0")
  expect_match(curr$version, "^[0-9]+[.][0-9]+$")
  expect_identical(hist$years, c(1980L, 2007L, 2025L))
  expect_identical(curr$years, 2026L)
  expect_identical(discovery$year_index$year, c(1980L, 2007L, 2025L, 2026L))

  listing <- hist$listing
  expect_named(listing, names(dv_empty_listing()))
  expect_true(all(is_hex_checksum(listing$checksum)))
  expect_true(all(listing$checksum_type == "MD5"))
  expect_true(all(listing$size_bytes > 0))
  expect_identical(
    sort(paste(listing$directory_label, listing$label, sep = "/")),
    c(
      "02_wind/excluded_storms/2007.parquet",
      "02_wind/storm_metadata/1980.parquet",
      "02_wind/storm_metadata/2025.parquet",
      "metadata/codebook.json"
    )
  )
})

test_that("recorded downloads verify, cache and read", {
  local_recorded_dataverse()
  dir <- withr::local_tempdir()
  httptest2::with_mock_api({
    discovery <- discover_datasets(refresh = TRUE, quiet = TRUE)
    ctx <- fetch_context(discovery, fetcher_cache(dir), quiet = TRUE)
    hist <- fetch_file("historical", "02_wind/excluded_storms", "2007.parquet", ctx)
    curr <- fetch_file("current", "02_wind/excluded_storms", "2026.parquet", ctx)
    cache_manage_ctx(ctx)
  })

  expect_s3_class(hist, "Table")
  expect_true(all(c("STORM_ID", "STORM_NAME") %in% names(hist)))
  expect_true(all(c("STORM_ID", "STORM_NAME") %in% names(curr)))

  files <- ctx_manifest(ctx)$files
  expect_identical(files$dataset_doi, c(discovery$historical$doi, discovery$current$doi))
  listed <- rbind(discovery$historical$listing, discovery$current$listing)
  expected <- listed[match(files$file_id, listed$file_id), ]
  expect_identical(files$checksum, expected$checksum)
  expect_identical(files$size_bytes, expected$size_bytes)

  # Both files are now cached: reading them again makes no request.
  idx <- cache_index_read(dir)
  expect_identical(nrow(idx), 2L)
  httptest2::without_internet({
    ctx <- fetch_context(discovery, fetcher_cache(dir), quiet = TRUE)
    again <- fetch_file("historical", "02_wind/excluded_storms", "2007.parquet", ctx)
  })
  expect_identical(again$num_rows, hist$num_rows)
})

test_that("recordings are small, portable and carry no credentials", {
  dir <- test_path("dv")
  rel <- list.files(dir, recursive = TRUE)
  expect_gt(length(rel), 0L)
  # Plan 04 PERF-15: committed HTTP fixtures stay under 200 KB in all.
  expect_lt(sum(file.size(file.path(dir, rel))), 200 * 1024)
  # Paths in the built tarball must not exceed 100 bytes.
  expect_true(all(nchar(file.path("gtropic", "tests", "testthat", "dv", rel)) <= 100))
  text <- unlist(lapply(file.path(dir, rel), readLines, warn = FALSE))
  expect_false(any(grepl(
    "X-Dataverse-key|X-Amz-|AKIA|JSESSIONID|AWSALB|Set-Cookie",
    text,
    ignore.case = TRUE
  )))
})

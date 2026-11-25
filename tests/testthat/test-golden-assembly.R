# Golden-output test (Plan 04 PERF-00; enforces invariant G6).
#
# For a fixed query against a fixed collection, every table gtropic_data()
# returns must stay identical -- values, column order, column types and row
# order -- through every performance and security change. The goldens in
# tests/testthat/golden/ were written before any such change. Rewrite them
# (GTROPIC_UPDATE_GOLDEN=true) only for a deliberate change to returned data,
# with a NEWS.md entry saying why.

golden_query <- function(...) {
  gtropic_data(..., cache = FALSE, quiet = TRUE, confirm = FALSE)
}

all_tables_but_geometry <- c(
  "wind",
  "precip",
  "flood",
  "pop",
  "adm2",
  "codebook",
  "zero_pairs",
  "tracks",
  "storm_metadata"
)

test_that("golden: a single storm, every table except geometry", {
  local_golden_collection()

  res <- golden_query(STORM_ID = "2002BBB", tables = all_tables_but_geometry)

  expect_golden(res, "single-storm")
})

test_that("golden: a warm cache returns the same tables as a download", {
  log <- local_golden_collection()
  cache_dir <- withr::local_tempdir()
  query <- function() {
    gtropic_data(
      STORM_ID = "2002BBB",
      tables = all_tables_but_geometry,
      cache = TRUE,
      quiet = TRUE,
      confirm = FALSE
    )
  }
  withr::local_options(gtropic.cache_dir = cache_dir)

  cold <- query()
  n_cold <- length(log$downloads)
  warm <- query()

  expect_gt(n_cold, 0L)
  expect_length(log$downloads, n_cold)
  expect_golden(cold, "single-storm")
  expect_golden(warm, "single-storm")
})

test_that("golden: a country and date range with precip and flood", {
  local_golden_collection()

  res <- golden_query(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "precip", "flood", "zero_pairs", "storm_metadata")
  )

  expect_golden(res, "country-date-range")
})

test_that("golden: zero_pairs read only for linkage gives the same tables", {
  local_golden_collection()

  # Not returning zero_pairs reads only the columns linkage needs (PERF-05);
  # every other table must match the query that returns it.
  res <- golden_query(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "precip", "flood", "storm_metadata")
  )

  expected <- readRDS(testthat::test_path("golden", "country-date-range.rds"))
  actual <- golden_tables(res)
  expect_identical(names(actual), setdiff(names(expected), "zero_pairs"))
  for (nm in names(actual)) {
    expect_identical(actual[[nm]], expected[[nm]], label = nm)
  }
})

test_that("golden: a polygon filter", {
  skip_if_not_installed("sf")
  local_golden_collection()
  polygon <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 124, ymin = 10, xmax = 126, ymax = 12),
    crs = 4326
  ))

  # Membership is evaluated over two years, which is reported as a message
  # (suppressed by quiet = TRUE); no unit changes membership, so no warning.
  expect_no_warning(
    res <- golden_query(
      polygon = polygon,
      date_range = c("2002-01-01", "2003-12-31"),
      tables = c("wind", "zero_pairs", "pop", "adm2", "storm_metadata")
    )
  )

  expect_golden(res, "polygon")
})

test_that("golden: manifest replay returns the original tables", {
  local_golden_collection()
  original <- golden_query(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "precip", "flood", "zero_pairs", "storm_metadata")
  )

  replay <- gtropic_data(
    manifest = gtropic_manifest(original),
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  )

  expect_golden(replay, "country-date-range")
  # The replay reuses the recorded ADM2 ids, so it may read fewer files, but
  # every file it reads is one the original recorded.
  cols <- c("directory_label", "label", "file_id", "checksum")
  replayed <- gtropic_manifest(replay)$files[cols]
  recorded <- gtropic_manifest(original)$files[cols]
  expect_gt(nrow(replayed), 0L)
  expect_equal(nrow(dplyr::anti_join(replayed, recorded, by = cols)), 0L)
})

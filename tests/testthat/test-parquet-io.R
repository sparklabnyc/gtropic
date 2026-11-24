# A parquet file with several row groups, so the per-row-group filter in
# read_parquet_file() is exercised across group boundaries.
local_row_group_parquet <- function(env = parent.frame()) {
  df <- data.frame(
    STORM_ID = rep(c("S1", "S2", NA, "S3"), times = 5),
    ADM2_ID = sprintf("A%02d", 20:1),
    VALUE = as.numeric(1:20),
    stringsAsFactors = FALSE
  )
  path <- withr::local_tempfile(fileext = ".parquet", .local_envir = env)
  tbl <- arrow::arrow_table(
    STORM_ID = arrow::Array$create(df$STORM_ID, type = arrow::large_utf8()),
    ADM2_ID = arrow::Array$create(df$ADM2_ID, type = arrow::large_utf8()),
    VALUE = df$VALUE
  )
  arrow::write_parquet(tbl, path, chunk_size = 3)
  list(path = path, df = df)
}

read_df <- function(...) as.data.frame(read_parquet_file(...))

test_that("row filters keep the same rows, in the same order, as filtering in R", {
  f <- local_row_group_parquet()
  reader <- arrow::ParquetFileReader$create(arrow::ReadableFile$create(f$path))
  expect_gt(reader$num_row_groups, 1L)

  keep <- list(STORM_ID = c("S3", "S1"), ADM2_ID = sprintf("A%02d", c(1:10, 19)))
  expected <- f$df[f$df$STORM_ID %in% keep$STORM_ID &
    f$df$ADM2_ID %in% keep$ADM2_ID, , drop = FALSE]
  rownames(expected) <- NULL

  expect_identical(read_df(f$path, keep = keep), expected)
})

test_that("row filters treat missing values like %in%", {
  f <- local_row_group_parquet()

  with_na <- read_df(f$path, keep = list(STORM_ID = c(NA, "S2")))
  expect_identical(with_na$VALUE, f$df$VALUE[f$df$STORM_ID %in% c(NA, "S2")])

  without_na <- read_df(f$path, keep = list(STORM_ID = "S2"))
  expect_false(anyNA(without_na$STORM_ID))
})

test_that("NULL filters and filters on absent columns are skipped silently", {
  f <- local_row_group_parquet()

  expect_no_warning(
    out <- read_df(f$path, keep = list(ADM2_ID = NULL, NOT_A_COLUMN = "x"))
  )
  expect_identical(out, f$df)
})

test_that("a filter column outside the projection is not returned", {
  f <- local_row_group_parquet()

  out <- read_df(f$path,
    col_select = c("VALUE", "ADM2_ID"),
    keep = list(STORM_ID = "S1")
  )

  expect_named(out, c("VALUE", "ADM2_ID"))
  expect_identical(out$VALUE, f$df$VALUE[f$df$STORM_ID %in% "S1"])
})

test_that("an empty value set returns no rows, with the file's column types", {
  f <- local_row_group_parquet()

  out <- read_parquet_file(f$path, keep = list(STORM_ID = character(0)))

  expect_identical(out$num_rows, 0L)
  expect_identical(names(out), names(f$df))
  expect_true(out$schema$STORM_ID$type == arrow::large_utf8())
})

test_that("projection still warns about columns the file does not have", {
  f <- local_row_group_parquet()

  expect_warning(
    out <- read_df(f$path, col_select = c("VALUE", "MISSING"), keep = list(STORM_ID = "S1")),
    "MISSING"
  )
  expect_named(out, "VALUE")
})

test_that("an unreadable file is reported as a parquet read error", {
  path <- withr::local_tempfile(fileext = ".parquet")
  writeLines("not parquet", path)

  expect_error(
    read_parquet_file(path, keep = list(STORM_ID = "S1"), label = "02_wind/x.parquet"),
    "Could not read"
  )
})

geoparquet_test_geometry <- function(crs) {
  geometry <- if (is.null(crs)) {
    sf::st_sfc(sf::st_point(c(-80, 25)))
  } else {
    sf::st_sfc(sf::st_point(c(-80, 25)), crs = crs)
  }
  sf::st_sf(
    ADM2_ID = "TEST.1",
    geometry = geometry
  )
}

test_that("GeoParquet reading restores the published WGS 84 CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("sfarrow")
  geometry <- geoparquet_test_geometry(NULL)
  local_mocked_bindings(
    st_read_parquet = function(...) geometry,
    .package = "sfarrow"
  )

  result <- read_geoparquet_file("geometry.parquet", "geometry.parquet")

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_identical(result$ADM2_ID, geometry$ADM2_ID)
  expect_equal(sf::st_coordinates(result), sf::st_coordinates(geometry))
})

test_that("GeoParquet reading normalizes a known CRS to WGS 84", {
  skip_if_not_installed("sf")
  skip_if_not_installed("sfarrow")
  expected <- geoparquet_test_geometry(4326)
  projected <- sf::st_transform(expected, 3857)
  local_mocked_bindings(
    st_read_parquet = function(...) projected,
    .package = "sfarrow"
  )

  result <- read_geoparquet_file("geometry.parquet", "geometry.parquet")

  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_equal(
    sf::st_coordinates(result),
    sf::st_coordinates(expected),
    tolerance = 1e-7
  )
})

test_that("plotting fixture retains an explicit geographic CRS", {
  skip_if_not_installed("sf")
  geometry <- plot_fixture()$geometry
  expect_equal(sf::st_crs(geometry)$epsg, 4326)
})

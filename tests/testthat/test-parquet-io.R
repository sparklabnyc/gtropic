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

# Writes a GeoParquet file: WKB geometry plus the `geo` metadata entry.
local_geoparquet <- function(geometry, crs, encoding = "WKB", chunk_size = NULL,
                             env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".parquet", .local_envir = env)
  wkb <- unclass(sf::st_as_binary(sf::st_geometry(geometry)))
  tbl <- arrow::arrow_table(
    ADM2_ID = geometry$ADM2_ID,
    geometry = arrow::Array$create(wkb, type = arrow::binary())
  )
  column <- list(encoding = encoding)
  column["crs"] <- list(crs)
  tbl$metadata$geo <- as.character(jsonlite::toJSON(
    list(primary_column = "geometry", columns = list(geometry = column)),
    auto_unbox = TRUE, null = "null"
  ))
  arrow::write_parquet(tbl, path, chunk_size = chunk_size)
  path
}

test_geometry <- function(crs = 4326) {
  sf::st_sf(
    ADM2_ID = sprintf("TEST.%d", 1:7),
    geometry = sf::st_sfc(lapply(1:7, function(i) {
      sf::st_point(c(-80 + i, 25))
    }), crs = crs)
  )
}

test_that("GeoParquet is read with the CRS its metadata records", {
  skip_if_not_installed("sf")
  expected <- test_geometry()
  path <- local_geoparquet(expected, sf::st_crs(4326)$wkt)

  result <- read_geoparquet_file(path, label = "geometry.parquet")

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_identical(result$ADM2_ID, expected$ADM2_ID)
  expect_equal(sf::st_coordinates(result), sf::st_coordinates(expected))
})

test_that("GeoParquet without a usable CRS is read as WGS 84", {
  skip_if_not_installed("sf")
  path <- local_geoparquet(test_geometry(), crs = NULL)

  result <- read_geoparquet_file(path, label = "geometry.parquet")

  expect_equal(sf::st_crs(result)$epsg, 4326)
})

test_that("GeoParquet in another CRS is transformed to WGS 84", {
  skip_if_not_installed("sf")
  expected <- test_geometry()
  projected <- sf::st_transform(expected, 3857)
  path <- local_geoparquet(projected, sf::st_crs(3857)$wkt)

  result <- read_geoparquet_file(path, label = "geometry.parquet")

  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_equal(
    sf::st_coordinates(result), sf::st_coordinates(expected),
    tolerance = 1e-7
  )
})

test_that("GeoParquet rows are filtered before geometries are built", {
  skip_if_not_installed("sf")
  whole <- test_geometry()
  path <- local_geoparquet(whole, sf::st_crs(4326)$wkt, chunk_size = 3)
  reader <- arrow::ParquetFileReader$create(arrow::ReadableFile$create(path))
  expect_gt(reader$num_row_groups, 1L)
  keep <- list(ADM2_ID = c("TEST.6", "TEST.2", "TEST.4", "NONE"))
  built <- 0L
  as_sfc <- sf::st_as_sfc
  local_mocked_bindings(
    st_as_sfc = function(x, ...) {
      built <<- built + length(x)
      as_sfc(x, ...)
    },
    .package = "sf"
  )

  result <- read_geoparquet_file(path, keep = keep, label = "geometry.parquet")

  expect_identical(built, 3L)
  # The same rows, order and row names as reading everything and subsetting.
  full <- read_geoparquet_file(path, label = "geometry.parquet")
  expect_identical(result, full[full$ADM2_ID %in% keep$ADM2_ID, , drop = FALSE])
  expect_identical(row.names(result), c("2", "4", "6"))
})

test_that("a file without GeoParquet metadata is a read error", {
  skip_if_not_installed("sf")
  path <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(ADM2_ID = "A"), path)

  expect_error(
    read_geoparquet_file(path, label = "geometry.parquet"),
    "Could not read .*geometry.parquet.* as GeoParquet"
  )
})

test_that("only WKB-encoded GeoParquet is accepted", {
  skip_if_not_installed("sf")
  path <- local_geoparquet(test_geometry(), sf::st_crs(4326)$wkt, encoding = "WKT")

  expect_error(read_geoparquet_file(path, label = "geometry.parquet"), "WKB")
})

test_that("plotting fixture retains an explicit geographic CRS", {
  skip_if_not_installed("sf")
  geometry <- plot_fixture()$geometry
  expect_equal(sf::st_crs(geometry)$epsg, 4326)
})

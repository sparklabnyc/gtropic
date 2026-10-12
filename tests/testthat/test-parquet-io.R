testthat::skip_if_not_installed("sf")
testthat::skip_if_not_installed("sfarrow")

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
  geometry <- geoparquet_test_geometry(NULL)
  local_mocked_bindings(
    st_read_parquet = function(...) geometry,
    .package = "sfarrow"
  )

  result <- read_geoparquet_raw(as.raw(1:4), "geometry.parquet")

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_identical(result$ADM2_ID, geometry$ADM2_ID)
  expect_equal(sf::st_coordinates(result), sf::st_coordinates(geometry))
})

test_that("GeoParquet reading normalizes a known CRS to WGS 84", {
  expected <- geoparquet_test_geometry(4326)
  projected <- sf::st_transform(expected, 3857)
  local_mocked_bindings(
    st_read_parquet = function(...) projected,
    .package = "sfarrow"
  )

  result <- read_geoparquet_raw(as.raw(1:4), "geometry.parquet")

  expect_equal(sf::st_crs(result)$epsg, 4326)
  expect_equal(
    sf::st_coordinates(result),
    sf::st_coordinates(expected),
    tolerance = 1e-7
  )
})

test_that("plotting fixture retains an explicit geographic CRS", {
  geometry <- plot_fixture()$geometry
  expect_equal(sf::st_crs(geometry)$epsg, 4326)
})

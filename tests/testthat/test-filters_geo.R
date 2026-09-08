adm2 <- tibble::tribble(
  ~ADM2_ID,   ~ADM2_NAME,  ~ADM2_GROUP, ~WHO_ENTITY,     ~WHO_REGION,
  "USA.1.1",  "Harris",    "USA",       "United States", "AMRO",
  "MEX.1.1",  "Cancun",    "MEX",       "Mexico",        "AMRO",
  "PHL.1.1",  "Tacloban",  "PHL",       "Philippines",   "WPRO",
  "CRI.1.1",  "San Jose",  "CRI",       "Costa Rica",    "AMRO"
)

test_that("values within one argument are combined with OR", {
  geo <- resolve_geography(
    adm2,
    filters = list(WHO_ENTITY = c("United States", "Mexico"))
  )
  expect_setequal(geo$adm2_ids, c("USA.1.1", "MEX.1.1"))
})

test_that("different arguments are combined with AND", {
  geo <- resolve_geography(
    adm2,
    filters = list(ADM2_NAME = "San Jose", ADM2_GROUP = "CRI")
  )
  expect_equal(geo$adm2_ids, "CRI.1.1")
})

test_that("a conflicting intersection errors rather than returning nothing", {
  expect_error(
    resolve_geography(
      adm2,
      filters = list(WHO_ENTITY = "Mexico", ADM2_GROUP = "USA")
    ),
    "No ADM2 units satisfy"
  )
})

test_that("the empty-intersection error names the arguments involved", {
  expect_error(
    resolve_geography(
      adm2,
      filters = list(WHO_ENTITY = "Mexico", ADM2_GROUP = "USA")
    ),
    "WHO_ENTITY"
  )
})

test_that("an unmatched geographic value errors with a suggestion", {
  expect_error(
    resolve_geography(adm2, filters = list(WHO_ENTITY = "Untied States")),
    "United States"
  )
})

test_that("ADM2_ID is matched exactly", {
  expect_error(
    resolve_geography(adm2, filters = list(ADM2_ID = "usa.1.1")),
    "did not match"
  )
  geo <- resolve_geography(adm2, filters = list(ADM2_ID = "USA.1.1"))
  expect_equal(geo$adm2_ids, "USA.1.1")
})

test_that("no filters returns every unit", {
  geo <- resolve_geography(adm2)
  expect_setequal(geo$adm2_ids, adm2$ADM2_ID)
})

# --- polygon ----------------------------------------------------------------

skip_if_no_sf <- function() {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("sfarrow")
}

make_pop <- function(lat_phl) {
  tibble::tibble(
    ADM2_ID = adm2$ADM2_ID,
    POP_CENTROID_LAT = c(29.86, 21.16, lat_phl, 9.93),
    POP_CENTROID_LON = c(-95.39, -86.85, 125.00, -84.08)
  )
}

test_that("a polygon with no CRS errors", {
  skip_if_no_sf()
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-97, 28), c(-93, 28), c(-93, 31), c(-97, 31), c(-97, 28)
  ))))
  expect_error(
    resolve_geography(adm2, list("2001" = make_pop(11.24)),
      polygon = poly
    ),
    "no coordinate reference system"
  )
})

test_that("reproject = FALSE with a non-4326 CRS errors", {
  skip_if_no_sf()
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-97, 28), c(-93, 28), c(-93, 31), c(-97, 31), c(-97, 28)
    ))),
    crs = 4326
  )
  poly <- sf::st_transform(poly, 3857)
  expect_error(
    resolve_geography(adm2, list("2001" = make_pop(11.24)),
      polygon = poly, reproject = FALSE
    ),
    "reproject"
  )
})

test_that("polygon membership across multiple years warns", {
  skip_if_no_sf()
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-97, 28), c(-93, 28), c(-93, 31), c(-97, 31), c(-97, 28)
    ))),
    crs = 4326
  )
  expect_warning(
    resolve_geography(
      adm2,
      list("2001" = make_pop(11.24), "2002" = make_pop(11.24)),
      polygon = poly
    ),
    "year-dependent"
  )
})

# --- guardrail --------------------------------------------------------------

test_that("an under-threshold estimate proceeds silently", {
  est <- list(bytes = 100 * 1024^2, n_files = 3L, n_cached = 0L)
  expect_true(apply_guardrail(est, quiet = TRUE))
})

test_that("confirm = FALSE skips the guardrail entirely", {
  est <- list(bytes = 500 * 1024^3, n_files = 900L, n_cached = 0L)
  expect_true(apply_guardrail(est, confirm = FALSE))
})

test_that("an over-threshold estimate errors non-interactively", {
  local_mocked_bindings(is_interactive = function() FALSE)
  est <- list(bytes = 5 * 1024^3, n_files = 47L, n_cached = 0L)
  expect_error(apply_guardrail(est), "not interactive")
})

test_that("the non-interactive error names the escape hatches", {
  local_mocked_bindings(is_interactive = function() FALSE)
  est <- list(bytes = 5 * 1024^3, n_files = 47L, n_cached = 0L)
  expect_error(apply_guardrail(est), "confirm_threshold_gb")
})

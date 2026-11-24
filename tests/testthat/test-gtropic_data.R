# Argument handling of gtropic_data() (Plan 03 UX-05, UX-06).

capture_args <- function(env = parent.frame()) {
  local_mocked_bindings(
    assemble_gtropic_data = function(args, ...) args,
    .env = env
  )
}

test_that("the default tables leave out geometry when sf is missing", {
  capture_args()
  local_mocked_bindings(geo_packages_missing = function() c("sf", "sfarrow"))
  withr::local_options(rlib_message_verbosity = "verbose")

  expect_message(
    args <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = 2005),
    "Leaving out the \"geometry\" table"
  )
  expect_false("geometry" %in% args$tables)
  expect_setequal(args$tables, setdiff(VALID_TABLES, "geometry"))

  expect_no_message(
    args <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = 2005, quiet = TRUE)
  )
  expect_false("geometry" %in% args$tables)
})

test_that("geometry named explicitly is kept, whatever is installed", {
  capture_args()
  local_mocked_bindings(geo_packages_missing = function() c("sf", "sfarrow"))

  args <- gtropic_data(WHO_ENTITY = "Jamaica", tables = c("wind", "geometry"))

  expect_true("geometry" %in% args$tables)
})

test_that("the default tables include geometry when sf and sfarrow are installed", {
  capture_args()
  local_mocked_bindings(geo_packages_missing = function() character(0))

  expect_no_message(args <- gtropic_data(WHO_ENTITY = "Jamaica"))

  expect_setequal(args$tables, VALID_TABLES)
})

test_that("logical arguments must be a single TRUE or FALSE", {
  capture_args()
  for (arg in c("quiet", "reproject", "offline", "make_valid", "confirm")) {
    for (value in list("yes", NA, 1, c(TRUE, FALSE), NULL)) {
      expect_error(
        do.call(gtropic_data, stats::setNames(list(value), arg)),
        arg,
        fixed = TRUE,
        label = paste(arg, "=", deparse(value))
      )
    }
  }
})

test_that("cache is TRUE, FALSE or NULL", {
  expect_error(cache_resolve("yes"), "cache", fixed = TRUE)
  expect_error(cache_resolve(NA), "cache", fixed = TRUE)
  expect_false(cache_resolve(FALSE)$enabled)
})

test_that("confirm defaults to TRUE and FALSE skips the guardrail", {
  capture_args()
  expect_true(gtropic_data(WHO_ENTITY = "Jamaica", tables = "wind")$confirm)

  withr::local_options(gtropic.confirm_threshold_gb = 0.001)
  local_mocked_bindings(is_interactive = function() FALSE)
  big <- list(bytes = 1024^3, n_files = 3L, n_cached = 0L)
  expect_error(apply_guardrail(big, confirm = TRUE), "confirmation threshold")
  expect_true(apply_guardrail(big, confirm = FALSE))
})

test_that("a single date_range value is recorded as both endpoints", {
  capture_args()
  args <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = 2005, tables = "wind")
  expect_identical(args$date_range, c(2005, 2005))
})

test_that("the storm helpers have no year argument", {
  expect_false("year" %in% names(formals(gtropic_storms)))
  expect_false("year" %in% names(formals(gtropic_excluded_storms)))
})

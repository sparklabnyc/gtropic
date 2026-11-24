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

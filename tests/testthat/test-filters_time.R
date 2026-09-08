test_that("partial date elements expand to period start and end", {
  spec <- parse_date_input(date_range = c("2001", "2003"))
  expect_equal(spec$start, as.Date("2001-01-01"))
  expect_equal(spec$end, as.Date("2003-12-31"))

  spec <- parse_date_input(date_range = c(2001, 2001))
  expect_equal(spec$start, as.Date("2001-01-01"))
  expect_equal(spec$end, as.Date("2001-12-31"))

  spec <- parse_date_input(date_range = c("2001-03", "2001-04"))
  expect_equal(spec$start, as.Date("2001-03-01"))
  expect_equal(spec$end, as.Date("2001-04-30"))

  # February in a leap year: the month-end must not be hardcoded.
  spec <- parse_date_input(date_range = c("2000-02", "2000-02"))
  expect_equal(spec$end, as.Date("2000-02-29"))
})

test_that("full dates and Date objects pass through unchanged", {
  spec <- parse_date_input(date_range = c("2001-03-04", "2001-03-09"))
  expect_equal(spec$start, as.Date("2001-03-04"))
  expect_equal(spec$end, as.Date("2001-03-09"))

  spec <- parse_date_input(
    date_range = c(as.Date("2005-08-01"), as.Date("2005-09-30"))
  )
  expect_equal(spec$start, as.Date("2005-08-01"))

  spec <- parse_date_input(
    date_range = c(
      as.POSIXct("2005-08-01 13:00:00", tz = "UTC"),
      as.POSIXct("2005-09-30 02:00:00", tz = "UTC")
    )
  )
  expect_equal(spec$end, as.Date("2005-09-30"))
})

test_that("joint order inference resolves an individually ambiguous pair", {
  # 19 cannot be a month, so dmy is infeasible for the SET, which forces mdy
  # for both elements. Nothing should be reported.
  expect_silent(
    spec <- parse_date_input(date_range = c("01/04/2001", "05/19/2001"))
  )
  expect_equal(spec$inferred_order, "mdy")
  expect_false(spec$ambiguous)
  expect_equal(spec$start, as.Date("2001-01-04"))
  expect_equal(spec$end, as.Date("2001-05-19"))
})

test_that("a genuinely ambiguous pair warns and picks by precedence", {
  expect_warning(
    spec <- parse_date_input(date_range = c("01/04/2001", "05/06/2001")),
    "ambiguous"
  )
  expect_true(spec$ambiguous)
  expect_equal(spec$inferred_order, "mdy")
})

test_that("date_format bypasses inference entirely", {
  expect_silent(
    spec <- parse_date_input(
      date_range = c("01/04/2001", "05/06/2001"),
      date_format = "dmy"
    )
  )
  expect_false(spec$ambiguous)
  expect_equal(spec$start, as.Date("2001-04-01"))
  expect_equal(spec$end, as.Date("2001-06-05"))
})

test_that("uninterpretable dates error", {
  expect_error(
    parse_date_input(date_range = c("not a date", "2001-01-01")),
    "Could not interpret"
  )
})

test_that("date_range and date_list are mutually exclusive", {
  expect_error(
    parse_date_input(date_range = c(2001, 2002), date_list = "2001-06-05"),
    "cannot both be supplied"
  )
})

test_that("a reversed range errors", {
  expect_error(
    parse_date_input(date_range = c("2003-01-01", "2001-01-01")),
    "reversed"
  )
})

test_that("date_range must have exactly two elements", {
  expect_error(parse_date_input(date_range = "2001"), "exactly 2 elements")
  expect_error(
    parse_date_input(date_range = c("2001", "2002", "2003")),
    "exactly 2 elements"
  )
})

test_that("date_list rejects partial dates", {
  expect_error(parse_date_input(date_list = "2001"), "not a complete date")
  expect_error(parse_date_input(date_list = "2001-06"), "not a complete date")
})

test_that("no filters yields a type of none", {
  spec <- parse_date_input()
  expect_equal(spec$type, "none")
  expect_null(spec$start)
})

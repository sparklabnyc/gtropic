test_that("gtropic_filters retains full list values and prints compact previews", {
  adm2_ids <- sprintf("ADM2-%02d", 1:12)
  result <- structure(
    list(),
    filters_resolved = list(
      adm2_ids = adm2_ids,
      wind_radius_km = 500
    ),
    class = c("gtropic_data", "list")
  )

  filters <- gtropic_filters(result)

  expect_s3_class(filters, "gtropic_filters")
  expect_true(is.list(filters))
  expect_identical(filters$adm2_ids, adm2_ids)

  displayed <- paste(testthat::capture_messages(print(filters)), collapse = "\n")
  expect_match(displayed, "adm2_ids")
  expect_match(displayed, "12 values")
  expect_match(displayed, "ADM2-01", fixed = TRUE)
  expect_match(displayed, "ADM2-10", fixed = TRUE)
  expect_false(grepl("ADM2-11", displayed, fixed = TRUE))
  expect_match(displayed, "2 values omitted", fixed = TRUE)
  expect_match(displayed, "wind_radius_km")

  expect_identical(suppressMessages(print(filters)), filters)
})

test_that("print.gtropic_filters applies its preview limit", {
  result <- structure(
    list(),
    filters_resolved = list(adm2_ids = sprintf("ADM2-%02d", 1:12)),
    class = c("gtropic_data", "list")
  )

  displayed <- paste(
    testthat::capture_messages(print(gtropic_filters(result), n = 3)),
    collapse = "\n"
  )
  expect_match(displayed, "ADM2-03", fixed = TRUE)
  expect_false(grepl("ADM2-04", displayed, fixed = TRUE))
  expect_match(displayed, "9 values omitted", fixed = TRUE)
  expect_error(
    suppressMessages(print(gtropic_filters(result), n = 0)),
    "positive whole number"
  )
})

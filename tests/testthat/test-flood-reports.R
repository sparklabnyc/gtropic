test_that("published flood columns are renamed and other columns kept", {
  published <- tibble::tibble(
    ADM2_ID = "A",
    FLOOD_ID = "F1",
    PROP_FLOODED_AREA = 0.5,
    PROP_FLOODED_POP = 0.25,
    FLOODED_POP_CENTROID = TRUE
  )
  out <- rename_flood_columns(published)
  expect_named(
    out,
    c(
      "ADM2_ID",
      "FLOOD_ID",
      "PROP_AREA_IN_FLOOD_REPORT",
      "PROP_POP_IN_FLOOD_REPORT",
      "POP_CENTROID_IN_FLOOD_REPORT"
    )
  )
  expect_identical(unname(as.list(out[3:5])), unname(as.list(published[3:5])))

  # Files without some (or any) of the published columns are left as they are.
  expect_named(
    rename_flood_columns(published[c("ADM2_ID", "PROP_FLOODED_POP")]),
    c("ADM2_ID", "PROP_POP_IN_FLOOD_REPORT")
  )
  expect_identical(
    rename_flood_columns(published[1:2]),
    published[1:2]
  )
  expect_null(rename_flood_columns(NULL))
})

test_that("gtropic_data() returns flood columns under the gtropic names", {
  local_golden_collection()
  res <- gtropic_data(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "flood", "storm_metadata"),
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  )
  expect_gt(nrow(res$flood), 0L)
  expect_true(all(unname(FLOOD_COLUMN_NAMES) %in% names(res$flood)))
  expect_false(any(names(FLOOD_COLUMN_NAMES) %in% names(res$flood)))
})

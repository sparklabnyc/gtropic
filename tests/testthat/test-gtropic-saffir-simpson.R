test_that("gtropic_saffir_simpson returns the complete wind-category lookup", {
  out <- gtropic_saffir_simpson()

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("category", "threshold", "unit", "color"))
  expect_identical(out$category, c("TD", "TS", "Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"))
  expect_identical(out$threshold, c(17, 32, 42, 49, 58, 70, Inf))
  expect_true(all(out$unit == "m/s"))
  expect_identical(out$color, c("#6EC1EA", "#55FFFF", "#FFFFD9", "#FBDA8C", "#F89F59", "#F6738A", "#A188FC"))
})

test_that("gtropic_saffir_simpson returns a fresh mutable object", {
  first <- gtropic_saffir_simpson()
  first$color[1] <- "#000000"

  expect_identical(gtropic_saffir_simpson()$color[1], "#6EC1EA")
})

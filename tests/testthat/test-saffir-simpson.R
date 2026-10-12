test_that("saffir_simpson returns the complete wind-category lookup", {
  out <- saffir_simpson()

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("category", "threshold", "unit", "color"))
  expect_identical(out$category, c("TD", "TS", "Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5"))
  expect_identical(out$threshold, c(17, 32, 42, 49, 58, 70, Inf))
  expect_true(all(out$unit == "m/s"))
  expect_identical(out$color, c("#6EC1EA", "#5FF", "#FFFFD9", "#FBDA8C", "#F89F59", "#F6738A", "#A188FC"))
})

test_that("saffir_simpson returns a fresh mutable object", {
  first <- saffir_simpson()
  first$color[1] <- "#000000"

  expect_identical(saffir_simpson()$color[1], "#6EC1EA")
})

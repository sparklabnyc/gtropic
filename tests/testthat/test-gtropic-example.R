# Size budgets for files shipped in the tarball (Plan 04 PERF-15). The tarball
# limit itself (5 MB, warning at 4 MB) is enforced by the CI size gate; the
# recorded HTTP fixtures have their own guard in test-recorded-dataverse.R.

test_that("the bundled example object stays under 50 KB", {
  path <- system.file("extdata", "gtropic-example.rds", package = "gtropic")
  expect_true(nzchar(path))
  expect_lt(file.size(path), 50 * 1024)
})

test_that("the example object is a complete gtropic_data result", {
  x <- gtropic_example()
  expect_s3_class(x, "gtropic_data")
  expect_true(all(c("wind", "links") %in% names(x)))
  expect_s3_class(gtropic_manifest(x), "gtropic_manifest")
})

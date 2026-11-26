# Print, summary and format methods (Plan 03 UX-08; Plan 01 INFRA-05 item 2).
# Snapshots use gtropic_example(), whose values are synthetic and fixed.

hide_version <- function(lines) {
  gsub(
    as.character(utils::packageVersion("gtropic")),
    "<version>",
    lines,
    fixed = TRUE
  )
}

test_that("print.gtropic_data() output is stable", {
  skip_if_not_installed("sf") # the example has a geometry table only with sf
  testthat::local_reproducible_output(width = 80)
  expect_snapshot(print(gtropic_example()))
})

test_that("summary.gtropic_data() output is stable", {
  skip_if_not_installed("sf")
  testthat::local_reproducible_output(width = 80)
  expect_snapshot(summary(gtropic_example()))
})

test_that("print.gtropic_manifest() output is stable", {
  testthat::local_reproducible_output(width = 80)
  expect_snapshot(
    print(gtropic_manifest(gtropic_example())),
    transform = hide_version
  )
})

test_that("print.gtropic_filters() output is stable", {
  testthat::local_reproducible_output(width = 80)
  expect_snapshot(print(gtropic_filters(gtropic_example()), n = 3))
})

test_that("summary() returns a classed object and printing returns it", {
  x <- gtropic_example()
  s <- summary(x)
  expect_s3_class(s, "summary.gtropic_data")
  expect_named(
    s,
    c("manifest", "filters", "diagnostics", "retrieved_at", "tables")
  )
  expect_identical(names(s$tables), names(x))
  expect_identical(s$manifest, gtropic_manifest(x))
  out <- withVisible(suppressMessages(print(s)))
  expect_false(out$visible)
  expect_identical(out$value, s)
})

test_that("format() returns the printed lines", {
  x <- gtropic_example()
  for (obj in list(x, summary(x), gtropic_manifest(x), gtropic_filters(x))) {
    lines <- format(obj)
    expect_type(lines, "character")
    expect_identical(lines, cli::cli_fmt(print(obj)))
  }
})

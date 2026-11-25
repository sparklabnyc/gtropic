# The wording of the main errors users see (Plan 01 INFRA-05 item 2). Each
# class in ?gtropic_conditions has one snapshot; the class itself is tested
# in test-conditions.R. Review changes with testthat::snapshot_review().

test_that("a transport failure names the request and the server", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    abort_dataverse(
      simpleError("Could not resolve host: example.invalid"),
      what = "file listing",
      server = "example.invalid"
    ),
    error = TRUE
  )
})

test_that("a checksum mismatch names the file and both checksums", {
  local_reproducible_output(width = 80)
  path <- local_bytes_file(charToRaw("abc"))
  expect_snapshot(
    verify_checksum(path, strrep("0", 32L), label = "2005.parquet"),
    error = TRUE
  )
})

test_that("an unmatched filter value suggests the closest names", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    match_values(
      "Example Coastt",
      c("Example Coast", "Example Bay"),
      arg = "ADM2_NAME"
    ),
    error = TRUE
  )
  expect_snapshot(
    abort_empty_intersection(c("WHO_ENTITY", "ADM2_NAME")),
    error = TRUE
  )
})

test_that("an unconfirmed large download states its size", {
  local_reproducible_output(width = 80)
  withr::local_options(gtropic.confirm_threshold_gb = 0.001)
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_snapshot(
    apply_guardrail(list(bytes = 1024^3, n_files = 2L, n_cached = 0L)),
    error = TRUE
  )
})

test_that("a rejected manifest says which field is wrong", {
  local_reproducible_output(width = 80)
  bad <- gtropic_manifest(gtropic_example())
  bad$files$label[1] <- "../outside.parquet"
  expect_snapshot(gtropic_data(manifest = bad), error = TRUE)
})

# These fail before any request is made. (A malformed `date_range` is only
# rejected after dataset discovery, so it is not snapshotted here; see O12.)
test_that("common argument errors in exported functions read well", {
  local_reproducible_output(width = 80)
  expect_snapshot(gtropic_manifest(list()), error = TRUE)
  expect_snapshot(gtropic_data(quiet = "yes"), error = TRUE)
  expect_snapshot(gtropic_data(tables = "winds"), error = TRUE)
})

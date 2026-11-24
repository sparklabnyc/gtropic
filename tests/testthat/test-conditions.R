# Classed conditions (Plan 03 UX-07).

expect_gtropic_error <- function(expr, class) {
  err <- expect_error(expr, class = class)
  expect_s3_class(err, c(class, "gtropic_error", "rlang_error"))
  invisible(err)
}

test_that("transport failures are gtropic_error_network", {
  err <- simpleError("Could not resolve host")
  expect_gtropic_error(
    abort_dataverse(err, what = "file listing", server = "example.invalid"),
    "gtropic_error_network"
  )
})

test_that("checksum mismatches are gtropic_error_checksum", {
  expect_gtropic_error(
    verify_checksum(charToRaw("abc"), strrep("0", 32L), label = "x.parquet"),
    "gtropic_error_checksum"
  )
})

test_that("unmatched filter values are gtropic_error_no_match", {
  expect_gtropic_error(
    match_values("Atlantis", c("Example Coast", "Example Bay"), arg = "ADM2_NAME"),
    "gtropic_error_no_match"
  )
  expect_gtropic_error(
    abort_empty_intersection(c("WHO_ENTITY", "ADM2_NAME")),
    "gtropic_error_no_match"
  )
})

test_that("an unconfirmed large download is gtropic_error_download_size", {
  withr::local_options(gtropic.confirm_threshold_gb = 0.001)
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_gtropic_error(
    apply_guardrail(list(bytes = 1024^3, n_files = 2L, n_cached = 0L)),
    "gtropic_error_download_size"
  )
})

test_that("manifest validation errors are gtropic_error_manifest", {
  m <- gtropic_manifest(gtropic_example())
  m$files$label[1] <- "../outside.parquet"
  expect_gtropic_error(manifest_validate(m), "gtropic_error_manifest")
  expect_gtropic_error(manifest_validate(list()), "gtropic_error_manifest")
  m <- gtropic_manifest(gtropic_example())
  m$files <- m$files[0, ]
  expect_gtropic_error(manifest_validate(m), "gtropic_error_manifest")
})

test_that("classed warnings inherit from gtropic_warning", {
  w <- expect_warning(
    warn_precip_all_missing("EX.1"),
    class = "gtropic_warning_precip_all_missing"
  )
  expect_s3_class(w, "gtropic_warning")
  w <- expect_warning(
    gtropic_warn("{.val {1}} thing.", class = "gtropic_warning_availability"),
    class = "gtropic_warning_availability"
  )
  expect_s3_class(w, c("gtropic_warning_availability", "gtropic_warning"))
})

test_that("multi-year polygon warnings are gtropic_warning_polygon", {
  skip_if_not_installed("sf")
  local_golden_collection()
  polygon <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 124, ymin = 10, xmax = 126, ymax = 12),
    crs = 4326
  ))
  expect_warning(
    gtropic_data(
      polygon = polygon, date_range = c("2002-01-01", "2003-12-31"),
      tables = c("wind", "zero_pairs", "pop", "adm2", "storm_metadata"),
      cache = FALSE, quiet = TRUE, confirm = FALSE
    ),
    class = "gtropic_warning_polygon"
  )
})

test_that("accessors reject objects that are not gtropic_data", {
  for (f in list(gtropic_manifest, gtropic_filters, gtropic_diagnostics, gtropic_availability)) {
    expect_error(f(list()), "must be a <gtropic_data> object")
  }
})

test_that("the ?gtropic_conditions example catches a rejected manifest", {
  bad <- gtropic_manifest(gtropic_example())
  bad$files$label[1] <- "../outside.parquet"
  out <- tryCatch(
    gtropic_data(manifest = bad),
    gtropic_error_manifest = function(e) "rejected the manifest"
  )
  expect_identical(out, "rejected the manifest")
})

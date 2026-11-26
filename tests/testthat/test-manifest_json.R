# JSON manifest exchange (Plan 05 SEC-12 item 3).

json_round_trip <- function(m) {
  path <- withr::local_tempfile(
    fileext = ".json",
    .local_envir = parent.frame()
  )
  gtropic_manifest_write(m, path)
  gtropic_manifest_read(path)
}

test_that("a manifest survives a JSON round trip", {
  m <- gtropic_manifest(gtropic_example())
  r <- json_round_trip(m)
  expect_s3_class(r, "gtropic_manifest")
  expect_named(r, names(m))
  expect_equal(r, m, ignore_attr = "names")
})

test_that("gtropic_manifest_write() accepts a result and returns the path", {
  path <- withr::local_tempfile(fileext = ".json")
  expect_identical(gtropic_manifest_write(gtropic_example(), path), path)
  expect_equal(
    gtropic_manifest_read(path),
    gtropic_manifest(gtropic_example()),
    ignore_attr = "names"
  )
})

test_that("the JSON is stable text, so it diffs cleanly", {
  m <- gtropic_manifest(gtropic_example())
  a <- withr::local_tempfile(fileext = ".json")
  b <- withr::local_tempfile(fileext = ".json")
  gtropic_manifest_write(m, a)
  gtropic_manifest_write(json_round_trip(m), b)
  expect_identical(readLines(a), readLines(b))
  expect_match(readLines(a)[2], '"format": "gtropic_manifest"', fixed = TRUE)
})

test_that("a replay from a JSON manifest returns the original tables", {
  local_golden_collection()
  original <- gtropic_data(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "precip", "flood", "zero_pairs", "storm_metadata"),
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  )
  m <- json_round_trip(gtropic_manifest(original))
  expect_equal(m, gtropic_manifest(original), ignore_attr = "names")

  replay <- gtropic_data(
    manifest = m,
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  )

  expect_golden(replay, "country-date-range")
})

test_that("infinite radii and missing checksums round-trip", {
  m <- gtropic_manifest(gtropic_example())
  m$resolved_query$wind_radius_km <- Inf
  m$files$checksum[2] <- NA_character_
  r <- json_round_trip(m)
  expect_identical(r$resolved_query$wind_radius_km, Inf)
  expect_true(is.na(r$files$checksum[2]))
})

write_json_text <- function(text) {
  path <- withr::local_tempfile(
    fileext = ".json",
    .local_envir = parent.frame()
  )
  writeLines(text, path)
  path
}

edited_manifest <- function(edit) {
  m <- gtropic_manifest(gtropic_example())
  path <- withr::local_tempfile(
    fileext = ".json",
    .local_envir = parent.frame()
  )
  gtropic_manifest_write(m, path)
  j <- jsonlite::read_json(path, simplifyVector = FALSE)
  j <- edit(j)
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, null = "null"), path)
  path
}

test_that("files that are not valid manifests are rejected", {
  bad <- list(
    not_json = write_json_text("{not json"),
    not_manifest = write_json_text('{"a": 1}'),
    path_escape = edited_manifest(function(j) {
      j$files[[1]]$label <- "../outside.parquet"
      j
    }),
    wrong_type = edited_manifest(function(j) {
      j$files[[1]]$file_id <- "one"
      j
    }),
    missing_column = edited_manifest(function(j) {
      j$files[[1]]$checksum <- NULL
      j
    }),
    bad_query = edited_manifest(function(j) {
      j$resolved_query$wind_radius_km <- list(1, 2)
      j
    }),
    polygon = edited_manifest(function(j) {
      j$resolved_query$polygon <- list(type = "Polygon")
      j
    }),
    schema = edited_manifest(function(j) {
      j$schema_version <- 99
      j
    })
  )
  for (nm in names(bad)) {
    expect_error(
      gtropic_manifest_read(bad[[nm]]),
      class = "gtropic_error_manifest",
      label = nm
    )
  }
})

test_that("reading a crafted manifest never evaluates its strings", {
  flag <- withr::local_tempfile()
  path <- edited_manifest(function(j) {
    j$datasets[[1]]$dataset_doi <- sprintf("{file.create('%s')}", flag)
    j$files[[1]]$dataset_doi <- sprintf("{file.create('%s')}", flag)
    j
  })
  expect_error(gtropic_manifest_read(path), class = "gtropic_error_manifest")
  expect_false(file.exists(flag))
})

test_that("unknown query fields are dropped", {
  path <- edited_manifest(function(j) {
    j$resolved_query$confirm <- FALSE
    j
  })
  r <- gtropic_manifest_read(path)
  expect_false("confirm" %in% names(r$resolved_query))
})

test_that("paths are checked", {
  expect_error(gtropic_manifest_read(c("a", "b")), "single file path")
  expect_error(gtropic_manifest_read(tempfile()), "does not exist")
  expect_error(
    gtropic_manifest_write(gtropic_example(), NA_character_),
    "single file path"
  )
})

test_that("manifest times are UTC whatever the input's time zone", {
  # Before R 4.3.0, as.POSIXct(<POSIXct>, tz = "UTC") kept the input's zone,
  # so a JSON round trip changed the attribute on R 4.2.
  local_time <- as.POSIXct("2026-01-05 12:00:00", tz = "America/New_York")
  attr(local_time, "tzone") <- ""
  row <- tibble::tibble(
    directory_label = "yearly",
    label = "2001.parquet",
    file_id = 1L,
    checksum = strrep("a", 32L),
    checksum_type = "MD5",
    size_bytes = 1
  )
  m <- manifest_add(
    gtropic_manifest(gtropic_example()),
    "doi:10.0000/EXAMPLE",
    "1.0",
    row,
    downloaded_at = local_time,
    source_role = "historical"
  )
  added <- m$files$downloaded_at
  expect_identical(attr(added, "tzone"), "UTC")
  expect_equal(as.numeric(added[nrow(m$files)]), as.numeric(local_time))
})

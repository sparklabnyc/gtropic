test_that("cache hits retain original download times in file provenance", {
  cache <- fetcher_cache(withr::local_tempdir())
  key <- fetcher_key()
  bytes <- charToRaw("timestamped source")
  downloaded <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")

  cache_put(cache, key, bytes, checksum_raw(bytes), downloaded_at = downloaded)
  cached <- cache_get_record(cache, key, checksum_raw(bytes))

  expect_equal(cached$downloaded_at, downloaded)

  discovery <- fetcher_discovery(fetcher_listing()$listing)
  manifest <- manifest_new(discovery)
  manifest <- manifest_add(
    manifest, key$doi, key$version,
    tibble::tibble(
      directory_label = key$directory_label, label = key$label,
       file_id = 1L, checksum = checksum_raw(bytes), checksum_type = "SHA-256",
       size_bytes = length(bytes)
    ),
    cached$downloaded_at, source_role = "historical"
  )
  manifest <- manifest_finalize(manifest)

  expect_identical(manifest$schema_version, 1L)
  expect_equal(manifest$files$downloaded_at, downloaded)
  expect_equal(manifest$retrieved_at, downloaded)
  expect_equal(manifest$datasets, tibble::tibble(
    source_role = "Historical",
    dataset_doi = key$doi, dataset_version = key$version
  ))
})

test_that("manifest roles retain routing for one or both source datasets", {
  manifest <- manifest_new(list(server = "example.org"))
  row <- function(year) {
    tibble::tibble(
      directory_label = "02_wind/storm_metadata",
      label = paste0(year, ".parquet"), file_id = as.integer(year),
      checksum = paste0("checksum-", year), checksum_type = "SHA-256",
      size_bytes = 1
    )
  }
  manifest <- manifest_add(
    manifest, "doi:10.0000/CURRENT", "2.0", row(2026),
    as.POSIXct("2026-01-01", tz = "UTC"), source_role = "current"
  )
  manifest <- manifest_add(
    manifest, "doi:10.0000/HISTORICAL", "3.0", row(2005),
    as.POSIXct("2026-01-01", tz = "UTC"), source_role = "historical"
  )
  manifest <- manifest_finalize(manifest)
  discovery <- discovery_from_manifest(manifest)

  expect_equal(manifest$datasets$source_role, c("Historical", "Current-year"))
  expect_equal(discovery$historical$doi, "doi:10.0000/HISTORICAL")
  expect_equal(discovery$current$doi, "doi:10.0000/CURRENT")
  expect_equal(discovery$year_index$dataset, c("current", "historical"))

  current_only <- manifest_finalize(manifest_add(
    manifest_new(list(server = "example.org")), "doi:10.0000/CURRENT", "2.0",
    row(2026), as.POSIXct("2026-01-01", tz = "UTC"), source_role = "current"
  ))
  current_discovery <- discovery_from_manifest(current_only)
  expect_null(current_discovery$historical)
  expect_equal(current_discovery$current$doi, "doi:10.0000/CURRENT")
})

test_that("manifest replay rejects only explicitly supplied data-shaping arguments", {
  call <- quote(gtropic_data(manifest = mf, cache = FALSE, quiet = TRUE))
  expect_silent(manifest_assert_replay_arguments(call))
  expect_error(
    manifest_assert_replay_arguments(quote(gtropic_data(manifest = mf, tables = "wind"))),
    "data-shaping"
  )
})

test_that("gtropic_data replays the stored canonical query", {
  query <- list(
    geo_filters = list(WHO_ENTITY = "Example"), storm_filters = list(),
    storm_name_years = NULL, wind_radius_km = 500, polygon = NULL,
    reproject = TRUE, date_range = c("2001-01-01", "2001-12-31"),
    date_list = NULL, date_format = NULL, tables = c("wind", "storm_metadata"),
    precip_days_before = 2, precip_days_after = 1, precip_radius_km = 500,
    flood_days_before = 2, flood_days_after = 1, flood_radius_km = 500,
    flood_match = "start", geometry_resolution = "simplified",
    resolved_adm2_ids = "ADM2.1", resolved_storm_ids = "S.1",
    storm_metadata_years = 2001L
  )
  manifest <- manifest_new(list(server = "example.org"))
  manifest <- manifest_add(
    manifest, "doi:10.0000/HIST", "1.0",
    tibble::tibble(
      directory_label = "metadata", label = "adm2.parquet", file_id = 1L,
      checksum = "abc", checksum_type = "MD5", size_bytes = 1
    ),
    downloaded_at = as.POSIXct("2026-01-01", tz = "UTC"),
    source_role = "historical"
  )
  manifest <- manifest_finalize(manifest, resolved_query = query)
  local_mocked_bindings(assemble_gtropic_data = function(args, ...) args)

  replay <- gtropic_data(manifest = manifest, cache = FALSE, as = "arrow")

  expect_identical(replay$resolved_adm2_ids, query$resolved_adm2_ids)
  expect_identical(replay$resolved_storm_ids, query$resolved_storm_ids)
  expect_identical(replay$storm_metadata_years, query$storm_metadata_years)
  expect_identical(replay$tables, query$tables)
  expect_identical(replay$as, "arrow")
  expect_error(gtropic_data(manifest = manifest, tables = "flood"), "data-shaping")
})

test_that("canonical manifest queries retain resolved IDs and metadata years", {
  args <- list(
    geo_filters = list(WHO_ENTITY = "Example"),
    storm_filters = list(STORM_NAME = "Example-2026"),
    storm_name_years = 2026L, wind_radius_km = 500, polygon = NULL,
    reproject = TRUE, date_range = c("2026-01-01", "2026-12-31"),
    date_list = NULL, date_format = NULL, tables = c("wind", "storm_metadata"),
    precip_days_before = 2, precip_days_after = 1, precip_radius_km = 500,
    flood_days_before = 2, flood_days_after = 1, flood_radius_km = 500,
    flood_match = "start", geometry_resolution = "simplified",
    resolved_adm2_ids = NULL, resolved_storm_ids = NULL,
    storm_metadata_years = NULL
  )

  query <- manifest_query_from_args(
    args, resolved_adm2_ids = c("ADM2.2", "ADM2.1"),
    resolved_storm_ids = "S.2026", storm_metadata_years = 2026L
  )

  expect_identical(query$geo_filters, list())
  expect_identical(query$storm_filters, list(STORM_ID = "S.2026"))
  expect_identical(query$resolved_adm2_ids, c("ADM2.1", "ADM2.2"))
  expect_identical(query$resolved_storm_ids, "S.2026")
  expect_identical(query$storm_metadata_years, 2026L)
  expect_true(all(c("polygon", "storm_name_years") %in% names(query)))
  expect_null(query$polygon)
  expect_null(query$storm_name_years)

  manifest <- manifest_new(list(server = "example.org"))
  manifest <- manifest_add(
    manifest, "doi:10.0000/CURRENT", "2.0",
    tibble::tibble(
      directory_label = "02_wind/storm_metadata", label = "2026.parquet",
       file_id = 1L, checksum = "fixture", checksum_type = "SHA-256",
       size_bytes = 1
    ), as.POSIXct("2026-01-01", tz = "UTC"), source_role = "current"
  )
  manifest <- manifest_finalize(manifest, resolved_query = query)
  expect_silent(manifest_validate(manifest))
  expect_equal(manifest$datasets$source_role, "Current-year")

  reached_assembly <- FALSE
  local_mocked_bindings(assemble_gtropic_data = function(args, ...) {
    reached_assembly <<- TRUE
    args
  })
  replay <- gtropic_data(manifest = manifest)

  expect_true(reached_assembly)
  expect_null(replay$polygon)
  expect_null(replay$storm_name_years)
})

test_that("unrestricted current-only wind retrieval avoids historical metadata", {
  seen <- character(0)
  current <- list(
    doi = "doi:10.0000/CURRENT", version = "2.0", listing = dv_empty_listing()
  )
  discovery <- list(
    server = "example.org", historical = NULL, current = current,
    year_index = tibble::tibble(year = 2026L, dataset = "current")
  )
  args <- list(
    quiet = TRUE, version = ":latest", cache = FALSE, manifest = NULL,
    offline = FALSE, date_range = c("2026-01-01", "2026-12-31"),
    date_list = NULL, date_format = NULL, storm_name_years = NULL,
    geo_filters = list(), polygon = NULL, reproject = TRUE,
    storm_filters = list(STORM_ID = "S.2026"),
    tables = c("wind", "storm_metadata"), wind_radius_km = 500,
    precip_days_before = 2, precip_days_after = 1, precip_radius_km = 500,
    flood_days_before = 2, flood_days_after = 1, flood_radius_km = 500,
    flood_match = "start", geometry_resolution = "simplified", as = "tibble",
    confirm = FALSE, resolved_adm2_ids = NULL, resolved_storm_ids = NULL,
    storm_metadata_years = NULL, resolved_query = NULL
  )
  local_mocked_bindings(
    cache_resolve = function(...) list(enabled = FALSE, dir = NULL),
    discover_datasets = function(...) discovery,
    apply_guardrail = function(...) invisible(TRUE),
    fetch_file = function(dataset, directory_label, label, ctx, ...) {
      seen <<- c(seen, paste(dataset, directory_label, label, sep = "/"))
      expect_identical(dataset, "current")
      ctx$manifest_out$value <- manifest_add(
        ctx$manifest_out$value, current$doi, current$version,
        tibble::tibble(
          directory_label = directory_label, label = label, file_id = 1L,
          checksum = "fixture", checksum_type = "SHA-256", size_bytes = 1
        ), as.POSIXct("2026-01-01", tz = "UTC"), source_role = dataset
      )
      if (identical(directory_label, "02_wind/storm_metadata")) {
        return(tibble::tibble(
          STORM_ID = "S.2026", STORM_NAME = "Example-2026",
          STORM_GENESIS_DATE_UTC = "2026-06-01"
        ))
      }
      if (identical(directory_label, "02_wind/exposures")) {
        return(tibble::tibble(
          STORM_ID = "S.2026", ADM2_ID = "ADM2.1", STORM_DIST_KM = 25,
          LOCAL_DATETIME_STORM_CLOSEST = "2026-06-02T00:00:00+00:00"
        ))
      }
      stop("Unexpected source path")
    }
  )

  result <- suppressWarnings(assemble_gtropic_data(args))
  manifest <- gtropic_manifest(result)

  expect_false(any(grepl("historical|metadata/adm2", seen)))
  expect_true(all(startsWith(seen, "current/")))
  expect_equal(manifest$datasets, tibble::tibble(
    source_role = "Current-year",
    dataset_doi = current$doi, dataset_version = current$version
  ))
  expect_identical(manifest$resolved_query$resolved_adm2_ids, "ADM2.1")
  expect_identical(manifest$resolved_query$resolved_storm_ids, "S.2026")
  expect_identical(manifest$resolved_query$storm_metadata_years, 2026L)
})

test_that(":latest discovery bypasses the session memo", {
  calls <- 0L
  result <- list(server = "example.org")
  local_mocked_bindings(
    discover_datasets_impl = function(...) {
      calls <<- calls + 1L
      result
    }
  )

  versions <- list(historical = ":latest", current = ":latest")
  discover_datasets(versions, server = "example.org", collection = "x", quiet = TRUE)
  discover_datasets(versions, server = "example.org", collection = "x", quiet = TRUE)

  expect_equal(calls, 2L)
})

test_that("offline discovery uses a persisted snapshot and reports staleness", {
  dir <- withr::local_tempdir()
  discovery <- list(
    server = "example.org",
    historical = list(version = "1.0"),
    current = NULL,
    year_index = tibble::tibble(year = integer(), dataset = character())
  )
  cache_discovery_write(dir, discovery, "x")

  expect_warning(
    restored <- discover_datasets(
      list(historical = ":latest", current = ":latest"),
      server = "example.org", collection = "x", offline = TRUE,
      cache_dir = dir, quiet = TRUE
    ),
    "offline G-TROPIC discovery snapshot"
  )
  expect_identical(restored, discovery)
})

test_that("availability metadata records known lags and flood endpoint", {
  spec <- list(
    type = "range", start = as.Date("2026-01-20"), end = as.Date("2026-01-25")
  )
  availability <- availability_metadata(
    spec, c("wind", "precip", "flood"), today = as.Date("2026-01-26")
  )
  warnings <- availability_warnings(availability)

  expect_equal(availability$groundsource_flood$available_through, as.Date("2026-01-22"))
  expect_true(all(c("ibtracs_provisional", "era5_land_lag", "groundsource_flood_coverage") %in% names(warnings)))
  expect_match(warnings$groundsource_flood_coverage, "exceeds")
})

test_that("availability diagnostics use resolved storms and hazard windows", {
  old_storm <- tibble::tibble(
    STORM_ID = "2013-HAIYAN",
    STORM_NAME = "Haiyan-2013",
    STORM_GENESIS_DATE_UTC = "2013-11-03"
  )
  old_links <- tibble::tibble(
    PRECIP_IN_FIELD = TRUE,
    PRECIP_WINDOW_START = as.Date("2013-11-05"),
    PRECIP_WINDOW_END = as.Date("2013-11-08"),
    FLOOD_IN_FIELD = TRUE,
    FLOOD_WINDOW_START = as.Date("2013-11-05"),
    FLOOD_WINDOW_END = as.Date("2013-11-08")
  )

  availability <- availability_metadata(
    list(type = "none", start = NULL, end = NULL),
    c("wind", "precip", "flood"),
    storm_metadata = old_storm,
    links = old_links,
    today = as.Date("2026-01-26")
  )

  expect_false(availability$requested$includes_current_period)
  expect_length(availability_warnings(availability), 0L)
})

test_that("an old named storm without a date filter emits no current-period warning", {
  historical <- list(
    doi = "doi:10.0000/HIST", version = "1.0", listing = dv_empty_listing()
  )
  discovery <- list(
    server = "example.org", historical = historical, current = NULL,
    year_index = tibble::tibble(year = 2013L, dataset = "historical")
  )
  args <- list(
    quiet = TRUE, version = ":latest", cache = FALSE, manifest = NULL,
    offline = FALSE, date_range = NULL, date_list = NULL, date_format = NULL,
    storm_name_years = 2013L, geo_filters = list(), polygon = NULL,
    reproject = TRUE, storm_filters = list(STORM_NAME = "Haiyan-2013"),
    tables = c("wind", "storm_metadata"), wind_radius_km = 500,
    precip_days_before = 2, precip_days_after = 1, precip_radius_km = 500,
    flood_days_before = 2, flood_days_after = 1, flood_radius_km = 500,
    flood_match = "start", geometry_resolution = "simplified", as = "tibble",
    confirm = FALSE, resolved_adm2_ids = NULL, resolved_storm_ids = NULL,
    storm_metadata_years = NULL, resolved_query = NULL
  )
  local_mocked_bindings(
    cache_resolve = function(...) list(enabled = FALSE, dir = NULL),
    discover_datasets = function(...) discovery,
    apply_guardrail = function(...) invisible(TRUE),
    fetch_file = function(dataset, directory_label, label, ctx, ...) {
      expect_identical(dataset, "historical")
      ctx$manifest_out$value <- manifest_add(
        ctx$manifest_out$value, historical$doi, historical$version,
        tibble::tibble(
          directory_label = directory_label, label = label, file_id = 1L,
          checksum = "fixture", checksum_type = "SHA-256", size_bytes = 1
        ), as.POSIXct("2026-01-01", tz = "UTC"), source_role = dataset
      )
      if (identical(directory_label, "02_wind/storm_metadata")) {
        return(tibble::tibble(
          STORM_ID = "2013-HAIYAN", STORM_NAME = "Haiyan-2013",
          STORM_GENESIS_DATE_UTC = "2013-11-03"
        ))
      }
      if (identical(directory_label, "02_wind/exposures")) {
        return(tibble::tibble(
          STORM_ID = "2013-HAIYAN", ADM2_ID = "ADM2.1", STORM_DIST_KM = 25,
          LOCAL_DATETIME_STORM_CLOSEST = "2013-11-08T00:00:00+00:00"
        ))
      }
      stop("Unexpected source path")
    }
  )

  expect_silent(result <- assemble_gtropic_data(args))
  expect_length(gtropic_warnings(result), 0L)
})

test_that("dataset roles appear in manifest and data summaries", {
  discovery <- list(server = "example.org")
  manifest <- manifest_new(discovery)
  row <- tibble::tibble(
    directory_label = "02_wind/storm_metadata", label = "2026.parquet",
    file_id = 1L, checksum = "fixture", checksum_type = "SHA-256", size_bytes = 1
  )
  manifest <- manifest_add(
    manifest, "doi:10.0000/CURRENT", "2.0", row,
    as.POSIXct("2026-01-01", tz = "UTC"), source_role = "current"
  )
  manifest <- manifest_finalize(manifest)
  result <- new_gtropic_data(
    tables = list(wind = tibble::tibble()), manifest = manifest,
    filters_resolved = list(), retrieved_at = manifest$retrieved_at
  )

  expect_match(
    paste(testthat::capture_messages(print(manifest)), collapse = "\n"),
    "Current-year dataset: doi:10.0000/CURRENT (version 2.0)", fixed = TRUE
  )
  expect_match(
    paste(testthat::capture_messages(summary(result)), collapse = "\n"),
    "Current-year dataset: doi:10.0000/CURRENT v2.0", fixed = TRUE
  )
})

test_that("availability diagnostics retain warnings for current selected coverage", {
  current_storm <- tibble::tibble(STORM_GENESIS_DATE_UTC = "2026-01-25")
  current_links <- tibble::tibble(
    PRECIP_IN_FIELD = TRUE,
    PRECIP_WINDOW_START = as.Date("2026-01-23"),
    PRECIP_WINDOW_END = as.Date("2026-01-26"),
    FLOOD_IN_FIELD = TRUE,
    FLOOD_WINDOW_START = as.Date("2026-01-23"),
    FLOOD_WINDOW_END = as.Date("2026-01-26")
  )

  availability <- availability_metadata(
    list(type = "none", start = NULL, end = NULL),
    c("wind", "precip", "flood"),
    storm_metadata = current_storm,
    links = current_links,
    today = as.Date("2026-01-26")
  )

  expect_true(all(c("ibtracs_provisional", "era5_land_lag", "groundsource_flood_coverage", "gtropic_update_schedule") %in% names(availability_warnings(availability))))
})

test_that("download estimates require the resolved version and checksum", {
  cache <- fetcher_cache(withr::local_tempdir())
  key <- fetcher_key(version = "1.0")
  old <- charToRaw("old version")
  current <- charToRaw("current version")
  plan <- tibble::tibble(
    dataset = "historical", directory_label = key$directory_label,
    label = key$label, size_bytes = 100
  )
  discovery <- fetcher_discovery(tibble::tibble(
    file_id = 1L, label = key$label, directory_label = key$directory_label,
     checksum = checksum_raw(current), size_bytes = 100, content_type = "application/octet-stream"
  ))
  discovery$historical$version <- "2.0"

  cache_put(cache, key, old, checksum_raw(old))
  estimate <- estimate_download(plan, cache, discovery)
  expect_equal(estimate$n_cached, 0L)
  expect_equal(estimate$n_files, 1L)

  cache_put(cache, fetcher_key(version = "2.0"), old, checksum_raw(old))
  expect_equal(estimate_download(plan, cache, discovery)$n_cached, 0L)

  cache_put(cache, fetcher_key(version = "2.0"), current, checksum_raw(current))
  expect_equal(estimate_download(plan, cache, discovery)$n_cached, 1L)
})

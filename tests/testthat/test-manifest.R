# What a replayed manifest may control (Plan 05 SEC-03).

example_manifest <- function() gtropic_manifest(gtropic_example())

test_that("a manifest cannot set how its replay is executed", {
  withr::local_options(gtropic.server = "example.invalid")
  m <- example_manifest()
  m$resolved_query[c("confirm", "quiet", "cache", "offline")] <-
    list(FALSE, TRUE, TRUE, TRUE)
  local_mocked_bindings(assemble_gtropic_data = function(args, ...) args)

  args <- gtropic_data(manifest = m)

  expect_true(args$confirm)
  expect_false(args$quiet)
  expect_null(args$cache)
  expect_false(args$offline)
  expect_false("as" %in% names(args))
  expect_identical(args$resolved_adm2_ids, m$resolved_query$resolved_adm2_ids)
})

test_that("a manifest with confirm = FALSE still meets the download guardrail", {
  local_golden_collection()
  original <- gtropic_data(
    STORM_ID = "2002BBB",
    tables = c("wind", "storm_metadata"),
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  )
  m <- gtropic_manifest(original)
  m$resolved_query$confirm <- FALSE
  local_mocked_bindings(
    estimate_download = function(...) {
      list(bytes = 50 * 1024^3, n_files = 2L, n_cached = 0L)
    },
    is_interactive = function() FALSE
  )

  expect_error(
    gtropic_data(manifest = m, cache = FALSE, quiet = TRUE),
    "confirmation threshold"
  )
})

test_that("a manifest made by the package passes validation unchanged", {
  m <- example_manifest()
  expect_identical(manifest_validate(m), m)

  m$server <- "https://example.invalid/"
  expect_identical(manifest_validate(m)$server, "example.invalid")
})

test_that("malformed manifest fields are rejected", {
  set_file <- function(field, value) {
    function(m) {
      m$files[[field]][1] <- value
      m
    }
  }
  set_query <- function(field, value) {
    function(m) {
      m$resolved_query[field] <- list(value)
      m
    }
  }
  edits <- list(
    schema_version = function(m) {
      m$schema_version <- 2L
      m
    },
    server = function(m) {
      m$server <- "evil.example/steal?x="
      m
    },
    file_id_zero = set_file("file_id", 0L),
    file_id_na = set_file("file_id", NA_integer_),
    file_id_fraction = function(m) {
      m$files$file_id <- as.numeric(m$files$file_id)
      m$files$file_id[1] <- 1.5
      m
    },
    file_id_text = function(m) {
      m$files$file_id <- as.character(m$files$file_id)
      m
    },
    label_traversal = set_file("label", "../2020.parquet"),
    label_absolute = set_file("label", "/etc/passwd"),
    label_backslash = set_file("label", "a\\b.parquet"),
    label_hidden = set_file("label", ".hidden"),
    label_na = set_file("label", NA_character_),
    dir_traversal = set_file("directory_label", "../metadata"),
    dir_inner_traversal = set_file("directory_label", "metadata/../.."),
    dir_absolute = set_file("directory_label", "/metadata"),
    dir_empty_part = set_file("directory_label", "a//b"),
    dir_trailing = set_file("directory_label", "metadata/"),
    dir_backslash = set_file("directory_label", "a\\b"),
    checksum_path = set_file("checksum", paste0("../", strrep("a", 29L))),
    checksum_not_hex = set_file("checksum", "xyz"),
    doi_files = set_file("dataset_doi", "doi:10.0000/{x}"),
    doi_datasets = function(m) {
      m$datasets$dataset_doi[1] <- "https://evil.example"
      m
    },
    version_files = set_file("dataset_version", "1.0; x"),
    version_datasets = function(m) {
      m$datasets$dataset_version[1] <- "{x}"
      m
    },
    wind_radius_text = set_query("wind_radius_km", "500"),
    wind_radius_negative = set_query("wind_radius_km", -1),
    tables_unknown = set_query("tables", "nope"),
    precip_days_fraction = set_query("precip_days_before", 1.5),
    flood_radius_zero = set_query("flood_radius_km", 0),
    flood_match = set_query("flood_match", "anything"),
    geometry_resolution = set_query("geometry_resolution", "huge"),
    reproject = set_query("reproject", "yes"),
    make_valid = set_query("make_valid", NA),
    adm2_ids = set_query("resolved_adm2_ids", 1:3),
    storm_ids = set_query("resolved_storm_ids", NA_character_),
    metadata_years = set_query("storm_metadata_years", "2020"),
    storm_name_years = set_query("storm_name_years", 2020.5),
    geo_filter_name = set_query("geo_filters", list(EVIL = "x")),
    storm_filter_type = set_query("storm_filters", list(STORM_ID = 1)),
    polygon = set_query("polygon", "POLYGON ((0 0, 1 0, 1 1, 0 0))"),
    date_range = set_query("date_range", list("2020-01-01")),
    date_format = set_query("date_format", c("ymd", "dmy"))
  )
  for (nm in names(edits)) {
    m <- edits[[nm]](example_manifest())
    expect_error(
      manifest_validate(m),
      class = "gtropic_error_manifest",
      label = nm
    )
  }
})

test_that("an empty directory label is a valid root path", {
  m <- example_manifest()
  m$files$directory_label[1] <- ""
  expect_no_error(manifest_validate(m))
})

test_that("replaying a manifest from another server warns, naming both hosts", {
  local_mocked_bindings(assemble_gtropic_data = function(args, ...) args)
  m <- example_manifest()

  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  w <- expect_warning(gtropic_data(manifest = m), "example.invalid")
  expect_match(conditionMessage(w), "dataverse.harvard.edu", fixed = TRUE)

  withr::local_options(gtropic.server = "example.invalid")
  expect_no_warning(gtropic_data(manifest = m))
})

test_that("a context's manifest entries are bound once, first record kept", {
  discovery <- fetcher_discovery(fetcher_listing()$listing)
  ctx <- fetch_context(
    discovery,
    list(enabled = FALSE, dir = NULL),
    quiet = TRUE
  )
  row <- function(label, id) {
    list(
      directory_label = "yearly",
      label = label,
      file_id = id,
      checksum = strrep("a", 32L),
      checksum_type = "MD5",
      size_bytes = 1
    )
  }
  times <- as.POSIXct(c("2026-01-01", "2026-01-02", "2026-01-03"), tz = "UTC")
  added <- list(
    list("2001.parquet", 1L, times[1]),
    list("2002.parquet", 2L, times[2]),
    list("2001.parquet", 1L, times[3])
  )

  sequential <- manifest_new(discovery)
  for (a in added) {
    ctx_manifest_add(
      ctx,
      manifest_entry(
        "doi:10.0000/HIST",
        "1.0",
        row(a[[1]], a[[2]]),
        a[[3]],
        "historical"
      )
    )
    sequential <- manifest_add(
      sequential,
      "doi:10.0000/HIST",
      "1.0",
      row(a[[1]], a[[2]]),
      a[[3]],
      source_role = "historical"
    )
  }
  expect_length(ctx$manifest_out$entries, 3L)

  files <- ctx_manifest(ctx)$files
  expect_identical(files, sequential$files)
  expect_identical(files$label, c("2001.parquet", "2002.parquet"))
  expect_identical(files$downloaded_at, times[1:2])
  expect_length(ctx$manifest_out$entries, 0L)
})

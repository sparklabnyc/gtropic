test_that("published date-only columns have one stable output type", {
  input <- tibble::tibble(
    STORM_GENESIS_DATE_UTC = "2020-08-01",
    LOCAL_DATE_STORM_CLOSEST = "2020-08-04",
    LOCAL_DATE = as.Date("2020-08-04"),
    FLOOD_START_DATE = "2020-08-03",
    FLOOD_END_DATE = "2020-08-06",
    PRECIP_WINDOW_START = as.Date("2020-08-02"),
    PRECIP_WINDOW_END = as.Date("2020-08-05"),
    FLOOD_WINDOW_START = as.Date("2020-08-02"),
    FLOOD_WINDOW_END = as.Date("2020-08-05"),
    LOCAL_DATETIME_STORM_CLOSEST = "2020-08-04T23:00:00-05:00"
  )

  output <- normalize_table_types(input, "fixture")

  date_columns <- setdiff(names(input), "LOCAL_DATETIME_STORM_CLOSEST")
  expect_true(all(vapply(output[date_columns], inherits, logical(1), "Date")))
  expect_identical(
    output$LOCAL_DATETIME_STORM_CLOSEST,
    input$LOCAL_DATETIME_STORM_CLOSEST
  )
})

test_that("yearly tables normalize date fields before binding", {
  context <- list(
    discovery = list(
      year_index = tibble::tibble(
        year = 2025:2026,
        dataset = c("historical", "current")
      )
    )
  )

  local_mocked_bindings(
    fetch_file = function(dataset, directory_label, label, ctx, ...) {
      tibble::tibble(
        STORM_ID = paste0(dataset, "-", label),
        STORM_GENESIS_DATE_UTC = sub("[.]parquet$", "-01-01", label)
      )
    }
  )

  storms <- fetch_yearly("storm_metadata", 2025:2026, context)

  expect_s3_class(storms$STORM_GENESIS_DATE_UTC, "Date")
  expect_equal(
    storms$STORM_GENESIS_DATE_UTC,
    as.Date(c("2025-01-01", "2026-01-01"))
  )
})

test_that("wind and links date keys can be joined directly", {
  wind <- derive_local_date(tibble::tibble(
    STORM_ID = "A",
    ADM2_ID = "TEST.1",
    STORM_DIST_KM = 10,
    LOCAL_DATETIME_STORM_CLOSEST = "2020-08-04T23:00:00-05:00"
  ))
  links <- build_links(
    wind,
    adm2_ids = "TEST.1",
    storm_ids = "A",
    wind_radius_km = 500,
    link_radius_km = 500
  )

  joined <- dplyr::inner_join(
    wind,
    links,
    by = c("ADM2_ID", "STORM_ID", "LOCAL_DATE_STORM_CLOSEST")
  )

  expect_equal(nrow(joined), 1L)
  expect_s3_class(joined$LOCAL_DATE_STORM_CLOSEST, "Date")
})

test_that("shared fixture fields retain compatible types across tables", {
  skip_if_not_installed("withr")
  fixture_dir <- local_parquet_fixtures()
  fixture <- function(...) {
    normalize_table_types(arrow::read_parquet(file.path(fixture_dir, ...)))
  }
  wind <- derive_local_date(fixture("02_wind", "exposures", "2002.parquet"))
  zero <- derive_local_date(fixture("02_wind", "zero_pairs", "2002.parquet"))
  links <- build_links(
    wind,
    zero,
    adm2_ids = unique(c(wind$ADM2_ID, zero$ADM2_ID)),
    storm_ids = unique(c(wind$STORM_ID, zero$STORM_ID)),
    wind_radius_km = Inf,
    link_radius_km = Inf
  )
  tables <- list(
    wind = wind,
    zero_pairs = zero,
    links = links,
    precip = fixture("04_precip", "2002.parquet"),
    flood = fixture("03_flood", "2002.parquet"),
    pop = fixture("01_pop", "2002.parquet"),
    storm_metadata = fixture("02_wind", "storm_metadata", "2002.parquet"),
    tracks = fixture("02_wind", "storm_tracks", "2002.parquet"),
    adm2 = fixture("metadata", "adm2.parquet")
  )

  all_names <- unlist(lapply(tables, names), use.names = FALSE)
  shared <- names(which(table(all_names) > 1L))
  signatures <- lapply(shared, function(column) {
    unique(vapply(
      tables[vapply(tables, function(x) column %in% names(x), logical(1))],
      function(x) {
        paste(typeof(x[[column]]), class(x[[column]]), collapse = "/")
      },
      character(1)
    ))
  })
  names(signatures) <- shared

  incompatible <- signatures[lengths(signatures) > 1L]
  expect_length(incompatible, 0L)
})

test_that("malformed published dates fail with column context", {
  input <- tibble::tibble(FLOOD_START_DATE = "08/03/2020")

  expect_error(
    normalize_table_types(input, "03_flood/2020.parquet"),
    "FLOOD_START_DATE.*03_flood/2020[.]parquet.*YYYY-MM-DD"
  )
})

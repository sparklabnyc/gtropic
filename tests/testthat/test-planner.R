storm_meta <- tibble::tribble(
  ~STORM_ID, ~STORM_NAME, ~USA_ATCF_ID, ~STORM_GENESIS_DATE_UTC, ~.gtropic_file_year,
  "2002BBB", "Bertha",    "AL032002",   "2002-08-28",            2002L,
  "2002CCC", "Cristobal", "AL052002",   "2002-09-10",            2002L,
  "2002DDD", "Dolly",     "WP212002",   "2002-12-28",            2002L
)

test_that("storms are selected by genesis date, not by when they struck", {
  # The documented case: Bertha forms 28 August and hits Texas on 3 September.
  # A September range must NOT return it.
  spec <- parse_date_input(date_range = c("2002-09-01", "2002-09-30"))
  ids <- resolve_storms(storm_meta, spec)

  expect_false("2002BBB" %in% ids)
  expect_true("2002CCC" %in% ids)
})

test_that("date_list matches genesis dates exactly", {
  spec <- parse_date_input(date_list = "2002-08-28")
  expect_equal(resolve_storms(storm_meta, spec), "2002BBB")
})

test_that("a date_list entry matching no storm errors", {
  spec <- parse_date_input(date_list = "2002-08-27")
  expect_error(resolve_storms(storm_meta, spec), "matched no storm")
})

test_that("storm filters and date filters are combined with AND", {
  spec <- parse_date_input(date_range = c("2002-09-01", "2002-09-30"))
  expect_error(
    resolve_storms(storm_meta, spec, list(STORM_NAME = "Bertha")),
    "No storms satisfy"
  )
})

test_that("storm names match case-insensitively, ids exactly", {
  ids <- resolve_storms(storm_meta, storm_filters = list(STORM_NAME = "bertha"))
  expect_equal(ids, "2002BBB")

  expect_error(
    resolve_storms(storm_meta, storm_filters = list(STORM_ID = "2002bbb")),
    "did not match"
  )
})

test_that("the genesis-year convention assertion passes on clean metadata", {
  res <- assert_genesis_year_convention(storm_meta, 2002L)
  expect_false(res$violated)
  expect_equal(res$years, 2002L)
})

test_that("a fabricated convention violation widens the scan and warns", {
  broken <- storm_meta
  broken$.gtropic_file_year[1] <- 2001L

  expect_warning(
    res <- assert_genesis_year_convention(broken, 2002L),
    "genesis-year convention"
  )
  expect_true(res$violated)
  expect_equal(res$years, 2001:2003)
})

# --- year scanning ----------------------------------------------------------

test_that("storm-linked tables scan the genesis span with no margin", {
  yrs <- years_for_tables(
    storm_years = 2002L,
    window_years = list(precip = 2002:2003, flood = 2002:2003),
    tables = c("wind", "precip", "flood", "storm_metadata")
  )
  expect_equal(yrs$wind, 2002L)
  expect_equal(yrs$storm_metadata, 2002L)
  expect_equal(yrs$zero_pairs, 2002L)
})

test_that("precip and flood follow the real window bounds across a boundary", {
  yrs <- years_for_tables(
    storm_years = 2002L,
    window_years = list(precip = 2002:2003, flood = 2002:2003),
    tables = c("precip", "flood")
  )
  expect_equal(yrs$precip, 2002:2003)
  expect_equal(yrs$flood, 2002:2003)
})

test_that("flood_match = 'overlap' adds one leading year", {
  yrs <- years_for_tables(
    storm_years = 2002L,
    window_years = list(precip = 2002L, flood = 2002L),
    tables = "flood",
    flood_match = "overlap"
  )
  expect_equal(yrs$flood, 2001:2002)
})

test_that("requesting precip forces zero_pairs to be fetched", {
  expect_true(needs_zero_pairs(c("precip")))
  expect_true(needs_zero_pairs(c("flood")))
  expect_false(needs_zero_pairs(c("wind", "pop")))
})

test_that("storm_metadata is added whenever a storm-linked table is asked for", {
  expect_true("storm_metadata" %in% normalize_tables("wind"))
  expect_false("storm_metadata" %in% normalize_tables(c("adm2", "codebook")))
})

test_that("unknown table names error", {
  expect_error(normalize_tables(c("wind", "rainfall")), "Unknown table")
})

# --- links ------------------------------------------------------------------

wind <- tibble::tibble(
  STORM_ID = "2002BBB",
  ADM2_ID = "USA.1.2",
  STORM_DIST_KM = 35,
  LOCAL_DATE_STORM_CLOSEST = "2002-09-03"
)

zero <- tibble::tibble(
  STORM_ID = c("2002BBB", "2002BBB"),
  ADM2_ID = c("USA.1.1", "MEX.1.1"),
  STORM_DIST_KM = c(410, 905),
  LOCAL_DATE_STORM_CLOSEST = c("2002-09-03", "2002-09-03")
)

test_that("links include a zero-pair inside the radius and exclude one
           beyond it", {
  lk <- build_links(wind, zero,
    adm2_ids = c("USA.1.1", "USA.1.2", "MEX.1.1"),
    storm_ids = "2002BBB"
  )

  expect_setequal(lk$ADM2_ID, c("USA.1.1", "USA.1.2", "MEX.1.1"))
  expect_true(lk$PRECIP_IN_FIELD[lk$ADM2_ID == "USA.1.1"])
  expect_false(lk$PRECIP_IN_FIELD[lk$ADM2_ID == "MEX.1.1"])
  expect_type(lk$WIND_EXPOSURE, "logical")
  expect_true(lk$WIND_EXPOSURE[lk$ADM2_ID == "USA.1.2"])
  expect_false(lk$WIND_EXPOSURE[lk$ADM2_ID == "USA.1.1"])
})

test_that("the geographic filter acts as an outer bound on links", {
  lk <- build_links(wind, zero, adm2_ids = "USA.1.2", storm_ids = "2002BBB")
  expect_equal(nrow(lk), 1L)
  expect_equal(lk$ADM2_ID, "USA.1.2")
})

test_that("empty links have a logical wind-exposure column", {
  expect_type(links_empty()$WIND_EXPOSURE, "logical")
})

test_that("window arithmetic is asymmetric and independent per hazard", {
  lk <- build_links(
    wind, NULL,
    adm2_ids = "USA.1.2", storm_ids = "2002BBB",
    precip_window = list(before = 3, after = 1, radius_km = 500),
    flood_window = list(before = 0, after = 7, radius_km = 500)
  )
  expect_equal(lk$PRECIP_WINDOW_START, as.Date("2002-08-31"))
  expect_equal(lk$PRECIP_WINDOW_END, as.Date("2002-09-04"))
  expect_equal(lk$FLOOD_WINDOW_START, as.Date("2002-09-03"))
  expect_equal(lk$FLOOD_WINDOW_END, as.Date("2002-09-10"))
})

test_that("window years are derived from real bounds and cross year ends", {
  december <- tibble::tibble(
    STORM_ID = "2002DDD", ADM2_ID = "PHL.1.1", STORM_DIST_KM = 9.8,
    LOCAL_DATE_STORM_CLOSEST = "2002-12-31"
  )
  lk <- build_links(december, NULL,
    adm2_ids = "PHL.1.1",
    storm_ids = "2002DDD"
  )
  expect_equal(window_years(lk)$precip, 2002:2003)
})

# --- anchor derivation ------------------------------------------------------

test_that("the local date is the first 10 characters, whatever the offset", {
  x <- tibble::tibble(
    LOCAL_DATETIME_STORM_CLOSEST = c(
      "2005-08-29T06:10:00-05:00", # negative offset
      "2002-12-31T08:00:00+08:00", # positive offset
      "2002-12-31T23:30:00-06:00" # UTC instant falls on the next day
    )
  )
  out <- derive_local_date(x)
  expect_equal(
    out$LOCAL_DATE_STORM_CLOSEST,
    c("2005-08-29", "2002-12-31", "2002-12-31")
  )
})

test_that("a malformed or unpadded anchor aborts", {
  x <- tibble::tibble(LOCAL_DATETIME_STORM_CLOSEST = "2005-8-29T06:10:00-05:00")
  expect_error(derive_local_date(x), "ISO 8601")
})

test_that("a missing anchor aborts", {
  x <- tibble::tibble(LOCAL_DATETIME_STORM_CLOSEST = NA_character_)
  expect_error(derive_local_date(x), "missing values")
})

# --- precip and flood selection ---------------------------------------------

test_that("precip rows are not duplicated when two windows overlap", {
  lk <- tibble::tibble(
    STORM_ID = c("A", "B"),
    ADM2_ID = c("USA.1.1", "USA.1.1"),
    LOCAL_DATE_STORM_CLOSEST = as.Date(c("2002-09-03", "2002-09-04")),
    STORM_DIST_KM = c(10, 20),
    WIND_EXPOSURE = TRUE,
    PRECIP_IN_FIELD = TRUE,
    PRECIP_WINDOW_START = as.Date(c("2002-09-01", "2002-09-02")),
    PRECIP_WINDOW_END = as.Date(c("2002-09-04", "2002-09-05")),
    FLOOD_IN_FIELD = TRUE,
    FLOOD_WINDOW_START = as.Date(c("2002-09-01", "2002-09-02")),
    FLOOD_WINDOW_END = as.Date(c("2002-09-04", "2002-09-05"))
  )
  precip <- tibble::tibble(
    ADM2_ID = "USA.1.1",
    LOCAL_DATE = as.Date(c("2002-09-03", "2002-09-04", "2002-09-09")),
    PRECIP_MM = c(10, 20, 30)
  )

  out <- select_precip(lk, precip)
  # Two days sit inside BOTH windows; each must appear once.
  expect_equal(nrow(out), 2L)
  expect_setequal(
    as.character(out$LOCAL_DATE),
    c("2002-09-03", "2002-09-04")
  )
})

test_that("flood 'start' and 'overlap' select different rows", {
  lk <- tibble::tibble(
    STORM_ID = "D", ADM2_ID = "PHL.1.1",
    LOCAL_DATE_STORM_CLOSEST = as.Date("2002-12-31"),
    STORM_DIST_KM = 9.8, WIND_EXPOSURE = TRUE,
    PRECIP_IN_FIELD = TRUE,
    PRECIP_WINDOW_START = as.Date("2002-12-29"),
    PRECIP_WINDOW_END = as.Date("2003-01-01"),
    FLOOD_IN_FIELD = TRUE,
    FLOOD_WINDOW_START = as.Date("2002-12-29"),
    FLOOD_WINDOW_END = as.Date("2003-01-01")
  )
  flood <- tibble::tibble(
    ADM2_ID = "PHL.1.1",
    FLOOD_ID = "F0003",
    FLOOD_START_DATE = as.Date("2002-12-20"),
    FLOOD_END_DATE = as.Date("2003-01-04")
  )

  # Started before the window opened, so "start" misses it...
  expect_equal(nrow(select_flood(lk, flood, "start")), 0L)
  # ...but it was in progress when the storm arrived.
  expect_equal(nrow(select_flood(lk, flood, "overlap")), 1L)
})

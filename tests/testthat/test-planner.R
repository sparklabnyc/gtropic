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

test_that("public storm-name filters require and extract final year suffixes", {
  expect_equal(
    storm_name_years(c("Katrina-2005", "Haiyan-2013", "Katrina-2005")),
    c(2005L, 2013L)
  )
  for (name in c("Katrina", "Katrina-05", "Katrina-2005-extra", "-2005")) {
    expect_error(storm_name_years(name), "Name-YYYY")
  }
})

test_that("gtropic_data validates storm-name suffixes before retrieval", {
  local_mocked_bindings(
    assemble_gtropic_data = function(args, ...) args
  )

  args <- gtropic_data(STORM_NAME = c("Name-2005", "Other-2006"))

  expect_equal(args$storm_name_years, c(2005L, 2006L))
  expect_error(gtropic_data(STORM_NAME = "Name"), "Name-YYYY")
})

test_that("gtropic_data repeats a single date-range endpoint", {
  local_mocked_bindings(
    assemble_gtropic_data = function(args, ...) args
  )

  expect_equal(gtropic_data(date_range = 2005)$date_range, c(2005, 2005))
  expect_equal(
    gtropic_data(date_range = as.Date("2005-08-01"))$date_range,
    as.Date(c("2005-08-01", "2005-08-01"))
  )
})

test_that("storm discovery fetches only years encoded in STORM_NAME", {
  fetched_years <- NULL
  local_mocked_bindings(
    helper_context = function(...) {
      list(discovery = list(year_index = tibble::tibble(year = 2004:2006)))
    },
    fetch_yearly = function(table, years, ctx, ...) {
      fetched_years <<- years
      if (length(years) == 0L) return(NULL)
      tibble::tibble(
        STORM_ID = "A", STORM_NAME = "Name-2005",
        STORM_GENESIS_DATE_UTC = "2005-08-01"
      )
    }
  )

  out <- gtropic_storms(STORM_NAME = "name-2005")

  expect_equal(fetched_years, 2005L)
  expect_equal(out$STORM_ID, "A")
  expect_warning(
    date_conflict <- gtropic_storms(
      date_range = c("2006-01-01", "2006-12-31"),
      STORM_NAME = "name-2005"
    ),
    "No storm metadata"
  )
  expect_identical(fetched_years, integer(0))
  expect_equal(nrow(date_conflict), 0L)

  expect_warning(
    year_conflict <- gtropic_storms(year = 2006, STORM_NAME = "name-2005"),
    "No storm metadata"
  )
  expect_identical(fetched_years, integer(0))
  expect_equal(nrow(year_conflict), 0L)
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

test_that("gtropic_data defaults to every available table", {
  default_tables <- eval(
    formals(gtropic_data)$tables,
    envir = environment(gtropic_data)
  )

  expect_identical(default_tables, VALID_TABLES)
})

test_that("wind_radius_km defaults to 500 km", {
  expect_identical(formals(gtropic_data)$wind_radius_km, 500)
})

test_that("wind-radius validation accepts finite, Inf, and NULL values", {
  expect_identical(check_wind_radius_km(500), 500)
  expect_identical(check_wind_radius_km(Inf), Inf)
  expect_identical(check_wind_radius_km(NULL), Inf)

  for (x in list(0, -1, NA_real_, NaN, -Inf, c(100, 200), "500")) {
    expect_error(check_wind_radius_km(x), "wind_radius_km")
  }
})

test_that("hazard-radius validation accepts finite, Inf, and NULL values", {
  expect_identical(check_positive_num(500, "radius"), 500)
  expect_identical(check_positive_num(Inf, "radius"), Inf)
  expect_identical(check_positive_num(NULL, "radius"), Inf)

  for (x in list(0, -1, NA_real_, NaN, -Inf, c(100, 200), "500")) {
    expect_error(check_positive_num(x, "radius"), "radius")
  }
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

test_that("a 500 km wind radius retains an in-range zero-pair and excludes one
            beyond it", {
  lk <- build_links(wind, zero,
    adm2_ids = c("USA.1.1", "USA.1.2", "MEX.1.1"),
    storm_ids = "2002BBB", wind_radius_km = 500, link_radius_km = 500
  )

  expect_setequal(lk$ADM2_ID, c("USA.1.1", "USA.1.2"))
  expect_true(lk$PRECIP_IN_FIELD[lk$ADM2_ID == "USA.1.1"])
  expect_type(lk$WIND_EXPOSURE, "logical")
  expect_type(lk$WIND_IN_FIELD, "logical")
  expect_true(lk$WIND_EXPOSURE[lk$ADM2_ID == "USA.1.2"])
  expect_false(lk$WIND_EXPOSURE[lk$ADM2_ID == "USA.1.1"])
})

test_that("the geographic filter acts as an outer bound on links", {
  lk <- build_links(
    wind, zero, adm2_ids = "USA.1.2", storm_ids = "2002BBB",
    wind_radius_km = 500, link_radius_km = 500
  )
  expect_equal(nrow(lk), 1L)
  expect_equal(lk$ADM2_ID, "USA.1.2")
})

test_that("a finite wind radius limits wind inclusively and excludes missing distances", {
  x <- tibble::tibble(
    STORM_DIST_KM = c(499, 500, 501, NA_real_),
    marker = c("inside", "boundary", "outside", "missing")
  )

  expect_equal(filter_wind_radius(x, 500)$marker, c("inside", "boundary"))
  expect_identical(filter_wind_radius(x, Inf), x)
})

test_that("a finite wind radius restricts links, while Inf and NULL preserve them", {
  pairs <- tibble::tibble(
    STORM_ID = rep("2002BBB", 4),
    ADM2_ID = paste0("USA.1.", 1:4),
    STORM_DIST_KM = c(499, 500, 501, NA_real_),
    LOCAL_DATE_STORM_CLOSEST = rep("2002-09-03", 4)
  )
  ids <- pairs$ADM2_ID

  finite <- build_links(
    pairs, NULL, adm2_ids = ids, storm_ids = "2002BBB",
    wind_radius_km = 500, link_radius_km = 500
  )
  unlimited <- build_links(
    pairs, NULL, adm2_ids = ids, storm_ids = "2002BBB",
    wind_radius_km = Inf, link_radius_km = Inf
  )
  null_radius <- build_links(
    pairs, NULL, adm2_ids = ids, storm_ids = "2002BBB",
    wind_radius_km = NULL, link_radius_km = Inf
  )

  expect_equal(finite$ADM2_ID, ids[1:2])
  expect_equal(unlimited$ADM2_ID, ids)
  expect_equal(null_radius$ADM2_ID, ids)
})

test_that("in-field flags distinguish finite and unbounded distance radii", {
  pairs <- tibble::tibble(
    STORM_ID = rep("2002BBB", 4),
    ADM2_ID = paste0("USA.1.", 1:4),
    STORM_DIST_KM = c(499, 500, 501, NA_real_),
    LOCAL_DATE_STORM_CLOSEST = rep("2002-09-03", 4)
  )

  finite <- build_links(
    pairs, NULL, adm2_ids = pairs$ADM2_ID, storm_ids = "2002BBB",
    wind_radius_km = 500, link_radius_km = Inf,
    precip_window = list(before = 0, after = 0, radius_km = Inf),
    flood_window = list(before = 0, after = 0, radius_km = Inf)
  )
  unbounded <- build_links(
    pairs, NULL, adm2_ids = pairs$ADM2_ID, storm_ids = "2002BBB",
    wind_radius_km = Inf, link_radius_km = Inf
  )

  expect_equal(finite$WIND_IN_FIELD, c(TRUE, TRUE, FALSE, FALSE))
  expect_true(all(finite$PRECIP_IN_FIELD))
  expect_true(all(finite$FLOOD_IN_FIELD))
  expect_true(all(unbounded$WIND_IN_FIELD))
})

test_that("link radius is the union of requested hazard fields", {
  expect_equal(
    link_radius_for_tables(
      c("wind", "precip"),
      wind_radius_km = 500, precip_radius_km = 1000, flood_radius_km = 500
    ),
    1000
  )
  expect_equal(
    link_radius_for_tables(
      "precip",
      wind_radius_km = 500, precip_radius_km = 1000, flood_radius_km = 500
    ),
    1000
  )
  expect_identical(
    link_radius_for_tables(
      "flood",
      wind_radius_km = 500, precip_radius_km = 500, flood_radius_km = Inf
    ),
    Inf
  )
})

test_that("larger precipitation fields retain links beyond the wind radius", {
  far_zero <- tibble::tibble(
    STORM_ID = "B", ADM2_ID = "far", STORM_DIST_KM = 900,
    LOCAL_DATE_STORM_CLOSEST = "2002-09-03"
  )
  near_wind <- tibble::tibble(
    STORM_ID = "A", ADM2_ID = "near", STORM_DIST_KM = 100,
    LOCAL_DATE_STORM_CLOSEST = "2002-09-03"
  )
  lk <- build_links(
    near_wind, far_zero, adm2_ids = c("near", "far"), storm_ids = c("A", "B"),
    wind_radius_km = 500, link_radius_km = 1000,
    precip_window = list(before = 0, after = 0, radius_km = 1000)
  )
  scope <- resolve_hazard_scope(
    lk, adm2_ids = c("near", "far"), tables = c("wind", "precip"),
    wind_radius_km = 500, precip_radius_km = 1000, flood_radius_km = 500
  )
  precip <- tibble::tibble(
    ADM2_ID = c("near", "far"), LOCAL_DATE = as.Date("2002-09-03")
  )

  expect_setequal(lk$ADM2_ID, c("near", "far"))
  expect_true(lk$WIND_IN_FIELD[lk$ADM2_ID == "near"])
  expect_false(lk$WIND_IN_FIELD[lk$ADM2_ID == "far"])
  expect_true(lk$PRECIP_IN_FIELD[lk$ADM2_ID == "far"])
  expect_equal(filter_wind_radius(near_wind, 500)$ADM2_ID, "near")
  expect_setequal(scope$adm2_ids, c("near", "far"))
  expect_setequal(scope$storm_ids, c("A", "B"))
  expect_setequal(select_precip(lk, precip)$ADM2_ID, c("near", "far"))
})

test_that("flood-only fields can retain zero pairs beyond the wind radius", {
  far_zero <- tibble::tibble(
    STORM_ID = "A", ADM2_ID = "far", STORM_DIST_KM = 900,
    LOCAL_DATE_STORM_CLOSEST = "2002-09-03"
  )
  lk <- build_links(
    wind = NULL, zero_pairs = far_zero, adm2_ids = "far", storm_ids = "A",
    wind_radius_km = 500, link_radius_km = 1000,
    flood_window = list(before = 0, after = 0, radius_km = 1000)
  )
  scope <- resolve_hazard_scope(
    lk, adm2_ids = c("near", "far"), tables = "flood",
    wind_radius_km = 500, precip_radius_km = 500, flood_radius_km = 1000
  )
  flood <- tibble::tibble(
    ADM2_ID = "far", FLOOD_START_DATE = as.Date("2002-09-03"),
    FLOOD_END_DATE = as.Date("2002-09-03")
  )

  expect_true(lk$FLOOD_IN_FIELD)
  expect_equal(scope$adm2_ids, "far")
  expect_equal(scope$storm_ids, "A")
  expect_equal(select_flood(lk, flood, "start")$ADM2_ID, "far")
})

test_that("empty links have logical wind columns", {
  expect_type(links_empty()$WIND_EXPOSURE, "logical")
  expect_type(links_empty()$WIND_IN_FIELD, "logical")
})

test_that("finite requested hazard fields scope ADM2s by the union of links", {
  lk <- tibble::tibble(
    STORM_ID = c("A", "B", "C"),
    ADM2_ID = c("one", "two", "three"),
    STORM_DIST_KM = c(100, 250, 800),
    WIND_EXPOSURE = c(TRUE, FALSE, TRUE),
    WIND_IN_FIELD = c(TRUE, FALSE, FALSE),
    PRECIP_IN_FIELD = c(FALSE, TRUE, FALSE),
    FLOOD_IN_FIELD = c(FALSE, FALSE, FALSE)
  )

  scope <- resolve_hazard_scope(
    lk, adm2_ids = lk$ADM2_ID,
    tables = c("wind", "precip", "flood"),
    wind_radius_km = 150, precip_radius_km = 300, flood_radius_km = 500
  )

  expect_setequal(scope$adm2_ids, c("one", "two"))
  expect_setequal(scope$storm_ids, c("A", "B"))
})

test_that("unbounded requested fields preserve selected ADM2 and storm scope", {
  lk <- tibble::tibble(
    STORM_ID = "A", ADM2_ID = "one", STORM_DIST_KM = NA_real_,
    PRECIP_IN_FIELD = FALSE, FLOOD_IN_FIELD = FALSE
  )

  scope <- resolve_hazard_scope(
    lk, adm2_ids = c("one", "two"), storm_ids = c("A", "B"),
    tables = c("wind", "precip"),
    wind_radius_km = Inf, precip_radius_km = Inf, flood_radius_km = Inf
  )

  expect_setequal(scope$adm2_ids, c("one", "two"))
  expect_setequal(scope$storm_ids, c("A", "B"))
})

test_that("finite hazard fields make absent links an empty ancillary scope", {
  scoped <- resolve_hazard_scope(
    links_empty(), adm2_ids = c("one", "two"), tables = "precip",
    wind_radius_km = Inf, precip_radius_km = 500, flood_radius_km = Inf
  )
  unbounded <- resolve_hazard_scope(
    links_empty(), adm2_ids = c("one", "two"), storm_ids = c("A", "B"),
    tables = "precip",
    wind_radius_km = Inf, precip_radius_km = Inf, flood_radius_km = Inf
  )

  expect_length(scoped$adm2_ids, 0L)
  expect_length(scoped$storm_ids, 0L)
  expect_setequal(unbounded$adm2_ids, c("one", "two"))
  expect_setequal(unbounded$storm_ids, c("A", "B"))
})

test_that("qualifying zero-pair links scope finite precipitation fields", {
  zero_only <- tibble::tibble(
    STORM_ID = "A", ADM2_ID = "zero", STORM_DIST_KM = 400,
    LOCAL_DATE_STORM_CLOSEST = "2002-09-03"
  )
  lk <- build_links(
    wind = NULL, zero_pairs = zero_only, adm2_ids = "zero", storm_ids = "A",
    wind_radius_km = Inf, link_radius_km = Inf,
    precip_window = list(before = 2, after = 1, radius_km = 500)
  )
  scope <- resolve_hazard_scope(
    lk, adm2_ids = c("zero", "outside"), tables = "precip",
    wind_radius_km = Inf, precip_radius_km = 500, flood_radius_km = Inf
  )

  expect_equal(lk$WIND_EXPOSURE, FALSE)
  expect_equal(scope$adm2_ids, "zero")
  expect_equal(scope$storm_ids, "A")
})

test_that("assembly reports finite hazard scope while preserving source links", {
  args <- function(wind_radius_km) {
    list(
      quiet = TRUE,
      version = ":latest",
      cache = NULL,
      manifest = NULL,
      date_range = NULL,
      date_list = NULL,
      date_format = NULL,
      storm_name_years = NULL,
      geo_filters = list(),
      polygon = NULL,
      reproject = TRUE,
      storm_filters = list(),
      tables = c("wind", "zero_pairs", "pop", "adm2", "tracks", "storm_metadata"),
      wind_radius_km = wind_radius_km,
      precip_days_before = 0,
      precip_days_after = 0,
      precip_radius_km = Inf,
      flood_days_before = 0,
      flood_days_after = 0,
      flood_radius_km = Inf,
      flood_match = "start",
      geometry_resolution = "simplified",
      as = "tibble",
      confirm = TRUE
    )
  }

  local_mocked_bindings(
    resolve_versions = function(...) list(),
    cache_resolve = function(...) list(),
    discover_datasets = function(...) {
      list(year_index = tibble::tibble(
        year = 2000:2001,
        dataset = c("historical", "historical")
      ))
    },
    fetch_context = function(...) {
      manifest_out <- new.env(parent = emptyenv())
      manifest_out$value <- list()
      list(manifest_out = manifest_out)
    },
    fetch_file = function(...) {
      tibble::tibble(ADM2_ID = c("near", "far"))
    },
    resolve_geography = function(...) {
      list(
        adm2_ids = c("near", "far"), warnings = list(),
        inconsistent_adm2_ids = character(0)
      )
    },
    fetch_yearly = function(table, years, ...) {
      switch(table,
        storm_metadata = tibble::tibble(
          STORM_ID = c("A", "B"), STORM_NAME = c("Alpha", "Bravo"),
          STORM_GENESIS_DATE_UTC = c("2000-08-01", "2001-08-01"),
          .gtropic_file_year = 2000:2001
        ),
        wind = tibble::tibble(
          STORM_ID = c("A", "B"), ADM2_ID = c("near", "far"),
          STORM_DIST_KM = c(100, 900),
          LOCAL_DATETIME_STORM_CLOSEST = c(
            "2000-08-02T00:00:00+00:00", "2001-08-02T00:00:00+00:00"
          )
        ),
        zero_pairs = tibble::tibble(
          STORM_ID = "B", ADM2_ID = "far", STORM_DIST_KM = 900,
          LOCAL_DATETIME_STORM_CLOSEST = "2001-08-02T00:00:00+00:00"
        ),
        pop = tibble::tibble(
          ADM2_ID = rep(c("near", "far"), each = length(years)),
          YEAR = rep(years, 2L)
        ),
        tracks = tibble::tibble(STORM_ID = c("A", "B"))
      )
    },
    plan_files = function(...) tibble::tibble(),
    attach_plan_sizes = function(plan, ...) plan,
    estimate_download = function(...) 0,
    apply_guardrail = function(...) invisible(NULL),
    manifest_finalize = function(...) {
      list(
        server = "test",
        datasets = tibble::tibble(
          source_role = "Historical", dataset_doi = "doi:test",
          dataset_version = "1.0"
        ),
        files = tibble::tibble(size_bytes = numeric(0))
      )
    }
  )

  finite <- assemble_gtropic_data(args(200))
  finite_filters <- gtropic_filters(finite)

  expect_equal(finite$storm_metadata$STORM_ID, "A")
  expect_equal(finite_filters$storm_ids, "A")
  expect_equal(finite_filters$adm2_ids, "near")
  expect_equal(finite$tracks$STORM_ID, "A")
  expect_equal(finite$pop$YEAR, 2000L)
  expect_equal(finite$zero_pairs$STORM_ID, "B")
  expect_true(any(grepl(
    "Storms: 1", testthat::capture_messages(print(finite)), fixed = TRUE
  )))
  expect_true(any(grepl(
    "ADM2 units: 1", testthat::capture_messages(summary(finite)), fixed = TRUE
  )))

  unbounded <- assemble_gtropic_data(args(Inf))
  unbounded_filters <- gtropic_filters(unbounded)

  expect_setequal(unbounded$storm_metadata$STORM_ID, c("A", "B"))
  expect_setequal(unbounded_filters$storm_ids, c("A", "B"))
  expect_setequal(unbounded_filters$adm2_ids, c("near", "far"))
  expect_setequal(unbounded$pop$YEAR, 2000:2001)
})

test_that("window arithmetic is asymmetric and independent per hazard", {
  lk <- build_links(
    wind, NULL,
    adm2_ids = "USA.1.2", storm_ids = "2002BBB",
    wind_radius_km = 500, link_radius_km = 500,
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
    storm_ids = "2002DDD", wind_radius_km = 500, link_radius_km = 500
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

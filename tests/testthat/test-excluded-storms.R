excluded_storm_fixture <- tibble::tribble(
  ~STORM_ID   , ~STORM_NAME           , ~STORM_GENESIS_DATE_UTC ,
  "EXCL.2002" , "Excluded-2002"       , "2002-08-01"            ,
  "EXCL.2003" , "Other excluded-2003" , "2003-09-01"
)

test_that("gtropic_excluded_storms reads and filters exclusion records", {
  expect_identical(
    TABLE_SPEC$excluded_storms$dir,
    "02_wind/excluded_storms"
  )

  fetched <- NULL
  local_mocked_bindings(
    helper_context = function(...) {
      list(discovery = list(year_index = tibble::tibble(year = 2002:2003)))
    },
    fetch_yearly = function(table, years, ...) {
      fetched <<- list(table = table, years = years)
      genesis_year <- as.integer(substr(
        excluded_storm_fixture$STORM_GENESIS_DATE_UTC,
        1L,
        4L
      ))
      excluded_storm_fixture[genesis_year %in% years, , drop = FALSE]
    }
  )

  out <- gtropic_excluded_storms(
    STORM_NAME = "excluded-2002",
    cache = FALSE
  )

  expect_identical(fetched$table, "excluded_storms")
  # The season candidate 2001 is not published and is not requested.
  expect_identical(fetched$years, 2002L)
  expect_named(out, c("STORM_ID", "STORM_NAME", "STORM_GENESIS_DATE_UTC"))
  expect_equal(out$STORM_ID, "EXCL.2002")
})

test_that("storm identity errors distinguish excluded and absent records", {
  available <- tibble::tribble(
    ~STORM_ID        , ~STORM_NAME      , ~USA_ATCF_ID , ~STORM_GENESIS_DATE_UTC ,
    "AVAILABLE.2002" , "Available-2002" , "AL012002"   , "2002-08-01"
  )
  excluded_lookup <- function() excluded_storm_fixture

  expect_error(
    resolve_storms(
      available,
      storm_filters = list(STORM_ID = "EXCL.2002"),
      excluded_lookup = excluded_lookup
    ),
    "listed as excluded"
  )
  expect_error(
    resolve_storms(
      available,
      storm_filters = list(STORM_NAME = "excluded-2002"),
      excluded_lookup = excluded_lookup
    ),
    "listed as excluded"
  )
  expect_error(
    resolve_storms(
      available,
      storm_filters = list(STORM_ID = "NOT.2002"),
      excluded_lookup = excluded_lookup
    ),
    "absent from both"
  )
})

test_that("gtropic_storms checks exclusion records only after a name misses", {
  fetched <- character(0)
  local_mocked_bindings(
    helper_context = function(...) {
      list(discovery = list(year_index = tibble::tibble(year = 2002L)))
    },
    fetch_yearly = function(table, ...) {
      fetched <<- c(fetched, table)
      switch(
        table,
        storm_metadata = tibble::tibble(
          STORM_ID = "AVAILABLE.2002",
          STORM_NAME = "Available-2002",
          STORM_GENESIS_DATE_UTC = "2002-08-01"
        ),
        excluded_storms = excluded_storm_fixture,
        stop("Unexpected table")
      )
    }
  )

  expect_error(
    gtropic_storms(STORM_NAME = "excluded-2002", cache = FALSE),
    "listed as excluded"
  )
  expect_identical(fetched, c("storm_metadata", "excluded_storms"))
})

test_that("excluded duplicate names warn and returned IDs distinguish records", {
  duplicate_excluded <- tibble::tribble(
    ~STORM_ID , ~STORM_NAME        , ~STORM_GENESIS_DATE_UTC ,
    "EXCL.A"  , "Duplicate-2003"   , "2002-12-30"            ,
    "EXCL.A"  , " duplicate-2003 " , "2002-12-30"            ,
    "EXCL.B"  , "DUPLICATE-2003"   , "2003-01-02"
  )
  local_mocked_bindings(
    helper_context = function(...) {
      list(discovery = list(year_index = tibble::tibble(year = 2002:2003)))
    },
    fetch_yearly = function(...) duplicate_excluded
  )

  expect_warning(
    out <- gtropic_excluded_storms(STORM_NAME = "duplicate-2003"),
    "returned unique.*STORM_ID.*distinguish",
    class = "gtropic_warning_ambiguous_excluded_storm_name"
  )
  expect_setequal(out$STORM_ID, c("EXCL.A", "EXCL.B"))

  expect_silent(
    narrowed <- gtropic_excluded_storms(
      date_range = c("2002-12-30", "2002-12-30"),
      STORM_NAME = "duplicate-2003"
    )
  )
  expect_equal(unique(narrowed$STORM_ID), "EXCL.A")
})

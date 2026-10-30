excluded_storm_fixture <- tibble::tribble(
  ~STORM_ID, ~STORM_NAME, ~STORM_GENESIS_DATE_UTC,
  "EXCL.2002", "Excluded-2002", "2002-08-01",
  "EXCL.2003", "Other excluded-2003", "2003-09-01"
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
      excluded_storm_fixture[excluded_storm_fixture$STORM_GENESIS_DATE_UTC %in%
        ifelse(years == 2002L, "2002-08-01", "2003-09-01"), , drop = FALSE]
    }
  )

  out <- gtropic_excluded_storms(
    STORM_NAME = "excluded-2002", cache = FALSE
  )

  expect_identical(fetched$table, "excluded_storms")
  expect_identical(fetched$years, 2002L)
  expect_named(out, c("STORM_ID", "STORM_NAME", "STORM_GENESIS_DATE_UTC"))
  expect_equal(out$STORM_ID, "EXCL.2002")
})

test_that("storm identity errors distinguish excluded and absent records", {
  available <- tibble::tribble(
    ~STORM_ID, ~STORM_NAME, ~USA_ATCF_ID, ~STORM_GENESIS_DATE_UTC,
    "AVAILABLE.2002", "Available-2002", "AL012002", "2002-08-01"
  )
  excluded_lookup <- function() excluded_storm_fixture

  expect_error(
    resolve_storms(
      available, storm_filters = list(STORM_ID = "EXCL.2002"),
      excluded_lookup = excluded_lookup
    ),
    "listed as excluded"
  )
  expect_error(
    resolve_storms(
      available, storm_filters = list(STORM_NAME = "excluded-2002"),
      excluded_lookup = excluded_lookup
    ),
    "listed as excluded"
  )
  expect_error(
    resolve_storms(
      available, storm_filters = list(STORM_ID = "NOT.2002"),
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
      switch(table,
        storm_metadata = tibble::tibble(
          STORM_ID = "AVAILABLE.2002", STORM_NAME = "Available-2002",
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

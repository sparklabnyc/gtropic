# Generate the tiny parquet fixtures the planner tests run against.
#
# These must mirror the real published schemas exactly -- same column names,
# same types -- or the tests will pass against a fiction. What they need NOT
# mirror is size: a handful of ADM2 units and five storms exercises every branch
# in the planner.
#
# Deliberately included edge cases:
#   * two countries in two different WHO regions
#   * a storm with a December genesis, so precip windows cross a year boundary
#   * zero-pairs at distances straddling the 500 km default radius
#   * a storm forming in late August that strikes in September (the documented
#     "excluded by a September range" case)
#   * anchors with negative and positive UTC offsets, and one where the offset
#     pushes the UTC instant into an adjacent day
#
# Run with: source("data-raw/make_fixtures.R")
# Commit the output under tests/testthat/fixtures/.

library(arrow)
library(dplyr)

fixture_dir <- file.path("tests", "testthat", "fixtures")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

write_fix <- function(x, ...) {
  path <- file.path(fixture_dir, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, path)
  invisible(path)
}

# --- adm2 -------------------------------------------------------------------

adm2 <- tibble::tribble(
  ~ADM2_ID,   ~ADM2_NAME,    ~ADM2_GROUP, ~WHO_ENTITY,      ~WHO_REGION,
  "USA.1.1",  "Harris",      "USA",       "United States",  "AMRO",
  "USA.1.2",  "Galveston",   "USA",       "United States",  "AMRO",
  "USA.2.1",  "Orleans",     "USA",       "United States",  "AMRO",
  "MEX.1.1",  "Cancun",      "MEX",       "Mexico",         "AMRO",
  "PHL.1.1",  "Tacloban",    "PHL",       "Philippines",    "WPRO",
  "PHL.1.2",  "Ormoc",       "PHL",       "Philippines",    "WPRO"
)
write_fix(adm2, "metadata", "adm2.parquet")

# --- pop --------------------------------------------------------------------
# Centroids move slightly between years; PHL.1.2 moves far enough to change
# polygon membership, which is what the year-varying warning test needs.

pop_year <- function(year, shift = 0) {
  tibble::tibble(
    ADM2_ID = adm2$ADM2_ID,
    YEAR = year,
    POP_TOTAL = c(4.7e6, 3.5e5, 3.9e5, 8.9e5, 2.4e5, 2.2e5),
    POP_CENTROID_LAT = c(29.86, 29.30, 29.95, 21.16, 11.24, 11.01) +
      c(0, 0, 0, 0, 0, shift),
    POP_CENTROID_LON = c(-95.39, -94.80, -90.07, -86.85, 125.00, 124.61),
    TIMEZONE = c(
      rep("America/Chicago", 3), "America/Cancun",
      rep("Asia/Manila", 2)
    )
  )
}
for (y in 2001:2003) {
  write_fix(
    pop_year(y, shift = if (y == 2003) 0.6 else 0),
    "01_pop", paste0(y, ".parquet")
  )
}

# --- storm metadata ---------------------------------------------------------
# Storms live in the file for their genesis year, by construction.

storms <- tibble::tribble(
  ~STORM_ID,    ~STORM_NAME, ~USA_ATCF_ID, ~STORM_GENESIS_DATE_UTC, ~SEASON,
  "2001AAA",    "Allison",   "AL012001",   "2001-06-05",            2001L,
  "2002BBB",    "Bertha",    "AL032002",   "2002-08-28",            2002L,
  "2002CCC",    "Cristobal", "AL052002",   "2002-09-10",            2002L,
  "2002DDD",    "Dolly",     "WP212002",   "2002-12-28",            2002L,
  "2003EEE",    "Erika",     "AL052003",   "2003-08-14",            2003L
)
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(storms, SEASON == y),
    "02_wind", "storm_metadata", paste0(y, ".parquet")
  )
}

# --- wind exposures ---------------------------------------------------------
# Anchor offsets are varied on purpose: a negative offset, a positive offset,
# and one where the UTC instant falls on the following day.

wind <- tibble::tribble(
  ~STORM_ID, ~ADM2_ID, ~STORM_DIST_KM, ~LOCAL_DATETIME_STORM_CLOSEST, ~MAX_WIND_MS, ~SEASON,
  "2001AAA", "USA.1.1", 12.4, "2001-06-09T14:30:00-05:00", 24.1, 2001L,
  "2002BBB", "USA.1.2", 35.0, "2002-09-03T22:45:00-05:00", 31.6, 2002L,
  "2002CCC", "USA.2.1", 48.2, "2002-09-13T06:10:00-05:00", 28.3, 2002L,
  "2002DDD", "PHL.1.1", 9.8, "2002-12-31T08:00:00+08:00", 44.0, 2002L,
  "2003EEE", "MEX.1.1", 21.7, "2003-08-18T19:20:00-05:00", 33.9, 2003L
)
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(wind, SEASON == y),
    "02_wind", "exposures", paste0(y, ".parquet")
  )
}

# --- zero pairs -------------------------------------------------------------
# Distances straddle the 500 km default so the radius field has something to
# include and something to exclude.

zero <- tibble::tribble(
  ~STORM_ID, ~ADM2_ID,  ~STORM_DIST_KM, ~LOCAL_DATETIME_STORM_CLOSEST,      ~SEASON,
  "2001AAA", "USA.1.2", 180.0,          "2001-06-09T15:00:00-05:00",        2001L,
  "2001AAA", "USA.2.1", 640.0,          "2001-06-09T17:00:00-05:00",        2001L,
  "2002BBB", "USA.1.1", 410.0,          "2002-09-03T21:00:00-05:00",        2002L,
  "2002BBB", "MEX.1.1", 905.0,          "2002-09-03T18:00:00-05:00",        2002L,
  "2002DDD", "PHL.1.2", 260.0,          "2002-12-31T09:30:00+08:00",        2002L,
  "2003EEE", "USA.1.1", 1200.0,         "2003-08-18T18:00:00-05:00",        2003L
)
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(zero, SEASON == y),
    "02_wind", "zero_pairs", paste0(y, ".parquet")
  )
}

# --- precipitation ----------------------------------------------------------
# Spans the 2002/2003 year boundary so the December storm's window crosses it.

precip_days <- function(ids, from, to) {
  tidyr::crossing(
    ADM2_ID = ids,
    LOCAL_DATE = seq(as.Date(from), as.Date(to), by = "day")
  ) |>
    dplyr::mutate(
      PRECIP_MM = round(stats::runif(dplyr::n(), 0, 180), 1),
      YEAR = as.integer(format(.data$LOCAL_DATE, "%Y"))
    )
}

set.seed(1)
precip <- precip_days(adm2$ADM2_ID, "2001-06-01", "2003-09-30")
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(precip, YEAR == y) |> dplyr::select(-YEAR),
    "04_precip", paste0(y, ".parquet")
  )
}

# --- flood ------------------------------------------------------------------
# One flood starts in December 2002 and ends in January 2003, which is the case
# flood_match = "overlap" exists for.

flood <- tibble::tribble(
  ~ADM2_ID, ~FLOOD_ID, ~FLOOD_START_DATE, ~FLOOD_END_DATE, ~FLOOD_AREA_KM2,
  "USA.1.1", "F0001", "2001-06-08", "2001-06-12", 140.2,
  "USA.1.2", "F0002", "2002-09-04", "2002-09-09", 88.5,
  "PHL.1.1", "F0003", "2002-12-27", "2003-01-04", 310.7,
  "MEX.1.1", "F0004", "2003-08-19", "2003-08-22", 45.0
) |>
  dplyr::mutate(YEAR = as.integer(substr(FLOOD_START_DATE, 1, 4)))
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(flood, YEAR == y) |> dplyr::select(-YEAR),
    "03_flood", paste0(y, ".parquet")
  )
}

# --- storm tracks -----------------------------------------------------------
# Raw ~3-hourly best-track observations. Present so the tracks table can be
# exercised; never used to recompute STORM_DIST_KM.

tracks <- tibble::tribble(
  ~STORM_ID, ~ISO_TIME,             ~LAT,  ~LON,   ~USA_WIND_KT, ~SEASON,
  "2001AAA", "2001-06-05T00:00:00", 27.9,  -94.9,  30L,          2001L,
  "2001AAA", "2001-06-09T12:00:00", 29.7,  -95.3,  45L,          2001L,
  "2002BBB", "2002-08-28T06:00:00", 21.4,  -85.1,  35L,          2002L,
  "2002BBB", "2002-09-04T00:00:00", 29.1,  -94.9,  60L,          2002L,
  "2003EEE", "2003-08-14T18:00:00", 18.2,  -80.4,  40L,          2003L
)
for (y in 2001:2003) {
  write_fix(
    dplyr::filter(tracks, SEASON == y),
    "02_wind", "storm_tracks", paste0(y, ".parquet")
  )
}

# --- codebook ---------------------------------------------------------------

codebook <- list(
  version = "fixture",
  tables = list(
    wind = list(description = "ADM2 by storm exposures with nonzero wind"),
    precip = list(description = "ADM2 by local date precipitation")
  )
)
jsonlite::write_json(
  codebook,
  file.path(fixture_dir, "metadata", "codebook.json"),
  auto_unbox = TRUE, pretty = TRUE
)

message("Fixtures written to ", fixture_dir)

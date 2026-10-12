testthat::skip_if_not_installed("sf")

test_that("precip joins retain identities and inclusive in-field windows", {
  x <- plot_fixture()
  spec <- resolve_plot_metrics(x, "exposure", "PRECIP_MM")
  rows <- plot_join_metric(x, spec, "start")
  expect_s3_class(rows, "tbl_df")
  expect_s3_class(rows$date, "Date")
  expect_type(rows$value, "double")
  expect_true(all(rows$PRECIP_IN_FIELD))
  expect_true(all(rows$date >= rows$PRECIP_WINDOW_START))
  expect_true(all(rows$date <= rows$PRECIP_WINDOW_END))
  shared <- rows[rows$ADM2_ID == "PLOT.2" & rows$date == "2020-08-04", ]
  expect_setequal(shared$STORM_ID, c("PLOT_A", "PLOT_B", "PLOT_C"))
  expect_length(unique(shared$.plot_row_id), 1)
  expect_equal(shared$PRECIP_MM, rep(20, 3))
  expect_equal(shared$LOCAL_DATE, rep("2020-08-04", 3))
  inland <- rows[rows$ADM2_ID == "PLOT.4", ]
  expect_equal(unique(inland$STORM_ID), "PLOT_B")
  expect_equal(nrow(inland), 4)
  expect_false(any(inland$WIND_EXPOSURE))
})

test_that("overlapping precip days are deduplicated within the target group", {
  x <- plot_fixture()
  total <- prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE)
  expect_equal(total$value[total$ADM2_ID == "PLOT.2"], 120)
  storm <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    facet_by = "STORM_ID", track = FALSE
  )
  expect_equal(storm$value[storm$ADM2_ID == "PLOT.2"], rep(80, 3))
  name <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    facet_by = "STORM_NAME", track = FALSE
  )
  expect_equal(name$value[name$ADM2_ID == "PLOT.2"], rep(80, 3))
  regional <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    facet_by = "WHO_ENTITY", track = FALSE
  )
  expect_equal(regional$value[regional$ADM2_ID == "PLOT.2"], 120)
  expect_equal(total$ADM2_NAME[total$ADM2_ID == "PLOT.2"], "Inland")
  expect_true(is.na(total$STORM_NAME[total$ADM2_ID == "PLOT.2"]))

  # Equal numeric values on different dates are separate source observations.
  mean <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    aggregate = "mean", track = FALSE
  )
  number <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    aggregate = "n", track = FALSE
  )
  expect_equal(mean$value[mean$ADM2_ID == "PLOT.2"], 20)
  expect_equal(number$value[number$ADM2_ID == "PLOT.2"], 6)
})

test_that("precip thresholds use daily values and counts use distinct units", {
  x <- plot_fixture()
  storms <- prepare_plot_data(x, "count", "PRECIP_MM", track = FALSE)
  days <- prepare_plot_data(x, "count", "PRECIP_MM",
    count_unit = "days", track = FALSE
  )
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.2"], 0)
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.3"], 3)
  expect_equal(days$value[days$ADM2_ID == "PLOT.3"], 6)
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.4"], 1)
  expect_equal(days$value[days$ADM2_ID == "PLOT.4"], 4)
  binary <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    binary = TRUE, track = FALSE
  )
  expect_type(binary$exposed, "logical")
  expect_equal(binary$value[binary$ADM2_ID == "PLOT.2"], 120)
  expect_false(binary$exposed[binary$ADM2_ID == "PLOT.2"])
  expect_true(binary$exposed[binary$ADM2_ID == "PLOT.3"])

  day <- x$precip$ADM2_ID == "PLOT.2" & x$precip$LOCAL_DATE == "2020-08-04"
  x$precip$PRECIP_MM[day] <- 75
  storms <- prepare_plot_data(x, "count", "PRECIP_MM", track = FALSE)
  days <- prepare_plot_data(x, "count", "PRECIP_MM",
    count_unit = "days", track = FALSE
  )
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.2"], 3)
  expect_equal(days$value[days$ADM2_ID == "PLOT.2"], 1)
})

test_that("observed zeros, missing values and out-of-field pairs differ", {
  x <- plot_fixture()
  x$precip <- x$precip[x$precip$ADM2_ID != "PLOT.2", ]
  for (type in c("count", "exposure")) {
    out <- prepare_plot_data(x, type, "PRECIP_MM", track = FALSE)
    expect_equal(out$value[out$ADM2_ID == "PLOT.1"], 0)
    expect_true(is.na(out$value[out$ADM2_ID == "PLOT.2"]))
    expect_true(is.na(out$value[out$ADM2_ID == "PLOT.5"]))
  }
  binary <- prepare_plot_data(x, "exposure", "PRECIP_MM",
    binary = TRUE, track = FALSE
  )
  expect_true(is.na(binary$exposed[binary$ADM2_ID == "PLOT.2"]))
  expect_false(binary$exposed[binary$ADM2_ID == "PLOT.1"])
  x$links$PRECIP_IN_FIELD[x$links$ADM2_ID == "PLOT.4"] <- FALSE
  out <- prepare_plot_data(x, "count", "PRECIP_MM", track = FALSE)
  expect_false("PLOT.4" %in% out$ADM2_ID)
  x$links$PRECIP_IN_FIELD[] <- NA
  expect_error(
    prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE),
    "No plotting data"
  )
})

test_that("wind thresholds and counts preserve raw pairs and local dates", {
  x <- plot_fixture()
  spec <- resolve_plot_metrics(x, "count", "VMAX_SUST_MS")
  rows <- plot_join_metric(x, spec, "start")
  expect_identical(
    rows$LOCAL_DATETIME_STORM_CLOSEST,
    x$wind$LOCAL_DATETIME_STORM_CLOSEST
  )
  expect_identical(rows$STORM_DIST_KM, x$wind$STORM_DIST_KM)
  expect_identical(rows$.plot_row_id, seq_len(nrow(x$wind)))
  x$wind$LOCAL_DATE_STORM_CLOSEST[] <- "2020-08-04"
  storms <- prepare_plot_data(x, "count", "VMAX_SUST_MS", track = FALSE)
  days <- prepare_plot_data(x, "count", "VMAX_SUST_MS",
    count_unit = "days", track = FALSE
  )
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.2"], 3)
  expect_equal(days$value[days$ADM2_ID == "PLOT.2"], 1)
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.3"], 0)
  expect_true(is.na(days$value[days$ADM2_ID == "PLOT.6"]))
  by_storm <- prepare_plot_data(x, "count", "VMAX_SUST_MS",
    count_unit = "days", facet_by = "STORM_ID", track = FALSE
  )
  expect_equal(by_storm$value[by_storm$ADM2_ID == "PLOT.2"], rep(1, 3))
})

test_that("flood start and overlap rules retain events and matching windows", {
  x <- plot_fixture()
  spec <- resolve_plot_metrics(x, "exposure", "PROP_FLOODED_POP")
  start <- plot_join_metric(x, spec, "start")
  overlap <- plot_join_metric(x, spec, "overlap")
  expect_false("PF1" %in% start$FLOOD_ID)
  expect_equal(sum(overlap$FLOOD_ID == "PF1", na.rm = TRUE), 3)
  shared <- overlap[overlap$FLOOD_ID %in% "PF2", ]
  expect_length(unique(shared$.plot_row_id), 1)
  expect_equal(unique(shared$FLOOD_START_DATE), "2020-08-03")
  expect_equal(unique(shared$FLOOD_END_DATE), "2020-08-06")
  expect_true(all(overlap$FLOOD_IN_FIELD))

  # An ongoing event touching the first window day is included in overlap mode.
  x$flood$FLOOD_END_DATE[1] <- "2020-08-02"
  boundary <- plot_join_metric(x, spec, "overlap")
  expect_equal(boundary$STORM_ID[boundary$FLOOD_ID %in% "PF1"], "PLOT_A")
})

test_that("flood days union events after clipping every storm window", {
  x <- plot_fixture()
  start <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", track = FALSE
  )
  expect_equal(start$value[start$ADM2_ID == "PLOT.1"], 4)
  expect_equal(start$value[start$ADM2_ID == "PLOT.2"], 0)
  expect_true(is.na(start$value[start$ADM2_ID == "PLOT.3"]))
  expect_true(is.na(start$value[start$ADM2_ID == "PLOT.5"]))
  attr(x, "filters_resolved")$flood_match <- "overlap"
  overlap <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", track = FALSE
  )
  expect_equal(overlap$value[overlap$ADM2_ID == "PLOT.1"], 5)
  facets <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", facet_by = "STORM_ID", track = FALSE
  )
  expect_equal(facets$value[facets$ADM2_ID == "PLOT.1"], c(4, 4, 3))
  storms <- prepare_plot_data(x, "count", "PROP_FLOODED_POP", track = FALSE)
  expect_equal(storms$value[storms$ADM2_ID == "PLOT.1"], 3)
  sums <- prepare_plot_data(x, "exposure", "PROP_FLOODED_POP",
    aggregate = "sum", track = FALSE
  )
  expect_equal(sums$value[sums$ADM2_ID == "PLOT.1"], 0.15)
  attr(x, "filters_resolved") <- NULL
  plain <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", track = FALSE
  )
  expect_equal(plain$value, start$value)
  expect_equal(attr(plain, "plot_info")$flood_match, "start")
})

test_that("flood interval unions handle nested and disjoint windows", {
  x <- plot_fixture()
  x <- subset_plot_data(x, storm = "PLOT_A", adm2 = "PLOT.1")
  x$links$FLOOD_WINDOW_START <- as.Date("2020-08-01")
  x$links$FLOOD_WINDOW_END <- as.Date("2020-08-20")
  x$flood <- x$flood[rep(1, 4), ]
  x$flood$FLOOD_ID <- paste0("nested", 1:4)
  x$flood$FLOOD_START_DATE <- c(
    "2020-08-01", "2020-08-02", "2020-08-12", "2020-08-13"
  )
  x$flood$FLOOD_END_DATE <- c(
    "2020-08-10", "2020-08-03", "2020-08-12", "2020-08-14"
  )
  out <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", track = FALSE
  )
  expect_equal(out$value, 13)
  x$flood$FLOOD_END_DATE[] <- NA_character_
  out <- prepare_plot_data(x, "count", "PROP_FLOODED_POP",
    count_unit = "days", track = FALSE
  )
  expect_true(is.na(out$value))
})

test_that("distance summaries are numeric minima of all published matches", {
  x <- plot_fixture()
  x$links$STORM_DIST_KM[x$links$ADM2_ID == "PLOT.2"] <- c(100, 20, 80)
  out <- prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE)
  expect_type(out$STORM_DIST_KM, "double")
  expect_equal(out$STORM_DIST_KM[out$ADM2_ID == "PLOT.2"], 20)
  x$tracks$LON[] <- 0
  unchanged <- prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE)
  expect_equal(out$STORM_DIST_KM, unchanged$STORM_DIST_KM)
  x$wind$STORM_DIST_KM[x$wind$ADM2_ID == "PLOT.2"] <- c(100, 101, NA)
  count <- prepare_plot_data(x, "count", "STORM_DIST_KM", track = FALSE)
  expect_equal(count$value[count$ADM2_ID == "PLOT.2"], 1)
  expect_equal(count$STORM_DIST_KM[count$ADM2_ID == "PLOT.2"], 100)
  x$wind$STORM_DIST_KM[] <- NA_real_
  missing <- prepare_plot_data(x, "exposure", "VMAX_SUST_MS", track = FALSE)
  expect_true(all(is.na(missing$STORM_DIST_KM)))
})

test_that("joins validate dates and accept empty source tables", {
  x <- plot_fixture()
  x$precip$LOCAL_DATE[1] <- "invalid"
  expect_error(
    prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE),
    "LOCAL_DATE.*valid calendar dates"
  )
  x$precip <- x$precip[0, ]
  out <- prepare_plot_data(x, "exposure", "PRECIP_MM", track = FALSE)
  expect_equal(nrow(out), 6)
  expect_true(all(is.na(out$value)))
})

# The flood note is shown once per session, so whether a query shows it
# depends on test order; tests that are not about it silence it by class.
suppress_flood_reports <- function(expr) {
  withCallingHandlers(
    expr,
    gtropic_warning_flood_reports = function(w) invokeRestart("muffleWarning")
  )
}

test_that("published flood columns are renamed and other columns kept", {
  published <- tibble::tibble(
    ADM2_ID = "A",
    FLOOD_ID = "F1",
    PROP_FLOODED_AREA = 0.5,
    PROP_FLOODED_POP = 0.25,
    FLOODED_POP_CENTROID = TRUE
  )
  out <- rename_flood_columns(published)
  expect_named(
    out,
    c(
      "ADM2_ID",
      "FLOOD_ID",
      "PROP_AREA_IN_FLOOD_REPORT",
      "PROP_POP_IN_FLOOD_REPORT",
      "POP_CENTROID_IN_FLOOD_REPORT"
    )
  )
  expect_identical(unname(as.list(out[3:5])), unname(as.list(published[3:5])))

  # Files without some (or any) of the published columns are left as they are.
  expect_named(
    rename_flood_columns(published[c("ADM2_ID", "PROP_FLOODED_POP")]),
    c("ADM2_ID", "PROP_POP_IN_FLOOD_REPORT")
  )
  expect_identical(
    rename_flood_columns(published[1:2]),
    published[1:2]
  )
  expect_null(rename_flood_columns(NULL))
})

test_that("gtropic_data() returns flood columns under the gtropic names", {
  local_golden_collection()
  res <- suppress_flood_reports(gtropic_data(
    WHO_ENTITY = "United States",
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "flood", "storm_metadata"),
    cache = FALSE,
    quiet = TRUE,
    confirm = FALSE
  ))
  expect_gt(nrow(res$flood), 0L)
  expect_true(all(unname(FLOOD_COLUMN_NAMES) %in% names(res$flood)))
  expect_false(any(names(FLOOD_COLUMN_NAMES) %in% names(res$flood)))
})

# A flood report in unit A, no report in unit B, and a window in unit C that
# predates the Groundsource record.
flood_binary_fixture <- function() {
  closest <- as.Date(c("2020-08-01", "2020-08-01", "1999-08-01"))
  list(
    flood = tibble::tibble(
      ADM2_ID = "A",
      FLOOD_ID = "F1",
      FLOOD_START_DATE = as.Date("2020-08-01"),
      FLOOD_END_DATE = as.Date("2020-08-02"),
      PROP_AREA_IN_FLOOD_REPORT = 0.3,
      PROP_POP_IN_FLOOD_REPORT = 0.2
    ),
    links = tibble::tibble(
      ADM2_ID = c("A", "B", "C"),
      STORM_ID = c("S1", "S1", "S0"),
      STORM_DIST_KM = 10,
      LOCAL_DATE_STORM_CLOSEST = closest,
      FLOOD_IN_FIELD = TRUE,
      FLOOD_WINDOW_START = closest - 2,
      FLOOD_WINDOW_END = closest + 1
    )
  )
}

test_that("the default exposure map draws flood as a yes/no panel", {
  data <- flood_binary_fixture()
  default <- resolve_plot_metrics(data, "exposure")
  expect_identical(default$metric, "PROP_AREA_IN_FLOOD_REPORT")
  expect_true(default$binary)
  expect_true(default$threshold_active)
  expect_identical(default$legend, "Flood reported")
  expect_identical(
    plot_metric_display_label(default),
    "Flood reported (area in flood report > 0)"
  )
  expect_identical(
    plot_binary_labels(default),
    c("No flood reported", "Flood reported")
  )

  # Naming the metric maps the share itself; counts are unchanged.
  named <- resolve_plot_metrics(data, "exposure", "PROP_AREA_IN_FLOOD_REPORT")
  expect_false(named$binary)
  expect_false(named$threshold_active)
  expect_false(resolve_plot_metrics(data, "count")$binary)
  population <- resolve_plot_metrics(
    data,
    "exposure",
    "PROP_POP_IN_FLOOD_REPORT",
    binary = TRUE
  )
  expect_identical(
    plot_metric_display_label(population),
    "Flood reported (population in flood report > 0)"
  )
})

test_that("yes/no flood panels separate no report from no data", {
  data <- flood_binary_fixture()
  spec <- resolve_plot_metrics(data, "exposure")
  rows <- plot_join_metric(data, spec)
  out <- plot_aggregate_metric(rows, spec, "exposure", binary = TRUE)
  out <- out[order(out$ADM2_ID), ]
  expect_identical(out$ADM2_ID, c("A", "B", "C"))
  # Reported; in the record but not reported; before the record.
  expect_identical(out$exposed, c(TRUE, FALSE, NA))
  expect_false(".no_flood_report" %in% names(out))

  # A continuous panel keeps unreported units missing, never zero.
  named <- resolve_plot_metrics(data, "exposure", "PROP_AREA_IN_FLOOD_REPORT")
  values <- plot_aggregate_metric(
    plot_join_metric(data, named),
    named,
    "exposure"
  )
  values <- values[order(values$ADM2_ID), ]
  expect_identical(values$value, c(0.3, NA, NA))
})

test_that("flood windows count as recorded only inside the Groundsource period", {
  expect_identical(
    flood_window_recorded(
      as.Date(c("2000-01-01", "1999-12-31", "2026-01-20", NA)),
      as.Date(c("2000-01-03", "2000-01-02", "2026-01-23", "2020-01-01"))
    ),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("static and interactive default maps label the flood panel", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  skip_if_not_installed("ggnewscale")
  x <- gtropic_example()
  p <- gtropic_plot(x, basemap = "none", track = FALSE)
  info <- attr(p$data, "plot_info")
  expect_identical(info$metrics$binary, c(FALSE, FALSE, TRUE))
  flood <- sf::st_drop_geometry(p$data)
  flood <- flood[flood$metric == "PROP_AREA_IN_FLOOD_REPORT", ]
  flood <- flood[order(flood$ADM2_ID), ]
  # PLOT.2's report covers none of the unit; PLOT.3 and PLOT.6 had none.
  expect_identical(
    flood$exposed[flood$ADM2_ID %in% paste0("PLOT.", 1:4)],
    c(TRUE, FALSE, FALSE, TRUE)
  )
  key_labels <- unlist(lapply(p$layers, function(layer) {
    if (is.data.frame(layer$data)) layer$data$label
  }))
  expect_true(all(c("No flood reported", "Flood reported") %in% key_labels))

  skip_if_not_installed("leaflet")
  m <- gtropic_plot(x, interactive = TRUE, basemap = "none", track = FALSE)
  legends <- Filter(
    function(call) identical(call$method, "addLegend"),
    m$x$calls
  )
  legend_labels <- unlist(lapply(legends, function(call) {
    call$args[[1L]]$labels
  }))
  expect_true(all(c("No flood reported", "Flood reported") %in% legend_labels))
  popups <- unlist(lapply(
    Filter(function(call) identical(call$method, "addPolygons"), m$x$calls),
    function(call) call$args[[5L]]
  ))
  expect_true(any(grepl("<b>Flood reported:</b>", popups, fixed = TRUE)))
})

test_that("gtropic_data() warns about flood rows and keeps the note", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  local_golden_collection()
  query <- function(...) {
    gtropic_data(
      WHO_ENTITY = "United States",
      ...,
      cache = FALSE,
      quiet = TRUE,
      confirm = FALSE
    )
  }
  expect_warning(
    res <- query(
      date_range = c("2002-01-01", "2003-12-31"),
      tables = c("wind", "flood", "storm_metadata")
    ),
    "places named in news reports",
    class = "gtropic_warning_flood_reports"
  )
  expect_gt(nrow(res$flood), 0L)
  expect_identical(gtropic_diagnostics(res)$flood_reports, FLOOD_REPORTS_NOTE)

  # No flood table, no warning and no note.
  no_flood <- query(
    date_range = c("2002-01-01", "2003-12-31"),
    tables = c("wind", "storm_metadata")
  )
  expect_null(gtropic_diagnostics(no_flood)$flood_reports)
})

test_that("the flood warning is shown once per session", {
  withr::local_options(rlib_warning_verbosity = "default")
  expect_warning(
    warn_flood_reports(id = "gtropic_flood_reports_test"),
    class = "gtropic_warning_flood_reports"
  )
  expect_no_warning(warn_flood_reports(id = "gtropic_flood_reports_test"))
})

test_that("maps with flood panels carry the flood note once", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  skip_if_not_installed("ggnewscale")
  x <- gtropic_example()
  count_lines <- function(p) {
    lines <- strsplit(p$labels$caption, "\n", fixed = TRUE)[[1L]]
    sum(startsWith(lines, "Flood data: places named in news reports"))
  }
  two_flood_panels <- gtropic_plot(
    x,
    metric = c("PROP_AREA_IN_FLOOD_REPORT", "PROP_POP_IN_FLOOD_REPORT"),
    basemap = "none",
    track = FALSE
  )
  expect_identical(count_lines(two_flood_panels), 1L)
  expect_match(
    two_flood_panels$labels$caption,
    "Flood parameters:[^\n]*\nFlood data: places named in news reports"
  )
  wind_only <- gtropic_plot(
    x,
    metric = "VMAX_SUST_MS",
    basemap = "none",
    track = FALSE
  )
  expect_identical(count_lines(wind_only), 0L)
})

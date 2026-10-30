testthat::skip_if_not_installed("sf")
testthat::skip_if_not_installed("ggplot2")

test_that("plotting fixtures have the published schemas and edge cases", {
  x <- plot_fixture()
  expect_s3_class(x$geometry, "sf")
  expect_equal(nrow(x$geometry), 6)
  expect_equal(nrow(x$storm_metadata), 3)
  expect_named(x$precip, c("ADM2_ID", "LOCAL_DATE", "PRECIP_MM"))
  expect_named(x$flood, c(
    "ADM2_ID", "FLOOD_ID", "FLOOD_START_DATE",
    "FLOOD_END_DATE", "PROP_FLOODED_AREA", "PROP_FLOODED_POP",
    "FLOODED_POP_CENTROID"
  ))
  expect_true(any(abs(diff(x$tracks$LON)) > 180))
  expect_equal(nrow(x$precip), 60)
})

test_that("normalization retains sf, nested codebook and provenance", {
  x <- plot_fixture()
  attr(x, "query") <- quote(gtropic_data(WHO_ENTITY = "Example Coast"))
  class(x) <- c("gtropic_data", "list")
  y <- normalize_plot_data(x)
  expect_type(y, "list")
  expect_false(inherits(y, "gtropic_data"))
  expect_identical(y$geometry, x$geometry)
  expect_identical(y$codebook, x$codebook)
  expect_identical(attr(y, "query"), attr(x, "query"))
  expect_identical(attr(y, "filters_resolved"), attr(x, "filters_resolved"))
  expect_error(normalize_plot_data(list(1)), "uniquely named")
  expect_error(normalize_plot_data(list(wind = 1)), "data frame")
  duplicate <- stats::setNames(list(1, 2), c("wind", "wind"))
  expect_error(normalize_plot_data(duplicate), "uniquely named")
})

test_that("missing tables and columns give a copyable amended query", {
  x <- plot_fixture()
  x$geometry <- NULL
  attr(x, "query") <- quote(gtropic_data(
    WHO_ENTITY = "United States",
    date_range = c(2005, 2005), tables = c("wind", "adm2")
  ))
  expect_error(prepare_plot_data(x, "exposure"), "geometry")
  hint <- plot_query_hint(x, c("wind", "adm2", "geometry"))
  call <- parse(text = hint)[[1]]
  expect_equal(call$WHO_ENTITY, "United States")
  expect_equal(eval(call$date_range), c(2005, 2005))
  expect_true("geometry" %in% eval(call$tables))
  x$tracks <- NULL
  expect_error(prepare_plot_data(x, "track"), "tracks")
  x <- plot_fixture()
  x$wind$VMAX_SUST_MS <- NULL
  expect_error(prepare_plot_data(x, "exposure", "VMAX_SUST_MS"), "VMAX_SUST_MS")
  expect_error(prepare_plot_data(x, "exposure", "MADE_UP"), "Available metrics")
})

test_that("metrics carry defaults, codebook units and explicit overrides", {
  x <- plot_fixture()
  expect_equal(
    resolve_plot_metrics(x, "exposure")$metric,
    c("VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_AREA")
  )
  expect_equal(
    attr(prepare_plot_data(x, "exposure", track = FALSE), "plot_info")$metrics$metric,
    c("VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_AREA")
  )
  expect_equal(resolve_plot_metrics(x, "count")$metric, "VMAX_SUST_MS")
  expect_equal(resolve_plot_metrics(x, "track")$metric, "USA_WIND")
  x$flood <- NULL
  expect_equal(
    resolve_plot_metrics(x, "exposure")$metric,
    c("VMAX_SUST_MS", "PRECIP_MM")
  )
  x <- plot_fixture()
  spec <- resolve_plot_metrics(x, "count", c(
    "VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_POP", "STORM_DIST_KM"
  ))
  expect_equal(spec$threshold, c(17.5, 75, 0, 100))
  expect_equal(spec$operator, c(">=", ">=", ">", "<="))
  expect_true(all(spec$aggregate == "n"))
  expect_error(resolve_plot_metrics(x, "count", "VMAX_GUST_MS"), "explicit")
  spec <- resolve_plot_metrics(x, "count", "VMAX_GUST_MS",
    threshold = list(VMAX_GUST_MS = 21)
  )
  expect_equal(spec$threshold, 21)
  expect_error(
    resolve_plot_metrics(x, "count", threshold = list(PRECIP_MM = 80)),
    "selected metrics"
  )
  expect_error(
    resolve_plot_metrics(x, "count", threshold = list(VMAX_SUST_MS = NA_real_)),
    "finite"
  )
})

test_that("tracks need no geometry and retain metadata", {
  x <- plot_fixture()
  x$geometry <- NULL
  out <- prepare_plot_data(x, "track", storm = "Dateline", facet_by = "SEASON")
  expect_s3_class(out, "tbl_df")
  expect_equal(unique(out$STORM_ID), "PLOT_C")
  expect_s3_class(out$ISO_TIME, "POSIXct")
  info <- attr(out, "plot_info")
  expect_equal(info$facet_by, "SEASON")
  expect_equal(info$facets, 2020L)
  expect_equal(info$metrics$unit, "knots")
  expect_equal(info$flood_match, "start")
  attr(x, "filters_resolved")$flood_match <- "overlap"
  info <- attr(prepare_plot_data(x, "track"), "plot_info")
  expect_equal(info$flood_match, "overlap")
  attr(x, "filters_resolved") <- NULL
  info <- attr(prepare_plot_data(x, "track"), "plot_info")
  expect_equal(info$flood_match, "start")
  expect_error(
    prepare_plot_data(x, "track", storm = "absent"),
    "No track observations"
  )
  expect_error(
    prepare_plot_data(x, "track", facet_by = "absent"),
    "Valid columns"
  )
  expect_error(prepare_plot_data(x, "track", unused = 1), "Unused preparation")
  expect_error(prepare_plot_data(x, "track", binary = TRUE), "binary")
})

test_that("UTC parsing honors both offset directions and date changes", {
  out <- plot_parse_utc(
    c("2020-08-04T23:00:00-05:00", "2020-08-04T01:00:00+12:00"),
    require_offset = TRUE
  )
  expect_equal(
    format(out, tz = "UTC"),
    c("2020-08-05 04:00:00", "2020-08-03 13:00:00")
  )
  expect_error(
    plot_parse_utc("2020-08-04T23:00:00", require_offset = TRUE),
    "offset"
  )
  expect_error(plot_parse_utc("invalid"), "invalid timestamps")
})

test_that("theme, scale and caption defaults need no rendering", {
  expect_s3_class(theme_gtropic(), "theme")
  expect_equal(theme_gtropic()$legend.position, "right")
  expect_error(theme_gtropic(0), "positive")
  continuous <- scale_fill_gtropic_c(metric = "PRECIP_MM")
  discrete <- scale_fill_gtropic_d()
  expect_false(continuous$is_discrete())
  expect_true(discrete$is_discrete())
  expect_equal(continuous$name, "Precipitation (mm)")
  expect_equal(continuous$na.value, "grey90")
  expect_error(scale_fill_gtropic_c(direction = 0), "direction")
  x <- plot_fixture()
  prepared <- prepare_plot_data(x, "count", "PRECIP_MM", track = FALSE)
  info <- attr(prepared, "plot_info")
  caption <- plot_caption(info)
  expect_match(
    caption,
    "Precip parameters: radius=400 km, window=-2/+1 days, aggregate=n",
    fixed = TRUE
  )
  expect_equal(plot_caption(info, "Custom caption"), "Custom caption")
})

test_that("Arrow normalization and type errors are local and informative", {
  x <- plot_fixture()
  wind <- x$wind
  x$wind <- arrow::Table$create(wind)
  expect_equal(normalize_plot_data(x)$wind, wind)
  x$wind <- wind
  x$wind$VMAX_SUST_MS <- as.character(x$wind$VMAX_SUST_MS)
  expect_error(prepare_plot_data(x, "exposure"), "must be numeric")
  x <- plot_fixture()
  x$links$PRECIP_IN_FIELD <- as.integer(x$links$PRECIP_IN_FIELD)
  expect_error(prepare_plot_data(x, "exposure", "PRECIP_MM"), "logical")
  expect_error(plot_parse_utc(as.POSIXct(NA)), "missing timestamps")
})

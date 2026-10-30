testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("the simplified entrypoint exposes supported output modes", {
  expect_equal(
    eval(formals(gtropic_plot)$type),
    c("exposure", "count", "track")
  )
  expect_false("animate" %in% names(formals(gtropic_plot)))
  expect_false("frame_by" %in% names(formals(gtropic_plot)))
  expect_true(all(c(
    "track_map_key", "track_labels", "track_interval", "gridlines"
  ) %in% names(formals(gtropic_plot))))
  expect_error(gtropic_plot(plot_fixture(), type = "timeline"), "type")
})

test_that("metric order controls facets, scales, and caption blocks", {
  testthat::skip_if_not_installed("ggnewscale")
  metrics <- c("VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_AREA")
  p <- gtropic_plot(plot_fixture(), metric = metrics, basemap = "none", track = FALSE)
  built <- ggplot2::ggplot_build(p)
  expect_equal(as.character(built$layout$layout$metric), metrics)
  expect_equal(attr(p$data, "plot_info")$metric_labels, c(
    VMAX_SUST_MS = "Maximum sustained wind (m/s)",
    PRECIP_MM = "Cumulative precipitation (mm)",
    PROP_FLOODED_AREA = "Maximum flooded area (proportion, 0-1)"
  ))
  positions <- vapply(
    c("Wind parameters", "Precip parameters", "Flood parameters"),
    regexpr, integer(1), text = p$labels$caption, fixed = TRUE
  )
  expect_true(all(diff(positions) > 0))
})

test_that("captions retain one parameter line per same-hazard metric", {
  p <- gtropic_plot(
    plot_fixture(),
    metric = c("VMAX_SUST_MS", "STORM_DIST_KM"),
    aggregate = c("max", "min"),
    basemap = "none", track = FALSE
  )
  wind_lines <- grep(
    "^Wind parameters:", strsplit(p$labels$caption, "\\n")[[1L]], value = TRUE
  )
  expect_identical(wind_lines, c(
    "Wind parameters: radius=300 km, aggregate=max",
    "Wind parameters: radius=300 km, aggregate=min"
  ))
})

test_that("subtitle, captions, and gridlines describe the plotted map", {
  p <- gtropic_plot(plot_fixture(), metric = "VMAX_SUST_MS", basemap = "none",
    track = FALSE, crs = 4326
  )
  expect_equal(p$labels$subtitle, "Aggregated across 08/04/2020 - 08/06/2020")
  expect_match(p$labels$caption, "Wind parameters: radius=200 km, aggregate=max", fixed = TRUE)
  expect_match(p$labels$caption, "CRS: EPSG:4326", fixed = TRUE)
  expect_match(
    plot_wrap_caption(paste(rep("caption", 30), collapse = " ")),
    "\n", fixed = TRUE
  )
  no_grid <- gtropic_plot(plot_fixture(), metric = "VMAX_SUST_MS",
    basemap = "none", track = FALSE, gridlines = FALSE
  )
  expect_s3_class(no_grid$theme$panel.grid, "element_blank")
  expect_s3_class(no_grid$theme$axis.text, "element_blank")
})

test_that("single-storm exposure defaults identify the plotted storm", {
  testthat::skip_if_not_installed("ggnewscale")
  p <- gtropic_plot(plot_fixture(), storm = "PLOT_A", basemap = "none",
    crs = 4326
  )
  expect_identical(attr(p$data, "plot_info")$metrics$metric, c(
    "VMAX_SUST_MS", "PRECIP_MM", "PROP_FLOODED_AREA"
  ))
  expect_equal(p$labels$title, "Alpha: Exposure metrics")
  expect_match(
    gsub("\\s+", " ", p$labels$caption),
    "Storm track interval: every 6 hours", fixed = TRUE
  )
})

test_that("automatic plot labels document storm sets and active thresholds", {
  x <- plot_fixture()
  p <- gtropic_plot(x, metric = "VMAX_SUST_MS", basemap = "none", track = FALSE)
  expect_equal(p$labels$title, "Maximum sustained wind (m/s)")

  multi <- gtropic_plot(x, metric = c("VMAX_SUST_MS", "PRECIP_MM"),
    basemap = "none", track = FALSE
  )
  expect_equal(multi$labels$title, "Exposure metrics")

  count <- gtropic_plot(x, type = "count", metric = "PRECIP_MM",
    basemap = "none", track = FALSE
  )
  labels <- attr(count$data, "plot_info")$metric_labels
  expect_equal(count$labels$title, "Storm-exposures (n): Precipitation (mm) >= 75 mm")
  expect_equal(labels[["PRECIP_MM"]], "Precipitation (mm) >= 75 mm")

  binary <- gtropic_plot(x, metric = "PRECIP_MM", binary = TRUE,
    basemap = "none", track = FALSE
  )
  expect_equal(binary$labels$title, "Cumulative precipitation (mm) >= 75 mm")

  count_days <- gtropic_plot(x, type = "count", count_unit = "days",
    metric = c("VMAX_SUST_MS", "PRECIP_MM"), basemap = "none", track = FALSE
  )
  expect_equal(count_days$labels$title, "Exposure-days (n) per metric")
  count_storms <- gtropic_plot(x, type = "count", count_unit = "storms",
    metric = c("VMAX_SUST_MS", "PRECIP_MM"), basemap = "none", track = FALSE
  )
  expect_equal(count_storms$labels$title, "Storm-exposures (n) per metric")

  track_one <- gtropic_plot(x, type = "track", storm = "PLOT_A", basemap = "none")
  expect_equal(track_one$labels$title, "Alpha: Storm track")
  track_many <- gtropic_plot(x, type = "track", basemap = "none")
  expect_equal(track_many$labels$title, "Storm tracks")
})

test_that("caption provenance and hazard settings come from the supplied data", {
  x <- plot_fixture()
  attr(x, "manifest") <- list(
    datasets = tibble::tibble(
      source_role = c("Historical", "Current-year"),
      dataset_doi = c("doi:10.1/historical", "doi:10.1/current"),
      dataset_version = c("3.0", "14.0")
    ),
    retrieved_at = as.POSIXct("2020-08-08 12:00:00", tz = "America/New_York"),
    gtropic_version = "0.0.0.9000"
  )
  attr(x, "retrieved_at") <- as.POSIXct("2020-08-08 13:00:00", tz = "America/New_York")
  attr(x, "gtropic_version") <- "0.0.0.9000"
  p <- gtropic_plot(x, metric = c("PRECIP_MM", "PROP_FLOODED_AREA"),
    basemap = "none", track = FALSE, crs = 4326
  )
  expect_identical(strsplit(p$labels$caption, "\\n", fixed = FALSE)[[1L]], c(
    "Source: G-TROPIC R package v.0.0.0.9000",
    "Data retrieved on 2020-08-08 13:00:00 EDT",
    paste0(
      "Datasets: Historical v.3.0 (doi:10.1/historical) / ",
      "Current-year v.14.0 (doi:10.1/current)"
    ),
    "Precip parameters: radius=400 km, window=-2/+1 days, aggregate=sum",
    "Flood parameters: radius=400 km, window=-2/+1 days, aggregate=max, match=start",
    "CRS: EPSG:4326"
  ))
  expect_equal(
    plot_source_caption(),
    paste0("Source: G-TROPIC R package v.", utils::packageVersion("gtropic"))
  )
  expect_equal(
    plot_provenance_caption(list(
      manifest = list(datasets = tibble::tibble(
        source_role = "Current-year",
        dataset_doi = "10.1/current",
        dataset_version = "14.0"
      ))
    )),
    c(
      paste0("Source: G-TROPIC R package v.", utils::packageVersion("gtropic")),
      "Datasets: Current-year v.14.0 (doi:10.1/current)"
    )
  )
})

test_that("plot provenance retains roles of datasets recorded by new manifests", {
  current_only <- plot_provenance_caption(list(
    manifest = list(datasets = tibble::tibble(
      source_role = "Current-year",
      dataset_doi = "doi:10.1/current", dataset_version = "14.0"
    ))
  ))
  historical_and_current <- plot_provenance_caption(list(
    manifest = list(datasets = tibble::tibble(
      source_role = c("Current-year", "Historical"),
      dataset_doi = c("doi:10.1/current", "doi:10.1/historical"),
      dataset_version = c("14.0", "3.0")
    ))
  ))

  expect_identical(tail(current_only, 1L), "Datasets: Current-year v.14.0 (doi:10.1/current)")
  expect_identical(
    tail(historical_and_current, 1L),
    paste0(
      "Datasets: Historical v.3.0 (doi:10.1/historical) / ",
      "Current-year v.14.0 (doi:10.1/current)"
    )
  )
})

test_that("wind radii are rounded and displayed without scientific notation", {
  x <- plot_fixture()
  x$wind$STORM_DIST_KM[1L] <- 200.7
  p <- gtropic_plot(x, metric = "VMAX_SUST_MS", basemap = "none", track = FALSE)
  expect_match(p$labels$caption, "Wind parameters: radius=201 km", fixed = TRUE)
  expect_equal(
    plot_format_number(c(1000000, 0.0000123, 200)),
    c("1000000", "0.0000123", "200")
  )
})

test_that("per-metric options recycle or align with metric order", {
  metrics <- c("PRECIP_MM", "VMAX_SUST_MS")
  prepared <- prepare_plot_data(plot_fixture(), "exposure", metrics,
    track = FALSE, aggregate = c("mean", "max"),
    palette = c("mako", "inferno"), direction = c(-1, 1)
  )
  info <- attr(prepared, "plot_info")
  expect_equal(info$metrics$metric, metrics)
  expect_equal(info$metrics$aggregate, c("mean", "max"))
  expect_equal(info$metrics$palette, c("mako", "inferno"))
  expect_equal(info$metrics$direction, c(-1, 1))
  scalar <- prepare_plot_data(plot_fixture(), "exposure", metrics,
    track = FALSE, palette = "magma", direction = -1
  )
  expect_equal(scalar_info <- attr(scalar, "plot_info")$metrics$palette,
    rep("magma", 2)
  )
  expect_equal(scalar_info, rep("magma", 2))
  expect_error(
    gtropic_plot(plot_fixture(), metric = metrics, aggregate = c("max", "sum", "mean")),
    "length"
  )
  expect_error(
    gtropic_plot(plot_fixture(), metric = metrics,
      palette = c(VMAX_SUST_MS = "magma")),
    "every selected metric"
  )
})

test_that("track legend positions are validated", {
  expect_no_error(gtropic_plot(
    plot_fixture(), basemap = "none", track = FALSE,
    track_legend_position = "top left"
  ))
  expect_error(gtropic_plot(
    plot_fixture(), track_legend_position = "centre"
  ), "track_legend_position")
})

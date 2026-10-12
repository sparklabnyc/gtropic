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
    VMAX_SUST_MS = "Maximum sustained wind (meters per second)",
    PRECIP_MM = "Precipitation (millimeters per local day)",
    PROP_FLOODED_AREA = "Flooded area (proportion, 0-1)"
  ))
  positions <- vapply(
    c("wind radius", "precip radius", "flood radius"),
    regexpr, integer(1), text = p$labels$caption, fixed = TRUE
  )
  expect_true(all(diff(positions) > 0))
})

test_that("subtitle, captions, and gridlines describe the plotted map", {
  p <- gtropic_plot(plot_fixture(), metric = "VMAX_SUST_MS", basemap = "none",
    track = FALSE, crs = 4326
  )
  expect_equal(p$labels$subtitle, "08/04/2020 - 08/06/2020")
  expect_match(p$labels$caption, "wind radius: 200 km", fixed = TRUE)
  expect_match(p$labels$caption, "wind aggregate: max", fixed = TRUE)
  expect_match(p$labels$caption, "CRS: EPSG:4326", fixed = TRUE)
  expect_match(
    plot_wrap_caption(paste(rep("caption", 30), collapse = " ")),
    "\n", fixed = TRUE
  )
  no_grid <- gtropic_plot(plot_fixture(), basemap = "none", track = FALSE,
    gridlines = FALSE
  )
  expect_s3_class(no_grid$theme$panel.grid, "element_blank")
  expect_s3_class(no_grid$theme$axis.text, "element_blank")
})

test_that("single-track defaults identify the plotted storm", {
  p <- gtropic_plot(plot_fixture(), storm = "PLOT_A", basemap = "none",
    crs = 4326
  )
  expect_equal(p$labels$title, "Tropical cyclone exposure: Alpha")
  expect_match(
    gsub("\\s+", " ", p$labels$caption),
    "track interval: every 6 hours", fixed = TRUE
  )
})

test_that("wind radii are rounded and displayed without scientific notation", {
  x <- plot_fixture()
  x$wind$STORM_DIST_KM[1L] <- 200.7
  p <- gtropic_plot(x, metric = "VMAX_SUST_MS", basemap = "none", track = FALSE)
  expect_match(p$labels$caption, "wind radius: 201 km", fixed = TRUE)
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

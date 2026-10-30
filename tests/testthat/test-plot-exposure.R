testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("exposure layers use each metric's resolved scale settings", {
  testthat::skip_if_not_installed("ggnewscale")
  data <- prepare_plot_data(plot_fixture(), "exposure",
    c("PRECIP_MM", "VMAX_SUST_MS"), track = FALSE,
    palette = c("mako", "inferno"), direction = c(-1, 1)
  )
  p <- build_exposure_plot(data, plot_cartography(data, basemap = "none"))
  fills <- Filter(function(scale) any(grepl("^fill", scale$aesthetics)), p$scales$scales)
  expect_equal(length(fills), 2)
  expect_equal(fills[[1]]$palette(c(0, 1)),
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1)$palette(c(0, 1))
  )
  expect_identical(fills[[1]]$guide, "none")
  expect_identical(fills[[2]]$guide, "none")
  key_layers <- Filter(function(layer) inherits(layer$geom, "GeomTile"), p$layers)
  expect_length(key_layers, 2)
  expect_equal(unname(
    vapply(key_layers, function(layer) as.character(layer$data$metric[1L]), character(1))
  ),
    c("PRECIP_MM", "VMAX_SUST_MS")
  )
  key <- key_layers[[1L]]$data
  expect_equal(length(unique(key$y)), 1L)
  expect_lt(key$y[1L], plot_cartography(data, basemap = "none")$bbox[3])
  labels <- Filter(function(layer) inherits(layer$geom, "GeomText"), p$layers)[[1L]]$data
  expect_match(labels$label[1L], "^[0-9]+\\.[0-9]{2}$")
  for (position in c("top", "bottom", "left", "right", "top left", "top right",
    "bottom left", "bottom right")) {
    p <- gtropic_plot(plot_fixture(), metric = "VMAX_SUST_MS", basemap = "none",
      track = FALSE, track_legend_position = position
    )
    key <- Filter(function(layer) inherits(layer$geom, "GeomTile"), p$layers)[[1L]]$data
    expect_true(all(key$x >= p$coordinates$limits$x[1L]))
    expect_true(all(key$x <= p$coordinates$limits$x[2L]))
    expect_true(all(key$y >= p$coordinates$limits$y[1L]))
    expect_true(all(key$y <= p$coordinates$limits$y[2L]))
  }
})

test_that("track overlays receive the Saffir-Simpson intensity key", {
  testthat::skip_if_not_installed("ggnewscale")
  p <- gtropic_plot(plot_fixture(), metric = "VMAX_SUST_MS", basemap = "none")
  points <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), p$layers)
  expect_true(any(vapply(points, function(layer) {
    identical(rlang::as_label(layer$mapping$fill), ".track_category")
  }, logical(1))))
  track_scale <- utils::tail(p$scales$scales, 1L)[[1]]
  expect_equal(track_scale$guide$params$position, "bottom")
  grob <- ggplot2::ggplotGrob(p)
  bottom <- grob$grobs[[match("guide-box-bottom", grob$layout$name)]]
  expect_s3_class(bottom, "gtable")
})

test_that("static wind legends retain every Saffir-Simpson category", {
  testthat::skip_if_not_installed("ggnewscale")
  x <- plot_fixture()
  x$tracks$USA_WIND[] <- 50
  key <- saffir_simpson()
  expect_false(any(
    as.character(plot_categorize_track_wind(x$tracks$USA_WIND, key)) == "Cat 2"
  ))

  expect_complete_key <- function(p) {
    scales <- Filter(function(scale) {
      "fill" %in% scale$aesthetics && inherits(scale, "ScaleDiscrete")
    }, p$scales$scales)
    expect_length(scales, 1L)
    expect_identical(scales[[1L]]$get_limits(), key$category)
    expect_identical(scales[[1L]]$breaks, key$category)
  }

  expect_complete_key(gtropic_plot(x, type = "track", basemap = "none"))
  expect_complete_key(gtropic_plot(
    x, metric = "VMAX_SUST_MS", basemap = "none"
  ))
})

test_that("categorical ADM2 wind fills classify aggregated local exposure", {
  key <- saffir_simpson()
  p <- gtropic_plot(
    plot_fixture(), metric = "VMAX_SUST_MS", wind_categories = key,
    basemap = "none", track = FALSE
  )
  info <- attr(p$data, "plot_info")
  category_scale <- Filter(function(scale) {
    "fill" %in% scale$aesthetics && inherits(scale, "ScaleDiscrete")
  }, p$scales$scales)[[1L]]
  key_labels <- Filter(function(layer) inherits(layer$geom, "GeomText"), p$layers)[[1L]]$data

  expect_identical(as.character(p$data$wind_category), c("TS", "TS", "TD", "TS", NA))
  expect_identical(category_scale$get_limits(), key$category)
  expect_identical(category_scale$breaks, key$category)
  expect_identical(category_scale$na.value, "grey90")
  expect_identical(category_scale$name,
    "Maximum local sustained wind category (Saffir–Simpson-equivalent)"
  )
  expect_identical(key_labels$label, key$category)
  expect_identical(info$wind_categories$VMAX_SUST_MS$kind, "saffir_simpson")
  expect_match(p$labels$caption,
    "Categorical exposure: source metric=VMAX_SUST_MS, aggregate=max, key=Saffir–Simpson-equivalent, unit=m/s",
    fixed = TRUE
  )
})

test_that("Saffir-Simpson keys accept equivalent metre-per-second units", {
  x <- plot_fixture()
  x$codebook$variables$VMAX_SUST_MS <- list(unit = "meters per second")

  p <- gtropic_plot(
    x, metric = "VMAX_SUST_MS", wind_categories = saffir_simpson(),
    basemap = "none", track = FALSE
  )

  info <- attr(p$data, "plot_info")
  expect_identical(info$metrics$unit, "meters per second")
  expect_identical(info$wind_categories$VMAX_SUST_MS$key$unit[1L], "m/s")

  x$codebook$variables$VMAX_SUST_MS$unit <- "km/h"
  expect_error(
    gtropic_plot(
      x, metric = "VMAX_SUST_MS", wind_categories = saffir_simpson(),
      basemap = "none", track = FALSE
    ),
    "incompatible units"
  )
})

test_that("categorical exposure keys validate scope, units, and scientific guardrails", {
  x <- plot_fixture()
  custom_gust <- tibble::tibble(
    category = c("Lower", "Higher"), threshold = c(100, Inf),
    unit = "m/s", colour = c("grey70", "navy")
  )

  expect_error(
    gtropic_plot(x, type = "count", metric = "VMAX_SUST_MS",
      wind_categories = saffir_simpson()),
    "only to type = 'exposure'"
  )
  expect_error(
    gtropic_plot(x, metric = "VMAX_SUST_MS", binary = TRUE,
      wind_categories = saffir_simpson()),
    "binary = FALSE"
  )
  expect_error(
    gtropic_plot(x, metric = "VMAX_SUST_MS", aggregate = "mean",
      wind_categories = saffir_simpson()),
    "explicitly custom"
  )
  expect_error(
    gtropic_plot(x, metric = "VMAX_GUST_MS", wind_categories = saffir_simpson()),
    "explicitly custom"
  )
  expect_error(
    gtropic_plot(x, metric = "VMAX_GUST_MS",
      wind_categories = transform(custom_gust, unit = "mm")),
    "incompatible units"
  )
  expect_error(
    gtropic_plot(x, metric = "PRECIP_MM", wind_categories = saffir_simpson()),
    "only selected wind metrics"
  )
  expect_error(
    gtropic_plot(x, metric = "PRECIP_MM", wind_categories = tibble::tibble(
      category = c("Lower", "Higher"), threshold = c(100, Inf),
      unit = "mm", colour = c("grey70", "navy")
    )),
    "only selected wind metrics"
  )
  expect_error(
    gtropic_plot(x, metric = c("VMAX_SUST_MS", "VMAX_GUST_MS"),
      wind_categories = saffir_simpson()),
    "named list"
  )
  expect_error(
    gtropic_plot(x, metric = "VMAX_GUST_MS", wind_categories = list(OTHER = custom_gust)),
    "keyed by selected metrics"
  )

  p <- gtropic_plot(x, metric = c("VMAX_SUST_MS", "VMAX_GUST_MS"),
    wind_categories = list(VMAX_GUST_MS = custom_gust),
    basemap = "none", track = FALSE
  )
  fills <- Filter(function(scale) any(grepl("^fill", scale$aesthetics)), p$scales$scales)
  categorical <- Filter(function(scale) inherits(scale, "ScaleDiscrete"), fills)[[1L]]
  expect_identical(categorical$get_limits(), custom_gust$category)
  expect_identical(categorical$name, "Local exposure category (user-defined; m/s)")
  expect_identical(
    attr(p$data, "plot_info")$wind_categories$VMAX_GUST_MS$key$unit[1L], "m/s"
  )
})

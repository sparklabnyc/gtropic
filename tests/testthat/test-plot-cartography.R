testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("cartography chooses and reports geographic and projected CRSs", {
  data <- prepare_plot_data(
    plot_fixture(),
    "exposure",
    "VMAX_SUST_MS",
    track = FALSE
  )
  regional <- plot_cartography(data, basemap = "none", crs = 4326)
  expect_equal(sf::st_crs(regional$crs)$epsg, 4326)
  p <- build_exposure_plot(data, regional)
  expect_match(p$labels$caption, "CRS: EPSG:4326", fixed = TRUE)
  wide <- plot_cartography(data, basemap = "none")
  expect_false(sf::st_is_longlat(wide$crs))
})

test_that("numeric bounds use sf bbox order", {
  data <- prepare_plot_data(
    plot_fixture(),
    "exposure",
    "VMAX_SUST_MS",
    track = FALSE
  )
  expected <- c(xmin = -100, ymin = 25, xmax = -88, ymax = 34)
  bounds <- list(
    unname(expected),
    expected[c("ymax", "xmin", "ymin", "xmax")],
    sf::st_bbox(expected, crs = sf::st_crs(4326))
  )
  for (bbox in bounds) {
    cartography <- plot_cartography(
      data,
      basemap = "none",
      bbox = bbox,
      expand = 0,
      crs = 4326
    )
    expect_equal(cartography$bbox, expected)
  }
})

test_that("metric key padding cannot extend latitude limits past a pole", {
  data <- prepare_plot_data(
    plot_fixture(),
    "exposure",
    "VMAX_SUST_MS",
    track = FALSE
  )
  ymin <- c(-60, -80, -88)
  expected_lower <- c(-76.56, -90, -90)
  for (i in seq_along(ymin)) {
    cartography <- plot_cartography(
      data,
      basemap = "none",
      bbox = c(-100, ymin[i], -40, 40)
    )
    p <- build_exposure_plot(data, cartography)
    expect_equal(p$coordinates$limits$y, c(expected_lower[i], 42))
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("metric strips use readable labels", {
  testthat::skip_if_not_installed("ggnewscale")
  data <- prepare_plot_data(
    plot_fixture(),
    "exposure",
    c("PRECIP_MM", "PROP_FLOODED_AREA"),
    track = FALSE
  )
  p <- build_exposure_plot(data, plot_cartography(data, basemap = "none"))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(
    attr(p$data, "plot_info")$metric_labels[["PRECIP_MM"]],
    "Cumulative precipitation (mm)"
  )
})

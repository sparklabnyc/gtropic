testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("cartography chooses and reports geographic and projected CRSs", {
  data <- prepare_plot_data(plot_fixture(), "exposure", "VMAX_SUST_MS", track = FALSE)
  regional <- plot_cartography(data, basemap = "none", crs = 4326)
  expect_equal(sf::st_crs(regional$crs)$epsg, 4326)
  p <- build_exposure_plot(data, regional)
  expect_match(p$labels$caption, "CRS: EPSG:4326", fixed = TRUE)
  wide <- plot_cartography(data, basemap = "none")
  expect_false(sf::st_is_longlat(wide$crs))
})

test_that("metric strips use readable labels", {
  testthat::skip_if_not_installed("ggnewscale")
  data <- prepare_plot_data(plot_fixture(), "exposure",
    c("PRECIP_MM", "PROP_FLOODED_AREA"), track = FALSE
  )
  p <- build_exposure_plot(data, plot_cartography(data, basemap = "none"))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(attr(p$data, "plot_info")$metric_labels[["PRECIP_MM"]],
    "Cumulative precipitation (mm)"
  )
})

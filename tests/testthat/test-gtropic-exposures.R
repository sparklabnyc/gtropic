test_that("gtropic_exposures lists exposure metadata by hazard", {
  out <- gtropic_exposures()

  expect_named(out, c("wind", "precip", "flood"))
  expect_true(all(vapply(out, inherits, logical(1), what = "tbl_df")))
  expect_true(all(vapply(out, function(x) {
    identical(names(x), c("metric", "label", "unit", "aggregate", "plot_supported"))
  }, logical(1))))
  expect_setequal(out$wind$metric, c(
    "VMAX_SUST_MS", "VMAX_GUST_MS", "SUST_DUR_MINUTES", "GUST_DUR_MINUTES",
    "WIND_RADII_VMAX_SUST_MS", "WIND_RADII_SUST_DUR_MINUTES"
  ))
  expect_identical(out$precip$metric, "PRECIP_MM")
  expect_true("FLOODED_POP_CENTROID" %in% out$flood$metric)
})

test_that("gtropic_exposures excludes non-exposure and non-plot metrics", {
  out <- gtropic_exposures()
  metrics <- unlist(lapply(out, `[[`, "metric"), use.names = FALSE)
  centroid <- out$flood[out$flood$metric == "FLOODED_POP_CENTROID", ]

  expect_false(any(metrics %in% c("STORM_DIST_KM", "USA_WIND", "USA_STATUS")))
  expect_false(centroid$plot_supported)
  expect_identical(centroid$aggregate, "any")
  expect_true(all(unlist(lapply(out, `[[`, "plot_supported")) |
    metrics == "FLOODED_POP_CENTROID"))
})

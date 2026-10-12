testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("interactive exposure maps contain vector layers and informative popups", {
  p <- gtropic_plot(plot_fixture(),
    type = "exposure", metric = "VMAX_SUST_MS", storm = "PLOT_A",
    track = FALSE, interactive = TRUE, basemap = "none"
  )
  expect_s3_class(p, "leaflet")
  methods <- vapply(p$x$calls, `[[`, character(1), "method")
  expect_identical(methods, c(
    "addPolygons", "addLegend", "addLayersControl", "addControl", "addControl"
  ))
  popup <- p$x$calls[[1]]$args[[5]]
  expect_true(all(grepl("ADM2_NAME", popup, fixed = TRUE)))
  expect_true(all(grepl("WHO_ENTITY", popup, fixed = TRUE)))
  expect_true(all(grepl("STORM_DIST_KM", popup, fixed = TRUE)))
  caption <- p$x$calls[[5]]$args[[1]]
  expect_match(caption, "Source: G-TROPIC", fixed = TRUE)
  expect_match(p$jsHooks$render[[1]]$code, "#FFFFFF", fixed = TRUE)
  legend <- Filter(
    function(call) identical(call$method, "addLegend"), p$x$calls
  )[[1L]]$args[[1L]]$labels
  expect_match(legend[1L], "^[0-9]+\\.[0-9]{2}$")
})

test_that("interactive panels have independent selectable groups", {
  p <- gtropic_plot(plot_fixture(),
    type = "count", metric = c("VMAX_SUST_MS", "PRECIP_MM"),
    track = FALSE, interactive = TRUE, basemap = "none"
  )
  methods <- vapply(p$x$calls, `[[`, character(1), "method")
  expect_equal(sum(methods == "addPolygons"), 2)
  expect_equal(sum(methods == "addLegend"), 2)
  expect_equal(sum(methods == "hideGroup"), 1)
  control <- p$x$calls[[which(methods == "addLayersControl")]]
  expect_setequal(control$args[[2]], c(
    "Maximum sustained wind (meters per second)",
    "Precipitation (millimeters per local day)"
  ))
})

test_that("interactive arguments are validated and text is escaped", {
  expect_identical(
    plot_interactive_escape("A&B <district>"),
    "A&amp;B &lt;district&gt;"
  )
  expect_error(
    gtropic_plot(plot_fixture(),
      interactive = TRUE, basemap = "none", lng = 1
    ),
    "reserved interactive"
  )
  expect_error(
    gtropic_plot(plot_fixture(), type = "track", interactive = TRUE),
    "support only"
  )
})

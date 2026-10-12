testthat::skip_if_not_installed("sf")

test_that("map overlays include only storms contributing to plotted panels", {
  x <- plot_fixture()
  excluded <- x$wind$ADM2_ID == "PLOT.1" & x$wind$STORM_ID == "PLOT_C"
  x$wind <- x$wind[!excluded, ]
  prepared <- prepare_plot_data(x,
    type = "exposure", adm2 = "PLOT.1", facet_by = "STORM_ID"
  )
  info <- attr(prepared, "plot_info")
  expect_identical(unique(info$tracks$STORM_ID), "PLOT_A")
})

test_that("track-only ADM2 filtering retains linked storms only", {
  x <- plot_fixture()
  out <- prepare_plot_data(x, "track", adm2 = "PLOT.1")
  expect_setequal(unique(out$STORM_ID), c("PLOT_A", "PLOT_C"))
})

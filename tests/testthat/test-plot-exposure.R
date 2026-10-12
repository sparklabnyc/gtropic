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

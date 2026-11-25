testthat::skip_if_not_installed("sf")

invalid_plot_geometry <- function(geometry) {
  geometry <- sf::st_transform(geometry, 3857)
  bounds <- sf::st_bbox(geometry[1, ])
  bow_tie <- matrix(
    c(
      bounds[["xmin"]],
      bounds[["ymin"]],
      bounds[["xmax"]],
      bounds[["ymax"]],
      bounds[["xmin"]],
      bounds[["ymax"]],
      bounds[["xmax"]],
      bounds[["ymin"]],
      bounds[["xmin"]],
      bounds[["ymin"]]
    ),
    ncol = 2,
    byrow = TRUE
  )
  sf::st_geometry(geometry)[[1L]] <- sf::st_polygon(list(bow_tie))
  geometry
}

test_that("invalid geometry is repaired by default and can be preserved", {
  geometry <- invalid_plot_geometry(plot_fixture()$geometry)
  expect_false(sf::st_is_valid(geometry)[1L])

  repaired <- repair_invalid_geometry(geometry)
  preserved <- repair_invalid_geometry(geometry, make_valid = FALSE)

  expect_true(all(sf::st_is_valid(repaired)))
  expect_false(sf::st_is_valid(preserved)[1L])
})

test_that("plot preparation repairs custom ADM2 geometry unless disabled", {
  data <- plot_fixture()
  data$geometry <- invalid_plot_geometry(data$geometry)

  repaired <- prepare_plot_data(
    data,
    "exposure",
    metric = "VMAX_SUST_MS",
    track = FALSE
  )
  preserved <- prepare_plot_data(
    data,
    "exposure",
    metric = "VMAX_SUST_MS",
    track = FALSE,
    make_valid = FALSE
  )

  expect_true(all(sf::st_is_valid(repaired)))
  expect_true(any(!sf::st_is_valid(preserved)))
})

test_that("make_valid requires one non-missing logical value", {
  for (value in list(NULL, NA, 1, "TRUE", c(TRUE, FALSE))) {
    expect_error(check_flag(value, "make_valid"), "make_valid")
  }
})

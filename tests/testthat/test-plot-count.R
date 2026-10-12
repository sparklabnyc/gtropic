testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("count builders delegate the full choropleth contract", {
  captured <- NULL
  local_mocked_bindings(build_exposure_plot = function(...) {
    captured <<- list(...)
    ggplot2::ggplot()
  })
  data <- tibble::tibble(ADM2_ID = "a", value = 2)
  cartography <- list(basemap = NULL)
  p <- build_count_plot(data, cartography,
    title = "Storm counts", subtitle = "Selected ADM2s", caption = "Definition",
    linewidth = 0.2
  )
  expect_s3_class(p, "ggplot")
  expect_identical(captured, list(
    data = data, cartography = cartography,
    title = "Storm counts", subtitle = "Selected ADM2s", caption = "Definition",
    gridlines = TRUE,
    linewidth = 0.2
  ))
})

test_that("precipitation counts use raw days and distinguish zero from NA", {
  x <- plot_fixture()
  spec <- resolve_plot_metrics(x, "count", "PRECIP_MM")
  rows <- tibble::tibble(
    ADM2_ID = c(rep("wet", 6), rep("dry", 2), "missing"),
    STORM_ID = c("A", "A", "A", "B", "B", "B", "A", "A", "A"),
    metric = "PRECIP_MM",
    date = as.Date("2020-08-01") + c(0, 1, 2, 0, 1, 2, 0, 1, 0),
    value = c(40, 40, 75, 40, 40, 75, 40, 40, NA_real_),
    .plot_row_id = c(1:3, 1:3, 4:6)
  )
  storms <- plot_aggregate_metric(rows, spec, "count", "storms", NULL, FALSE)
  days <- plot_aggregate_metric(rows, spec, "count", "days", NULL, FALSE)
  ids <- c("wet", "dry", "missing")
  expect_equal(storms$value[match(ids, storms$ADM2_ID)], c(2, 0, NA_real_))
  expect_equal(days$value[match(ids, days$ADM2_ID)], c(1, 0, NA_real_))
  faceted <- plot_aggregate_metric(
    rows, spec, "count", "days", "STORM_ID", FALSE
  )
  expect_equal(faceted$value[faceted$ADM2_ID == "wet"], c(1, 1))
})

test_that("wind counts distinguish storms from closest-approach dates", {
  spec <- resolve_plot_metrics(plot_fixture(), "count", "VMAX_SUST_MS")
  rows <- tibble::tibble(
    ADM2_ID = "coast", STORM_ID = c("A", "A", "B", "C"),
    LOCAL_DATE_STORM_CLOSEST = as.Date("2020-08-01") + c(0, 0, 0, 1),
    value = c(17.5, 17.5, 30, 17.4), metric = "VMAX_SUST_MS",
    .plot_row_id = c(1L, 1L, 2L, 3L)
  )
  storms <- plot_aggregate_metric(rows, spec, "count", "storms", NULL, FALSE)
  days <- plot_aggregate_metric(rows, spec, "count", "days", NULL, FALSE)
  expect_equal(storms$value, 2)
  expect_equal(days$value, 1)
})

test_that("prepared counts carry count labels and threshold definitions", {
  x <- plot_fixture()
  for (unit in c("storms", "days")) {
    data <- prepare_plot_data(x, "count", "PRECIP_MM",
      count_unit = unit, track = FALSE
    )
    info <- attr(data, "plot_info")
    expect_equal(info$metrics$legend, paste("Number of", unit))
    expect_equal(info$metrics$value_unit, unit)
    expect_match(info$caption, "exposed PRECIP_MM: >= 75 mm", fixed = TRUE)
    expect_match(info$caption, "precip window: -2/+1 days", fixed = TRUE)
    expect_match(info$caption, "500 km", fixed = TRUE)
    expect_equal(data$value[data$ADM2_ID == "PLOT.1"], 0)
    expect_true(is.na(data$value[data$ADM2_ID == "PLOT.5"]))
    expected <- if (unit == "storms") 3 else 6
    expect_equal(data$value[data$ADM2_ID == "PLOT.3"], expected)
    p <- build_count_plot(data, plot_cartography(data, basemap = "none"),
      title = "Exposure counts", subtitle = unit
    )
    expect_s3_class(p, "ggplot")
    expect_s3_class(p$layers[[1]]$geom, "GeomSf")
    expect_equal(rlang::as_label(p$layers[[1]]$mapping$fill), "value")
    scale <- p$scales$get_scales("fill")
    expect_false(scale$is_discrete())
    expect_equal(scale$name, paste("Number of", unit))
    expect_equal(p$labels$title, "Exposure counts")
    expect_equal(p$labels$subtitle, unit)
    expect_match(
      gsub("\\s+", " ", p$labels$caption),
      info$caption, fixed = TRUE
    )
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("count maps share count limits across storm facets", {
  data <- prepare_plot_data(plot_fixture(), "count", "PRECIP_MM",
    count_unit = "days", facet_by = "STORM_ID", track = FALSE
  )
  p <- build_count_plot(data, plot_cartography(data, basemap = "none"),
    caption = "A custom exposure definition"
  )
  expect_s3_class(p$facet, "FacetWrap")
  expect_named(p$facet$params$facets, "STORM_ID")
  expect_equal(p$scales$get_scales("fill")$limits, c(0, 4))
  expect_equal(p$labels$caption, "A custom exposure definition")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 3)
})

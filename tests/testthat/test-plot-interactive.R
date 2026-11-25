testthat::skip_if_not_installed("leaflet")
testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

test_that("interactive exposure maps contain vector layers and informative popups", {
  p <- gtropic_plot(
    plot_fixture(),
    type = "exposure",
    metric = "VMAX_SUST_MS",
    storm = "PLOT_A",
    track = FALSE,
    interactive = TRUE,
    basemap = "none"
  )
  expect_s3_class(p, "leaflet")
  methods <- vapply(p$x$calls, `[[`, character(1), "method")
  expect_identical(
    methods,
    c(
      "addPolygons",
      "addLegend",
      "addLayersControl",
      "addControl",
      "addControl"
    )
  )
  popup <- p$x$calls[[1]]$args[[5]]
  expect_true(all(grepl("ADM2_NAME", popup, fixed = TRUE)))
  expect_true(all(grepl("WHO_ENTITY", popup, fixed = TRUE)))
  expect_true(all(grepl("STORM_DIST_KM", popup, fixed = TRUE)))
  caption <- p$x$calls[[5]]$args[[1]]
  expect_match(caption, "Source: G-TROPIC", fixed = TRUE)
  expect_match(
    caption,
    "\nWind parameters: radius=200 km, aggregate=max",
    fixed = TRUE
  )
  expect_match(p$jsHooks$render[[1]]$code, "#FFFFFF", fixed = TRUE)
  legend <- Filter(
    function(call) identical(call$method, "addLegend"),
    p$x$calls
  )[[1L]]$args[[1L]]$labels
  expect_match(legend[1L], "^[0-9]+\\.[0-9]{2}$")
})

test_that("interactive panels have independent selectable groups", {
  p <- gtropic_plot(
    plot_fixture(),
    type = "count",
    metric = c("VMAX_SUST_MS", "PRECIP_MM"),
    track = FALSE,
    interactive = TRUE,
    basemap = "none"
  )
  methods <- vapply(p$x$calls, `[[`, character(1), "method")
  expect_equal(sum(methods == "addPolygons"), 2)
  expect_equal(sum(methods == "addLegend"), 2)
  expect_equal(sum(methods == "hideGroup"), 1)
  control <- p$x$calls[[which(methods == "addLayersControl")]]
  expect_setequal(
    control$args[[2]],
    c(
      "Maximum sustained wind (m/s) &gt;= 17.5 m/s",
      "Precipitation (mm) &gt;= 75 mm"
    )
  )
})

test_that("interactive count maps map every observed count categorically", {
  p <- gtropic_plot(
    plot_fixture(),
    type = "count",
    metric = "VMAX_SUST_MS",
    track = FALSE,
    interactive = TRUE,
    basemap = "none"
  )
  polygon <- Filter(
    function(call) identical(call$method, "addPolygons"),
    p$x$calls
  )[[1L]]
  legend <- Filter(
    function(call) identical(call$method, "addLegend"),
    p$x$calls
  )[[1L]]$args[[1L]]

  expect_false(anyNA(polygon$args[[4L]]$fillColor))
  expect_setequal(trimws(as.character(legend$labels)), c("0", "3", "NA"))
})

test_that("interactive arguments are validated and text is escaped", {
  expect_identical(
    plot_interactive_escape("A&B <district>"),
    "A&amp;B &lt;district&gt;"
  )
  expect_error(
    gtropic_plot(plot_fixture(), interactive = TRUE, basemap = "none", lng = 1),
    "reserved interactive"
  )
  expect_error(
    gtropic_plot(plot_fixture(), type = "track", interactive = TRUE),
    "support only"
  )
})

test_that("interactive maps escape HTML in names, category labels and units", {
  payload <- "<img src=x onerror=alert(1)>"
  x <- gtropic_example()
  x$adm2$ADM2_NAME[] <- payload
  key <- gtropic_saffir_simpson()
  key$category[] <- paste(payload, seq_len(nrow(key)))

  p <- gtropic_plot(
    x,
    type = "exposure",
    metric = "VMAX_SUST_MS",
    wind_categories = key,
    track = TRUE,
    interactive = TRUE,
    basemap = "none"
  )
  widget <- as.character(jsonlite::toJSON(
    unclass(p$x),
    auto_unbox = TRUE,
    force = TRUE
  ))

  expect_false(grepl("<img", widget, fixed = TRUE))
  expect_match(widget, "&lt;img src=x onerror=alert(1)&gt;", fixed = TRUE)
  legend <- Filter(
    function(call) identical(call$method, "addLegend"),
    p$x$calls
  )[[1L]]$args[[1L]]$labels
  expect_match(legend[1L], "^&lt;img", perl = TRUE)

  # A track unit cannot carry markup: the key accepts four units only.
  track_key <- gtropic_saffir_simpson()
  track_key$unit[] <- payload
  expect_error(
    gtropic_plot(
      x,
      type = "exposure",
      metric = "VMAX_SUST_MS",
      track = TRUE,
      track_map_key = track_key,
      interactive = TRUE,
      basemap = "none"
    ),
    "track_map_key"
  )
})

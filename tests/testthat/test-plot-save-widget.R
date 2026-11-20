testthat::skip_if_not_installed("htmlwidgets")

test_that("HTML widgets save through htmlwidgets with portable defaults", {
  widget <- structure(list(), class = c("leaflet", "htmlwidget"))
  seen <- NULL
  checked <- character()
  local_mocked_bindings(saveWidget = function(...) {
    seen <<- list(...)
  }, .package = "htmlwidgets")
  local_mocked_bindings(check_installed = function(pkg, ...) {
    checked <<- c(checked, pkg)
  }, .package = "rlang")

  result <- withVisible(gtropic_save(
    widget, "map.html", path = "maps", title = "G-TROPIC map"
  ))

  expect_false(result$visible)
  expect_identical(result$value, file.path("maps", "map.html"))
  expect_identical(seen, list(
    widget = widget, file = file.path("maps", "map.html"),
    title = "G-TROPIC map", selfcontained = FALSE
  ))
  expect_identical(checked, "htmlwidgets")
})

test_that("HTML widget saver validates filenames and widget options", {
  widget <- structure(list(), class = c("leaflet", "htmlwidget"))
  local_mocked_bindings(saveWidget = function(...) {
    stop("unexpected saving")
  }, .package = "htmlwidgets")

  expect_error(gtropic_save(widget, "map.png"), "end in.*html")
  expect_error(gtropic_save(widget, "map.html", device = "png"), "Unsupported HTML widget")
  expect_error(gtropic_save(widget, "map.html", path = ""), "path.*nonempty string")
})

test_that("HTML widget saver permits self-contained output when requested", {
  widget <- structure(list(), class = c("leaflet", "htmlwidget"))
  seen <- NULL
  local_mocked_bindings(saveWidget = function(...) {
    seen <<- list(...)
  }, .package = "htmlwidgets")

  gtropic_save(widget, "map.html", selfcontained = TRUE, libdir = "assets")

  expect_true(seen$selfcontained)
  expect_identical(seen$libdir, "assets")
})

test_that("HTML widgets are written with a sibling dependency directory", {
  widget <- htmlwidgets::createWidget("test", list(), package = "htmlwidgets")
  output <- tempfile(fileext = ".html")
  dependencies <- paste0(tools::file_path_sans_ext(output), "_files")
  on.exit(unlink(c(output, dependencies), recursive = TRUE), add = TRUE)

  gtropic_save(widget, output)

  expect_true(file.exists(output))
  expect_true(dir.exists(dependencies))
})

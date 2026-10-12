testthat::skip_if_not_installed("ggplot2")

test_that("static saving forwards publication defaults and returns a path invisibly", {
  p <- ggplot2::ggplot()
  seen <- NULL
  checked <- character()
  local_mocked_bindings(ggsave = function(...) {
    seen <<- list(...)
    "backend result"
  }, .package = "ggplot2")
  local_mocked_bindings(check_installed = function(pkg, ...) {
    checked <<- c(checked, pkg)
  }, .package = "rlang")
  result <- withVisible(gtropic_save(p, "figure.pdf", path = "figures", bg = "white"))
  expect_false(result$visible)
  expect_identical(result$value, file.path("figures", "figure.pdf"))
  expect_identical(seen, list(
    filename = "figure.pdf", plot = p, dpi = 300,
    width = 7, height = 5, units = "in", path = "figures", bg = "white"
  ))
  expect_identical(checked, "ggplot2")
})

test_that("map sizing corrects longitude spans for latitude and named bbox order", {
  p <- ggplot2::ggplot()
  seen <- NULL
  local_mocked_bindings(ggsave = function(...) {
    seen <<- list(...)
  }, .package = "ggplot2")
  for (bbox in list(
    c(0, 20, 50, 70),
    c(ymax = 70, xmin = 0, ymin = 50, xmax = 20),
    c(xmin = 170, xmax = 190, ymin = -70, ymax = -50)
  )) {
    attr(p, "gtropic_bbox") <- bbox
    gtropic_save(p, "map.png")
    expect_equal(seen$width, 7)
    # At +/-60 degrees the cosine halves the horizontal span: aspect = 0.5.
    expect_equal(seen$height, 10)
    gtropic_save(p, "map.png", height = 8)
    expect_equal(seen$width, 4)
    expect_equal(seen$height, 8)
  }
  attr(p, "gtropic_bbox") <- c(0, 20, -5, 5)
  gtropic_save(p, "map.png")
  expect_equal(seen$height, 3.5)
  gtropic_save(p, "map.png", width = 8)
  expect_equal(seen$height, 4)
})

test_that("extreme and unusable bounds produce finite bounded default dimensions", {
  p <- ggplot2::ggplot()
  seen <- NULL
  local_mocked_bindings(ggsave = function(...) {
    seen <<- list(...)
  }, .package = "ggplot2")
  for (bbox in list(c(0, 360, -0.01, 0.01), c(0, 0.01, 89, 90))) {
    attr(p, "gtropic_bbox") <- bbox
    gtropic_save(p, "map.png")
    expect_true(seen$height %in% c(3, 10))
    expect_equal(seen$width, 7)
  }
  for (bbox in list(
    NULL, "bad", numeric(), c(0, 1, 2), c(0, Inf, 0, 1),
    c(0, 1, NA, 1), c(0, 0, 0, 1), c(1, 0, 0, 1),
    c(0, 1, 0, 0), c(0, 1, 90, 91), c(0, 361, 0, 1),
    c(a = 0, b = 1, c = 0, d = 1)
  )) {
    attr(p, "gtropic_bbox") <- bbox
    gtropic_save(p, "map.png")
    expect_equal(c(seen$width, seen$height), c(7, 5))
  }
})

test_that("explicit dimensions, dpi and saver options take precedence", {
  p <- ggplot2::ggplot()
  attr(p, "gtropic_bbox") <- c(0, 360, 0, 1)
  seen <- NULL
  local_mocked_bindings(ggsave = function(...) {
    seen <<- list(...)
  }, .package = "ggplot2")
  for (units in c("in", "cm", "mm", "px")) {
    gtropic_save(p, "figure.tiff",
      width = 12, height = 2, dpi = 600,
      units = units, device = "tiff", compression = "lzw", scale = 0.5,
      limitsize = FALSE
    )
    expect_equal(c(seen$width, seen$height, seen$dpi), c(12, 2, 600))
    expect_identical(seen$units, units)
    expect_identical(seen$device, "tiff")
    expect_identical(seen$compression, "lzw")
    expect_equal(seen$scale, 0.5)
    expect_false(seen$limitsize)
  }
})

test_that("inferred dimensions are consistent across units and pixel resolution", {
  p <- ggplot2::ggplot()
  seen <- NULL
  local_mocked_bindings(ggsave = function(...) {
    seen <<- list(...)
  }, .package = "ggplot2")
  factors <- c("in" = 1, cm = 2.54, mm = 25.4, px = 120)
  for (units in names(factors)) {
    factor <- factors[[units]]
    gtropic_save(p, "figure.png", units = units, dpi = 120)
    expect_equal(c(seen$width, seen$height), c(7, 5) * factor)
    gtropic_save(p, "figure.png", units = units, dpi = 120, width = 8 * factor)
    expect_equal(c(seen$width, seen$height), c(8, 8 * 5 / 7) * factor)
    gtropic_save(p, "figure.png", units = units, dpi = 120, height = 6 * factor)
    expect_equal(c(seen$width, seen$height), c(6 * 7 / 5, 6) * factor)
  }
})

test_that("invalid save inputs fail before a backend is called", {
  p <- ggplot2::ggplot()
  local_mocked_bindings(
    ggsave = function(...) stop("unexpected saving"),
    .package = "ggplot2"
  )
  for (bad in list(NULL, list(), "plot", structure(list(), class = "leaflet"))) {
    expect_error(gtropic_save(bad, "figure.png"), "plot.*ggplot")
  }
  for (bad in list(NULL, NA_character_, "", "  ", 1, c("a", "b"))) {
    expect_error(gtropic_save(p, bad), "filename.*nonempty string")
  }
  for (bad in list(NA_character_, "", 1, c("a", "b"))) {
    expect_error(gtropic_save(p, "figure.png", path = bad), "path.*nonempty string")
  }
  for (argument in c("dpi", "width", "height")) {
    for (bad in list(0, -1, Inf, NA_real_, NaN, "300", TRUE, numeric(), c(1, 2))) {
      args <- c(
        list(plot = p, filename = "figure.png"),
        stats::setNames(list(bad), argument)
      )
      expect_error(do.call(gtropic_save, args), paste0(argument, ".*positive finite"))
    }
  }
  expect_error(gtropic_save(p, "figure.png", dpi = NULL), "dpi")
  expect_error(gtropic_save(p, "figure.png", units = "points"), "units")
  expect_error(gtropic_save(p, "figure.png", 1), "must be named")
  expect_error(gtropic_save(p, "figure.png", bg = "a", bg = "b"), "uniquely named")
})

test_that("dependency errors are gated and backend failures propagate", {
  p <- ggplot2::ggplot()
  missing <- "ggplot2"
  local_mocked_bindings(check_installed = function(pkg, ...) {
    if (missing %in% pkg) stop(paste("Missing dependency", missing))
  }, .package = "rlang")
  expect_error(gtropic_save(p, "figure.png"), "Missing dependency ggplot2")
  missing <- "none"
  local_mocked_bindings(
    ggsave = function(...) stop("static save failed"),
    .package = "ggplot2"
  )
  expect_error(gtropic_save(p, "figure.png"), "static save failed")
})

test_that("caching is enabled by default without creating a cache at load", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = NULL,
    gtropic.cache_dir = cache_dir
  ))
  local_mocked_bindings(cache_config_read = function() NULL)

  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_true(getOption("gtropic.cache_enabled"))
  expect_false(dir.exists(cache_dir))
})

test_that("cache = FALSE bypasses cache reads and writes", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = TRUE,
    gtropic.cache_dir = cache_dir
  ))

  resolved <- cache_resolve(FALSE)

  expect_identical(resolved, list(enabled = FALSE, dir = NULL))
  expect_false(dir.exists(cache_dir))
})

test_that("resolving the default cache does not create it", {
  cache_dir <- file.path(withr::local_tempdir(), "cache")
  withr::local_options(list(
    gtropic.cache_enabled = TRUE,
    gtropic.cache_dir = cache_dir
  ))

  resolved <- cache_resolve()

  expect_identical(resolved, list(enabled = TRUE, dir = cache_dir))
  expect_false(dir.exists(cache_dir))
})

test_that("cache enable and disable persist only when requested", {
  root <- withr::local_tempdir()
  cache_dir <- file.path(root, "cache")
  config_dir <- file.path(root, "config")
  withr::local_options(list(
    gtropic.cache_enabled = FALSE,
    gtropic.cache_dir = cache_dir
  ))
  local_mocked_bindings(cache_config_dir = function() config_dir)

  expect_silent(gtropic_cache_enable())
  expect_true(getOption("gtropic.cache_enabled"))
  expect_false(dir.exists(config_dir))

  expect_silent(gtropic_cache_disable(persist = TRUE))
  expect_false(getOption("gtropic.cache_enabled"))
  expect_true(file.exists(file.path(config_dir, "cache.rds")))
  expect_identical(cache_config_read(), FALSE)

  withr::local_options(list(gtropic.cache_enabled = NULL))
  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_false(getOption("gtropic.cache_enabled"))

  withr::local_options(list(gtropic.cache_enabled = TRUE))
  expect_silent(gtropic:::.onLoad("", "gtropic"))
  expect_true(getOption("gtropic.cache_enabled"))

  expect_silent(gtropic_cache_enable(persist = TRUE))
  expect_identical(cache_config_read(), TRUE)
})

test_that("cache settings validate their inputs", {
  expect_error(gtropic_cache_enable(persist = NA), "persist")
  expect_error(gtropic_cache_disable(persist = "yes"), "persist")
  expect_error(gtropic_cache_enable(dir = ""), "dir")
})

test_that("gtropic_codebook forwards its cache override", {
  seen_cache <- NULL
  local_mocked_bindings(
    helper_context = function(version, cache) {
      seen_cache <<- cache
      list()
    },
    fetch_file = function(...) list()
  )

  gtropic_codebook(cache = FALSE)

  expect_false(seen_cache)
})

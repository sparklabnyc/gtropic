# The options registry (Plan 03 UX-03): one source of truth for defaults,
# validation on read, and the table in ?gtropic_options.

test_that(".onLoad() sets every registry option that is unset", {
  specs <- gtropic_default_options()
  full <- paste0("gtropic.", names(specs))
  old <- options(stats::setNames(vector("list", length(full)), full))
  withr::defer(options(old))
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())

  .onLoad(NULL, "gtropic")

  for (nm in names(specs)) {
    expect_identical(
      getOption(paste0("gtropic.", nm)),
      specs[[nm]]$default,
      label = nm
    )
  }
})

test_that(".onLoad() keeps options the user has already set", {
  withr::local_options(gtropic.timeout = 30)
  .onLoad(NULL, "gtropic")
  expect_identical(getOption("gtropic.timeout"), 30)
})

test_that("unset options fall back to the registry default", {
  withr::local_options(gtropic.timeout = NULL, gtropic.max_tries = NULL)
  expect_identical(opt_timeout(), opt_default("timeout"))
  expect_identical(opt_max_tries(), opt_default("max_tries"))
})

test_that("option values are checked when they are read", {
  bad <- list(
    timeout = "fast",
    timeout = -1,
    timeout = c(1, 2),
    max_tries = 0,
    max_tries = 2.5,
    max_tries = Inf,
    confirm_threshold_gb = 0,
    confirm_threshold_gb = NA_real_,
    cache_enabled = "yes",
    cache_enabled = NA,
    doi_historical = 1,
    collection = NA_character_,
    cache_dir = character(0)
  )
  for (i in seq_along(bad)) {
    nm <- names(bad)[i]
    withr::with_options(
      stats::setNames(list(bad[[i]]), paste0("gtropic.", nm)),
      expect_error(opt(nm), paste0("gtropic.", nm), fixed = TRUE)
    )
  }
})

test_that("valid option values are returned with their natural type", {
  withr::local_options(
    gtropic.max_tries = 2,
    gtropic.timeout = 30L,
    gtropic.confirm_threshold_gb = Inf
  )
  expect_identical(opt_max_tries(), 2L)
  expect_identical(opt_timeout(), 30)
  expect_identical(opt_confirm_threshold_gb(), Inf)
})

test_that("the documentation table lists every option", {
  table <- options_doc_table()
  for (nm in names(gtropic_default_options())) {
    expect_match(table, paste0("`gtropic.", nm, "`"), fixed = TRUE)
  }
})

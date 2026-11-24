# Messaging (Plan 03 UX-12).

test_that("each kind of availability warning is shown once per session", {
  withr::local_options(rlib_warning_verbosity = "default")
  msgs <- list(
    ux12_test_a = "First availability note.",
    ux12_test_b = "Second availability note."
  )
  expect_warning(
    expect_warning(warn_availability(msgs), "First"),
    "Second"
  )
  expect_no_warning(warn_availability(msgs))
})

test_that("cache maintenance messages honour options(gtropic.quiet)", {
  withr::local_options(gtropic.cache_dir = withr::local_tempdir())
  expect_message(gtropic_cache_clear(confirm = FALSE), "already empty")
  withr::local_options(gtropic.quiet = TRUE)
  expect_no_message(gtropic_cache_clear(confirm = FALSE))
  expect_no_message(gtropic_cache_prune(max_size_gb = 1))
})

test_that("the guardrail and cache clearing share one prompt", {
  answers <- character(0)
  local_mocked_bindings(
    is_interactive = function() TRUE,
    read_answer = function(prompt) {
      answers <<- c(answers, prompt)
      "n"
    }
  )
  withr::local_options(gtropic.confirm_threshold_gb = 0.001)
  expect_error(
    suppressMessages(
      apply_guardrail(list(bytes = 1024^3, n_files = 2L, n_cached = 0L))
    ),
    class = "gtropic_error_download_size"
  )

  dir <- withr::local_tempdir()
  withr::local_options(gtropic.cache_dir = dir)
  dir.create(file.path(dir, "blobs"))
  writeLines("x", file.path(dir, "blobs", paste0(strrep("a", 32L), ".blob")))
  expect_message(gtropic_cache_clear(), "About to delete 1 cached file")
  expect_true(file.exists(file.path(dir, "blobs", paste0(strrep("a", 32L), ".blob"))))

  expect_identical(answers, rep("Proceed? [y/N] ", 2L))
})

test_that("confirm_action() accepts y and yes, in any case", {
  for (answer in c("y", "Yes", " YES ")) {
    local_mocked_bindings(read_answer = function(prompt) answer)
    expect_true(suppressMessages(confirm_action("Delete?")))
  }
  local_mocked_bindings(read_answer = function(prompt) "")
  expect_false(suppressMessages(confirm_action("Delete?")))
})

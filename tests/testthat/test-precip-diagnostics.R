test_that("precipitation diagnostics use the values actually returned", {
  precip <- tibble::tibble(
    ADM2_ID = c("A", "A", "B", "B"),
    PRECIP_MM = c(NA_real_, NA_real_, NA_real_, 1)
  )

  expect_identical(
    precip_all_missing_adm2_ids(precip, c("A", "B", "C")),
    c("A", "C")
  )
  expect_identical(
    precip_all_missing_adm2_ids(precip, c("B")),
    character(0)
  )
  expect_identical(
    precip_all_missing_adm2_ids(NULL, c("A", "B")),
    c("A", "B")
  )
})

test_that("precipitation warnings explain the limitation and next steps", {
  expect_warning(
    warn_precip_all_missing(c("A", "B")),
    regexp = "known current source-data limitation",
    class = "gtropic_warning_precip_all_missing"
  )
})

test_that("precipitation plot captions identify affected mapped units", {
  testthat::skip_if_not_installed("sf")
  x <- plot_fixture()

  prepared <- prepare_plot_data(
    x, "exposure", "PRECIP_MM", track = FALSE
  )
  info <- attr(prepared, "plot_info")
  expect_identical(info$precip_all_missing_adm2_ids, "PLOT.5")
  expect_match(
    info$caption,
    "No non-missing precipitation data for ADM2_ID PLOT.5",
    fixed = TRUE
  )
  expect_match(info$caption, "?gtropic_known_limitations", fixed = TRUE)

  x$precip$PRECIP_MM[x$precip$ADM2_ID == "PLOT.5"] <- 1
  corrected <- prepare_plot_data(
    x, "exposure", "PRECIP_MM", track = FALSE
  )
  expect_length(
    attr(corrected, "plot_info")$precip_all_missing_adm2_ids,
    0L
  )
  expect_false(grepl("known small/coastal", attr(corrected, "plot_info")$caption,
    fixed = TRUE
  ))
})

test_that("large caption diagnostics remain concise", {
  ids <- paste0("ADM2.", 1:20)
  caption <- plot_precip_limitation_caption(ids, has_diagnostics = TRUE)

  expect_match(caption, "20 mapped ADM2 units", fixed = TRUE)
  expect_match(caption, "gtropic_warnings()", fixed = TRUE)
  expect_false(any(vapply(ids, grepl, logical(1), x = caption, fixed = TRUE)))
})

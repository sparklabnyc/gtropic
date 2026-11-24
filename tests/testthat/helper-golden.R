# Offline stand-in for the G-TROPIC collection, used by the golden-output test
# (test-golden-assembly.R).
#
# The synthetic Parquet fixtures from helper-fixtures.R are split into two
# datasets the way the real collection is: a historical dataset holding
# 2001-2002 plus metadata/, and a current-year dataset holding 2003. The four
# transport functions are mocked, so discovery, planning, download, checksum
# verification, reading and assembly all run exactly as they do against
# Dataverse.

golden_doi <- c(historical = "doi:10.0000/GOLDEN-HIST", current = "doi:10.0000/GOLDEN-CURR")
golden_version <- c(historical = "3.0", current = "1.0")

#' Build per-dataset listings for a fixture directory
#' @noRd
golden_listings <- function(dir) {
  rel <- sort(list.files(dir, recursive = TRUE))
  year <- suppressWarnings(as.integer(sub("\\.parquet$", "", basename(rel))))
  dataset <- ifelse(!is.na(year) & year >= 2003L, "current", "historical")
  path <- file.path(dir, rel)
  listing <- tibble::tibble(
    file_id = seq_along(rel),
    label = basename(rel),
    directory_label = dirname(rel),
    checksum = unname(vapply(path, digest::digest,
      character(1),
      algo = "md5", file = TRUE
    )),
    checksum_type = "MD5",
    size_bytes = as.integer(file.size(path)),
    content_type = ifelse(grepl("\\.json$", rel), "application/json",
      "application/vnd.apache.parquet"
    )
  )
  list(
    historical = listing[dataset == "historical", , drop = FALSE],
    current = listing[dataset == "current", , drop = FALSE],
    path = stats::setNames(path, listing$file_id)
  )
}

#' Serve the fixture collection through mocked transport functions
#'
#' @return An environment whose `downloads` element records every file id
#'   requested from the mocked server, and whose `listings` element holds the
#'   served listings and fixture paths (see `golden_listings()`).
#' @noRd
local_golden_collection <- function(env = parent.frame()) {
  dir <- local_parquet_fixtures(env)
  listings <- golden_listings(dir)
  log <- new.env(parent = emptyenv())
  log$downloads <- integer(0)
  log$listings <- listings
  role_of <- function(doi) names(golden_doi)[match(doi, golden_doi)]

  local_dv_transport(
    dv_list_datasets = function(...) {
      tibble::tibble(doi = unname(golden_doi))
    },
    dv_list_files = function(doi, ...) listings[[role_of(doi)]],
    dv_version_label = function(doi, ...) unname(golden_version[role_of(doi)]),
    dv_get_file = function(file_id, ..., dest) {
      log$downloads <- c(log$downloads, as.integer(file_id))
      file.copy(listings$path[[as.character(file_id)]], dest)
      invisible(dest)
    },
    .env = env
  )
  withr::local_options(
    gtropic.server = "golden.invalid",
    gtropic.collection = "golden",
    .local_envir = env
  )
  log
}

#' The tables of a result, without attributes that record when it was made
#' @noRd
golden_tables <- function(x) {
  out <- unclass(x)
  attributes(out) <- list(names = names(out))
  out
}

#' Compare a result's tables with the committed golden file
#'
#' Set `GTROPIC_UPDATE_GOLDEN=true` to (re)write the golden files. That is only
#' legitimate for a deliberate change to returned data (Plan 06 G6), which needs
#' a `NEWS.md` entry.
#' @noRd
expect_golden <- function(x, name) {
  path <- testthat::test_path("golden", paste0(name, ".rds"))
  actual <- golden_tables(x)
  if (identical(Sys.getenv("GTROPIC_UPDATE_GOLDEN"), "true")) {
    dir.create(dirname(path), showWarnings = FALSE)
    saveRDS(actual, path, version = 3)
    testthat::skip(paste("Golden file written:", name))
  }
  expected <- readRDS(path)
  testthat::expect_identical(names(actual), names(expected))
  for (nm in names(expected)) {
    testthat::expect_identical(actual[[nm]], expected[[nm]], label = nm)
  }
}

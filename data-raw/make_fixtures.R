# Write the tiny synthetic Parquet fixtures to tests/testthat/fixtures/ for
# manual inspection.
#
# The tests do not read these files: they generate the same tables on demand
# in a temporary directory (`local_parquet_fixtures()`), so the test suite
# never depends on untracked local files. The table definitions live in one
# place, `tests/testthat/helper-fixtures.R`; edit them there.
#
# Run from the package root with: source("data-raw/make_fixtures.R")
# The output directory is git-ignored; do not commit it.

source(file.path("tests", "testthat", "helper-fixtures.R"))
fixture_dir <- file.path("tests", "testthat", "fixtures")
write_parquet_fixtures(fixture_dir)
message("Fixtures written to ", fixture_dir)

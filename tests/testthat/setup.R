# Never touch the real user directories, whatever a test does.
sandbox <- withr::local_tempdir(.local_envir = testthat::teardown_env())
withr::local_envvar(
  R_USER_CACHE_DIR = file.path(sandbox, "cache"),
  R_USER_CONFIG_DIR = file.path(sandbox, "config"),
  R_USER_DATA_DIR = file.path(sandbox, "data"),
  OMP_THREAD_LIMIT = "2",
  .local_envir = testthat::teardown_env()
)
withr::local_options(
  gtropic.cache_dir = file.path(sandbox, "cache", "R", "gtropic"),
  gtropic.cache_enabled = TRUE,
  gtropic.max_tries = 1L,
  # Every test discovers afresh against its own mocks, as if each were a new
  # session; tests of the reuse window set it themselves.
  gtropic.discovery_ttl = 0,
  arrow.use_threads = FALSE,
  .local_envir = testthat::teardown_env()
)
# At most two threads (CRAN policy; Plan 06 G7).
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::set_cpu_count(1L)
  arrow::set_io_thread_count(2L)
}
# Send any plot a test draws to a null PDF device so no test writes
# Rplots.pdf into the working directory.
withr::local_pdf(NULL, .local_envir = testthat::teardown_env())

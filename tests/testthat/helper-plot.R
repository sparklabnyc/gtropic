#' Read an independent copy of the synthetic, offline plotting fixture
#' @keywords internal
#' @noRd
plot_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "plotting.rds"))
}

#' The synthetic, offline plotting fixture
#'
#' The same object users get from gtropic_example(), built by
#' data-raw/make_plot_fixtures.R, so examples and tests share one source.
#' @keywords internal
#' @noRd
plot_fixture <- function() {
  gtropic_example()
}

# ---------------------------------------------------------------------------
# The download guardrail.
#
# gtropic_data() with no arguments means roughly 47 years of global daily
# precipitation. That is the case this exists to catch. Any query including
# precip or flood also pulls zero_pairs (~30 MB/year) whether or not the user
# asked for it, so a full-history precip query is legitimately enormous and
# SHOULD trip the prompt.
#
# The non-interactive branch errors rather than prompting. Hanging a batch job
# on a readline() that no one will ever answer is worse than failing fast.
# ---------------------------------------------------------------------------

#' Estimate the bytes a file plan will transfer
#'
#' Anything already cached is subtracted, so the estimate reflects what will
#' actually cross the network rather than the notional size of the query.
#'
#' @param plan File plan from `plan_files()`, joined to listing sizes.
#' @param cache Resolved cache list from `cache_resolve()`.
#' @param discovery Output of `discover_datasets()`.
#'
#' @return A list with `bytes`, `n_files` and `n_cached`.
#' @keywords internal
#' @noRd
estimate_download <- function(plan, cache, discovery) {
  if (nrow(plan) == 0L) {
    return(list(bytes = 0, n_files = 0L, n_cached = 0L))
  }

  cached <- vapply(seq_len(nrow(plan)), function(i) {
    slot <- discovery[[plan$dataset[i]]]
    if (is.null(slot)) {
      return(FALSE)
    }
    cache_has(cache, list(
      server = discovery$server, doi = slot$doi, version = slot$version,
      directory_label = plan$directory_label[i], label = plan$label[i]
    ))
  }, logical(1))

  list(
    bytes = sum(plan$size_bytes[!cached], na.rm = TRUE),
    n_files = sum(!cached),
    n_cached = sum(cached)
  )
}

#' Apply the download confirmation guardrail
#'
#' @param estimate Output of `estimate_download()`.
#' @param confirm `NULL` applies the guardrail; `FALSE` skips it.
#' @param quiet Suppress informational output.
#' @param call Calling environment for error reporting.
#'
#' @return Invisibly `TRUE` when the download may proceed.
#' @keywords internal
#' @noRd
apply_guardrail <- function(estimate, confirm = NULL, quiet = FALSE,
                            call = rlang::caller_env()) {
  if (isFALSE(confirm)) {
    return(invisible(TRUE))
  }

  threshold_gb <- opt_confirm_threshold_gb()
  gb <- estimate$bytes / 1024^3

  if (gb <= threshold_gb) {
    inform_unless_quiet(
      "Retrieving {estimate$n_files} file{?s}
       ({format_bytes(estimate$bytes)}){cli::qty(estimate$n_cached)}{?; / ;
       }{if (estimate$n_cached > 0) glue::glue('{estimate$n_cached} already cached')}.",
      quiet = quiet
    )
    return(invisible(TRUE))
  }

  if (!is_interactive()) {
    cli::cli_abort(
      c("This query would download about {format_bytes(estimate$bytes)} across
         {estimate$n_files} file{?s}, above the
         {threshold_gb} GB confirmation threshold.",
        i = "This session is not interactive, so there is no way to ask you to
             confirm.",
        i = "Pass {.code confirm = FALSE} to proceed anyway, or raise the
             threshold with
             {.code options(gtropic.confirm_threshold_gb = {ceiling(gb)})}.",
        i = "Narrowing {.arg date_range} or dropping {.val precip} from
             {.arg tables} is usually the better fix."
      ),
      call = call
    )
  }

  cli::cli_inform(c(
    "This query will download about {format_bytes(estimate$bytes)} across
     {estimate$n_files} file{?s}.",
    i = if (estimate$n_cached > 0) {
      "{estimate$n_cached} further file{?s} {?is/are} already cached."
    }
  ))
  ans <- readline("Proceed? [y/N] ")

  if (!tolower(trimws(ans)) %in% c("y", "yes")) {
    cli::cli_abort(
      c("Download cancelled.",
        i = "Narrow the query, or pass {.code confirm = FALSE} to skip this
             check."
      ),
      call = call
    )
  }

  invisible(TRUE)
}

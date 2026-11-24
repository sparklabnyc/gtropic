#' Conditions signalled by gtropic
#'
#' @description
#' Errors and warnings that a script may want to handle carry classes, so they
#' can be caught with [tryCatch()] or [withCallingHandlers()] by class rather
#' than by matching message text.
#'
#' ## Errors
#'
#' Each inherits from `gtropic_error` (and then `rlang_error`):
#'
#' * `gtropic_error_network`: a request to the Dataverse server failed, for
#'   example because there is no network connection, the DOI or version does
#'   not exist, or the server is down.
#' * `gtropic_error_checksum`: a downloaded file does not match the checksum
#'   published for it.
#' * `gtropic_error_no_match`: a filter value matches nothing in the published
#'   metadata, or the filters together select no storm or ADM2 unit.
#' * `gtropic_error_download_size`: a query is larger than
#'   `getOption("gtropic.confirm_threshold_gb")` and was not confirmed.
#' * `gtropic_error_manifest`: a manifest failed validation before replay.
#'
#' Other errors, such as an invalid argument value, are ordinary `rlang`
#' errors.
#'
#' ## Warnings
#'
#' Each inherits from `gtropic_warning`:
#'
#' * `gtropic_warning_availability`: the requested period reaches recent data
#'   that may still lag or change.
#' * `gtropic_warning_polygon`: ADM2 units moved in or out of a `polygon`
#'   filter across the years evaluated.
#' * `gtropic_warning_precip_all_missing`: some ADM2 units have no
#'   non-missing precipitation values (see [gtropic_known_limitations]).
#' * `gtropic_warning_ambiguous_storm_name` and
#'   `gtropic_warning_ambiguous_excluded_storm_name`: a `STORM_NAME` matches
#'   more than one `STORM_ID`.
#'
#' @return This topic documents condition classes; it has no return value.
#'
#' @examples
#' # A manifest from elsewhere that fails validation:
#' bad <- gtropic_manifest(gtropic_example())
#' bad$files$label[1] <- "../outside.parquet"
#' tryCatch(
#'   gtropic_data(manifest = bad),
#'   gtropic_error_manifest = function(e) "rejected the manifest"
#' )
#'
#' @seealso [gtropic_data()], [gtropic_diagnostics()]
#' @name gtropic_conditions
NULL

#' Signal a classed gtropic error
#'
#' A thin wrapper around [cli::cli_abort()] that adds the `gtropic_error`
#' parent class. The message template is passed through `...` so the cli
#' safety guard checks it at each call site.
#'
#' @param ... The message template, then any further arguments for
#'   [cli::cli_abort()] (for example `parent`).
#' @param class Subclass, such as `"gtropic_error_network"`.
#' @param call Calling environment reported in the error.
#' @param .envir Environment in which the template is evaluated.
#' @keywords internal
#' @noRd
gtropic_abort <- function(..., class, call = rlang::caller_env(),
                          .envir = parent.frame()) {
  cli::cli_abort(...,
    class = c(class, "gtropic_error"), call = call,
    .envir = .envir
  )
}

#' Signal a classed gtropic warning
#'
#' As `gtropic_abort()`, for warnings; adds the `gtropic_warning` parent class.
#'
#' @inheritParams gtropic_abort
#' @param class Subclass, such as `"gtropic_warning_polygon"`.
#' @keywords internal
#' @noRd
gtropic_warn <- function(..., class, .envir = parent.frame()) {
  cli::cli_warn(..., class = c(class, "gtropic_warning"), .envir = .envir)
}

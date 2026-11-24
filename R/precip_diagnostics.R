#' Known G-TROPIC data limitations
#'
#' @description
#' Documents temporary limitations in the published G-TROPIC source data that
#' can affect retrievals and maps.
#'
#' @details
#' ## Precipitation for small or coastal ADM2 units
#'
#' Some small or coastal ADM2 units in the current precipitation data have no
#' non-missing values. This can occur when no ERA5-Land precipitation grid cell
#' is assigned to the unit. A future G-TROPIC dataset release is planned to fill
#' these gaps using finer-resolution ERA5 precipitation data.
#'
#' [gtropic_data()] warns when a precipitation query includes an ADM2 unit with
#' no non-missing precipitation values in the returned period. The complete
#' vector of identifiers is retained in
#' `gtropic_diagnostics(x)$precip_all_missing_adm2_ids`. Automatic precipitation
#' captions from [gtropic_plot()] include a shorter note for affected mapped
#' units.
#'
#' The diagnostic is based on the values actually returned, rather than a fixed
#' list of identifiers. It will therefore stop appearing when corrected source
#' data provide precipitation for those units. Other temporary source-data gaps
#' can also produce the same diagnostic.
#'
#' @name gtropic_known_limitations
#' @seealso [gtropic_data()], [gtropic_plot()], [gtropic_diagnostics()]
NULL

#' Find ADM2 units with no non-missing precipitation values
#'
#' @param precip A precipitation-like data frame, or `NULL`.
#' @param adm2_ids Candidate ADM2 identifiers. When `NULL`, identifiers present
#'   in `precip` are used.
#' @param value_col Name of the precipitation-value column.
#'
#' @return A sorted character vector.
#' @keywords internal
#' @noRd
precip_all_missing_adm2_ids <- function(precip,
                                        adm2_ids = NULL,
                                        value_col = "PRECIP_MM") {
  if (is.null(adm2_ids)) {
    adm2_ids <- if (!is.null(precip) && "ADM2_ID" %in% names(precip)) {
      precip$ADM2_ID
    } else {
      character(0)
    }
  }
  adm2_ids <- sort(unique(as.character(adm2_ids)))
  adm2_ids <- adm2_ids[!is.na(adm2_ids) & nzchar(adm2_ids)]
  if (!length(adm2_ids)) return(character(0))

  if (is.null(precip) ||
    !all(c("ADM2_ID", value_col) %in% names(precip)) ||
    !nrow(precip)) {
    return(adm2_ids)
  }

  available <- !is.na(precip[[value_col]])
  available_ids <- unique(as.character(precip$ADM2_ID[available]))
  setdiff(adm2_ids, available_ids)
}

#' Warn about ADM2 units without precipitation values
#'
#' @param adm2_ids A character vector of affected ADM2 identifiers.
#' @param call Calling environment for warning reporting.
#'
#' @return `adm2_ids`, invisibly.
#' @keywords internal
#' @noRd
warn_precip_all_missing <- function(adm2_ids, call = rlang::caller_env()) {
  if (!length(adm2_ids)) return(invisible(adm2_ids))

  cli::cli_warn(
    c(
      "No non-missing precipitation values were returned for
       {length(adm2_ids)} selected ADM2 unit{?s}.",
      "!" = "{.val {truncate_display(adm2_ids)}}",
      "i" = "A known current source-data limitation can affect small or
             coastal ADM2 units when no ERA5-Land grid cell is assigned.",
      "i" = "A future G-TROPIC dataset release is planned to fill these gaps
             using finer-resolution ERA5 precipitation data.",
      "i" = "See {.code ?gtropic_known_limitations}; complete identifiers are
             available from {.code gtropic_diagnostics()}."
    ),
    class = "gtropic_warning_precip_all_missing",
    call = call
  )
  invisible(adm2_ids)
}

#' Format the compact precipitation limitation used in plot captions
#'
#' @param adm2_ids A character vector of affected mapped ADM2 identifiers.
#' @param has_diagnostics Whether the source object retains full diagnostics.
#'
#' @return A caption line, or `NULL`.
#' @keywords internal
#' @noRd
plot_precip_limitation_caption <- function(adm2_ids,
                                           has_diagnostics = FALSE) {
  if (!length(adm2_ids)) return(NULL)

  affected <- if (length(adm2_ids) <= 3L) {
    paste0(
      "ADM2_ID", if (length(adm2_ids) == 1L) " " else "s ",
      paste(adm2_ids, collapse = ", ")
    )
  } else {
    paste0(length(adm2_ids), " mapped ADM2 units")
  }
  details <- if (length(adm2_ids) > 3L && isTRUE(has_diagnostics)) {
    " See gtropic_diagnostics() for the complete IDs."
  } else {
    ""
  }
  paste0(
    "Warning: No non-missing precipitation data for ", affected, ".",
    details,
    "\nKnown small/coastal-unit source-data limitation; see ",
    "?gtropic_known_limitations."
  )
}

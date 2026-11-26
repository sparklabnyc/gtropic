#' Known G-TROPIC data limitations
#'
#' @description
#' Documents limitations in the published G-TROPIC source data that can
#' affect retrievals and maps: precipitation gaps for small or coastal units,
#' and flood data that record places named in news reports rather than
#' mapped flood extent.
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
#' ## Flood exposure from news reports
#'
#' G-TROPIC's flood data come from Groundsource (Mayo et al. 2026), a dataset
#' of flood events extracted from news articles. An AI model reads each
#' article, picks out the place and date of the flooding, and looks up that
#' place's boundary on Google Maps. The place can be a street, a
#' neighbourhood, a town, a county or a whole province. Each flood polygon is
#' therefore the outline of a place named in a news report about flooding,
#' not a map of where the water was, and one flood appears once for every
#' place its coverage names.
#'
#' G-TROPIC overlays each polygon on each ADM2 unit and publishes one row per
#' polygon and unit. gtropic returns three of its columns under names that say
#' what they measure; the files on Harvard Dataverse and the codebook keep the
#' published names:
#'
#' - `PROP_AREA_IN_FLOOD_REPORT` (published as `PROP_FLOODED_AREA`): the share
#'   of the unit's area inside the reported place.
#' - `PROP_POP_IN_FLOOD_REPORT` (published as `PROP_FLOODED_POP`): the share of
#'   the unit's population inside the reported place.
#' - `POP_CENTROID_IN_FLOOD_REPORT` (published as `FLOODED_POP_CENTROID`):
#'   whether the unit's population-weighted centroid lies inside the reported
#'   place.
#'
#' These values mostly reflect how precisely a report named a place, not how
#' much of a unit flooded:
#'
#' - A report naming a whole county gives that county a share near 1, even
#'   when the flooding it describes was local.
#' - A place reported neighbourhood by neighbourhood gives many small shares.
#' - Summarising a unit by its largest share ignores all its other reports.
#' - An outline that crosses a border gives a small share to a neighbouring
#'   unit where nothing was reported.
#'
#' For Hurricane Harvey (2017), the largest share is 99% for Bexar County
#' (San Antonio) but 19% for Harris County (Houston), the centre of the
#' catastrophic flooding. The same applies to the population measures.
#'
#' As noted by Groundsource, flood data is also both spatially and temporally
#' biased: flood events in regions and time periods with more accurate
#' reporting are over-represented, while those lacking in news coverage are
#' under-represented. The record covers 2000-01-01 to 2026-01-22. Care should
#' be taken when interpreting and utilizing G-TROPIC flood exposure metrics,
#' especially when comparing places, countries or years.
#'
#' What the rows support most directly is whether a flood was reported in a
#' unit during a storm's flood window. The default exposure map of
#' [gtropic_plot()] shows this as "Flood reported" or "No flood reported"
#' (see its Details); an outline crossing a border can still mark a
#' neighbouring unit. Future G-TROPIC releases are planned to resolve this
#' with a new flood exposure metric.
#'
#' [gtropic_data()] warns once per session when it returns flood rows (class
#' `gtropic_warning_flood_reports`, see [gtropic_conditions]), and every such
#' result keeps the note in `gtropic_diagnostics(x)$flood_reports`. Maps with
#' a flood panel carry a short version of it in their automatic caption.
#'
#' @references
#' Mayo, R., Zlydenko, O., Bootbool, M., Fronman, S., Gilon, O., Hassidim, A.,
#' Kratzert, F., Loike, G., Matias, Y., Nakar, Y., Nearing, G., Sayag, R.,
#' Sicherman, A., Zemach, I. and Cohen, D. (2026). Groundsource: A Dataset of
#' Flood Events from News. EarthArXiv preprint. \doi{10.31223/X5RR2K}
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
precip_all_missing_adm2_ids <- function(
  precip,
  adm2_ids = NULL,
  value_col = "PRECIP_MM"
) {
  if (is.null(adm2_ids)) {
    adm2_ids <- if (!is.null(precip) && "ADM2_ID" %in% names(precip)) {
      precip$ADM2_ID
    } else {
      character(0)
    }
  }
  adm2_ids <- sort(unique(as.character(adm2_ids)))
  adm2_ids <- adm2_ids[!is.na(adm2_ids) & nzchar(adm2_ids)]
  if (!length(adm2_ids)) {
    return(character(0))
  }

  if (
    is.null(precip) ||
      !all(c("ADM2_ID", value_col) %in% names(precip)) ||
      !nrow(precip)
  ) {
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
  if (!length(adm2_ids)) {
    return(invisible(adm2_ids))
  }

  gtropic_warn(
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
plot_precip_limitation_caption <- function(adm2_ids, has_diagnostics = FALSE) {
  if (!length(adm2_ids)) {
    return(NULL)
  }

  affected <- if (length(adm2_ids) <= 3L) {
    paste0(
      "ADM2_ID",
      if (length(adm2_ids) == 1L) " " else "s ",
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
    "Warning: No non-missing precipitation data for ",
    affected,
    ".",
    details,
    "\nKnown small/coastal-unit source-data limitation; see ",
    "?gtropic_known_limitations."
  )
}

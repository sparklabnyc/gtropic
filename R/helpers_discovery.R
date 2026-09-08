# ---------------------------------------------------------------------------
# Discovery helpers.
#
# These exist because unmatched filter values are hard errors. A user needs to
# be able to check the vocabulary before committing to a large pull, and each of
# these is cheap: adm2.parquet and the storm metadata files are small.
# ---------------------------------------------------------------------------

#' List available second-level administrative units
#'
#' @description
#' Retrieves the G-TROPIC lookup table for second-level administrative units
#' (ADM2), such as counties, districts, or provinces. The table provides the
#' geographic identifiers and names used to define study areas in
#' [gtropic_data()].
#'
#' Use this function to identify valid geographic values before requesting
#' exposure data. Filters that do not match a published value produce an error
#' rather than an empty result.
#'
#' @param ADM2_NAME,ADM2_GROUP,WHO_ENTITY,WHO_REGION Optional character vectors
#'   used to restrict the lookup table. Values are matched case-insensitively
#'   after normalizing whitespace. Multiple values within an argument are
#'   combined with OR; filters supplied through different arguments are
#'   combined with AND.
#' @param version Dataset version to use. The default, `":latest"`, uses the
#'   most recent published version. See [gtropic_data()] for version formats.
#' @param cache Whether downloaded files may be cached. `NULL` uses
#'   `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
#'   setting for this call.
#'
#' @return A tibble containing the published ADM2 lookup. Each row represents an
#'   administrative unit and includes its stable `ADM2_ID` and available name,
#'   country or territory, and WHO regional classifications.
#'
#' @examples
#' \donttest{
#' # Review all available administrative units
#' gtropic_adm2()
#'
#' # Find the identifiers used for units in the Philippines
#' gtropic_adm2(WHO_ENTITY = "Philippines")
#' }
#'
#' @seealso [gtropic_data()], [gtropic_storms()]
#' @export
gtropic_adm2 <- function(ADM2_NAME = NULL,
                         ADM2_GROUP = NULL,
                         WHO_ENTITY = NULL,
                         WHO_REGION = NULL,
                         version = ":latest",
                         cache = NULL) {
  ctx <- helper_context(version, cache)

  adm2 <- tibble::as_tibble(dplyr::collect(fetch_file(
    "historical", "metadata", "adm2.parquet", ctx
  )))

  filters <- compact(list(
    ADM2_NAME = as_filter_chr(ADM2_NAME, "ADM2_NAME"),
    ADM2_GROUP = as_filter_chr(ADM2_GROUP, "ADM2_GROUP"),
    WHO_ENTITY = as_filter_chr(WHO_ENTITY, "WHO_ENTITY"),
    WHO_REGION = as_filter_chr(WHO_REGION, "WHO_REGION")
  ))

  if (length(filters) == 0L) {
    return(adm2)
  }

  geo <- resolve_geography(adm2, filters = filters)
  adm2[adm2$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
}

#' List available tropical cyclones
#'
#' @description
#' Retrieves metadata for tropical cyclones represented in G-TROPIC, including
#' storm identifiers, names, and genesis dates. This function can be used to
#' identify storms and confirm temporal coverage before retrieving exposure
#' data with [gtropic_data()].
#'
#' Date filters are applied to `STORM_GENESIS_DATE_UTC`, the UTC date of the
#' first best-track observation. They do not select storms by landfall, closest
#' approach, or date of exposure.
#'
#' @param date_range Optional vector of length two defining an inclusive range
#'   of storm genesis dates. Bare years, year-month values, complete dates, and
#'   `Date` or `POSIXct` values are accepted as described in [gtropic_data()].
#' @param year Optional vector of years to retrieve. Ignored when `date_range`
#'   is supplied. Years outside the published collection are omitted.
#' @param STORM_NAME Optional character vector of storm names. Matching is
#'   case-insensitive; every supplied value must match a published name.
#' @param version Dataset version to use. The default, `":latest"`, uses the
#'   most recent published version. See [gtropic_data()] for version formats.
#' @param cache Whether downloaded files may be cached. `NULL` uses
#'   `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
#'   setting for this call.
#'
#' @return A tibble containing the published metadata records for storms that
#'   satisfy the requested genesis period and name filters. If no metadata files
#'   are available for the selected years, a warning is issued and an empty
#'   tibble is returned.
#'
#' @examples
#' \donttest{
#' # Review storms that formed during the 2005 season
#' gtropic_storms(year = 2005)
#'
#' # Retrieve the published identifiers and dates for Hurricane Katrina
#' gtropic_storms(STORM_NAME = "Katrina-2005")
#' }
#'
#' @seealso [gtropic_data()], [gtropic_adm2()]
#' @export
gtropic_storms <- function(date_range = NULL,
                           year = NULL,
                           STORM_NAME = NULL,
                           version = ":latest",
                           cache = NULL) {
  ctx <- helper_context(version, cache)

  date_spec <- parse_date_input(date_range = date_range)

  years <- if (!identical(date_spec$type, "none")) {
    year_span(date_spec$start, date_spec$end)
  } else if (!is.null(year)) {
    as_year_int(year)
  } else {
    ctx$discovery$year_index$year
  }
  years <- intersect(years, ctx$discovery$year_index$year)

  meta <- fetch_yearly("storm_metadata", years, ctx)
  if (is.null(meta)) {
    cli::cli_warn("No storm metadata found for the years requested.")
    return(tibble::tibble())
  }

  if (!identical(date_spec$type, "none")) {
    genesis <- as.Date(meta$STORM_GENESIS_DATE_UTC)
    meta <- meta[
      !is.na(genesis) &
        genesis >= date_spec$start & genesis <= date_spec$end, ,
      drop = FALSE
    ]
  }

  if (!is.null(STORM_NAME)) {
    values <- as_filter_chr(STORM_NAME, "STORM_NAME")
    canonical <- match_values(values, meta$STORM_NAME,
      arg = "STORM_NAME",
      exact = FALSE, helper = "gtropic_storms()",
      what = "storm"
    )
    meta <- meta[norm_key(meta$STORM_NAME) %in% norm_key(canonical), ,
      drop = FALSE
    ]
  }

  meta
}

#' Report the temporal coverage of G-TROPIC
#'
#' @description
#' Reports the calendar years represented in the historical and current-year
#' G-TROPIC datasets. This information can be used to define feasible study
#' periods before retrieving storm exposure data.
#'
#' The historical collection contains completed years. When available, the
#' separate current-year collection is updated as the tropical cyclone season
#' progresses and may therefore represent provisional temporal coverage.
#'
#' @param version Dataset version or versions to inspect. The default,
#'   `":latest"`, uses the most recent published versions. See [gtropic_data()]
#'   for supported version formats.
#'
#' @return A list of integer vectors with components `historical`, `current`,
#'   and `all`. `all` is the sorted union of the two collections; `current` is
#'   empty when no separate current-year dataset is available.
#'
#' @examples
#' \donttest{
#' coverage <- gtropic_years()
#' range(coverage$all)
#' }
#'
#' @seealso [gtropic_data()]
#' @export
gtropic_years <- function(version = ":latest") {
  discovery <- discover_datasets(resolve_versions(version), quiet = TRUE)

  list(
    historical = discovery$historical$years,
    current = discovery$current$years %||% integer(0),
    all = sort(unique(discovery$year_index$year))
  )
}

#' Retrieve the G-TROPIC data codebook
#'
#' @description
#' Downloads the published G-TROPIC codebook. The codebook describes the tables,
#' variables, measurement units, and data provenance needed to interpret wind,
#' precipitation, flood, population, and storm metadata in an exposure
#' analysis.
#'
#' @param version Dataset version to use. The default, `":latest"`, uses the
#'   most recent published version. See [gtropic_data()] for version formats.
#'
#' @return A nested list preserving the structure of the published JSON
#'   codebook.
#'
#' @examples
#' \donttest{
#' codebook <- gtropic_codebook()
#' names(codebook)
#' }
#'
#' @seealso [gtropic_data()]
#' @export
gtropic_codebook <- function(version = ":latest") {
  ctx <- helper_context(version, cache = NULL)
  fetch_file("historical", "metadata", "codebook.json", ctx, kind = "json")
}

#' Build a lightweight fetch context for the discovery helpers
#'
#' The helpers do not return manifests, but `fetch_file()` accumulates one, so
#' they still need a context object.
#'
#' @param version Version argument.
#' @param cache Cache argument.
#'
#' @return A fetch context.
#' @keywords internal
#' @noRd
helper_context <- function(version = ":latest", cache = NULL) {
  versions <- resolve_versions(version)
  discovery <- discover_datasets(versions, quiet = TRUE)
  fetch_context(discovery, cache_resolve(cache, quiet = TRUE), quiet = TRUE)
}

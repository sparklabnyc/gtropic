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
#' @inheritParams gtropic_data
#'
#' @return A tibble containing the published ADM2 lookup. Each row represents an
#'   administrative unit and includes its stable `ADM2_ID` and available name,
#'   country or territory, and WHO regional classifications.
#'
#' @examplesIf interactive()
#' # Review all available administrative units
#' gtropic_adm2()
#'
#' # Find the identifiers used for units in the Philippines
#' gtropic_adm2(WHO_ENTITY = "Philippines")
#'
#' @family discovery functions
#' @export
gtropic_adm2 <- function(ADM2_NAME = NULL,
                         ADM2_GROUP = NULL,
                         WHO_ENTITY = NULL,
                         WHO_REGION = NULL,
                         version = ":latest",
                         cache = NULL) {
  ctx <- helper_context(version, cache)
  on.exit(cache_manage_ctx(ctx), add = TRUE)

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
#' When a requested `STORM_NAME` is not available, G-TROPIC checks the published
#' exclusion records and reports whether the storm is excluded or absent from
#' both lists.
#'
#' `STORM_NAME` is a convenience lookup and can identify multiple storms. If a
#' name matches multiple unique `STORM_ID`s after all filters are applied, this
#' function returns every match and warns. Use `gtropic_storms(STORM_NAME = ...)`
#' to identify the intended `STORM_ID`, then query [gtropic_data()] with
#' `STORM_ID`, which is unique and preferred.
#'
#' @param date_range Optional vector of one or two endpoints defining an
#'   inclusive range of storm genesis dates; a single value is used for both
#'   endpoints, so `date_range = 2005` selects storms that formed in 2005. Bare
#'   years, year-month values, complete dates, and `Date` or `POSIXct` values
#'   are accepted as described in [gtropic_data()]. Years outside the
#'   published collection are omitted.
#' @param STORM_NAME Optional character vector of published convenience labels
#'   in `Name-YYYY` form, such as `Katrina-2005`. Labels can duplicate across
#'   distinct `STORM_ID`s. Matching is case-insensitive; every supplied value
#'   must match a published name. The suffix is the storm season. Because
#'   Southern Hemisphere storms can form in the preceding calendar year,
#'   partitions for both `YYYY - 1` and `YYYY` are considered. This candidate
#'   set is combined with a `date_range` filter using AND.
#' @inheritParams gtropic_data
#'
#' @return A tibble containing the published metadata records for storms that
#'   satisfy the requested genesis period and name filters. If no metadata files
#'   are available for the selected years, a warning is issued and an empty
#'   tibble is returned. `STORM_GENESIS_DATE_UTC` is returned as a `Date`.
#'
#' @examplesIf interactive()
#' # Review storms that formed during calendar year 2005
#' gtropic_storms(date_range = 2005)
#'
#' # Retrieve the published identifiers and dates for Hurricane Katrina
#' gtropic_storms(STORM_NAME = "Katrina-2005")
#'
#' @family discovery functions
#' @export
gtropic_storms <- function(date_range = NULL,
                            STORM_NAME = NULL,
                            version = ":latest",
                            cache = NULL) {
  storm_name <- as_filter_chr(STORM_NAME, "STORM_NAME")
  storm_name_year <- storm_name_years(storm_name)
  ctx <- helper_context(version, cache)
  on.exit(cache_manage_ctx(ctx), add = TRUE)

  date_spec <- parse_date_input(date_range = date_range)
  years <- resolve_helper_years(date_spec, storm_name_year, ctx)

  meta <- fetch_yearly("storm_metadata", years, ctx)
  if (is.null(meta)) {
    cli::cli_warn("No storm metadata found for the years requested.")
    return(tibble::tibble())
  }
  meta <- filter_genesis_dates(meta, date_spec)

  if (!is.null(storm_name)) {
    canonical <- match_storm_values(storm_name, meta$STORM_NAME,
      arg = "STORM_NAME",
      exact = FALSE, helper = "gtropic_storms()",
      what = "storm",
      excluded_lookup = function() {
        fetch_excluded_storms(years, ctx,
          col_select = c("STORM_ID", "STORM_NAME")
        )
      }
    )
    meta <- meta[norm_key(meta$STORM_NAME) %in% norm_key(canonical), ,
      drop = FALSE
    ]
    warn_ambiguous_storm_names(meta, canonical)
  }

  meta
}

#' List excluded tropical cyclones
#'
#' @description
#' Retrieves records for storms excluded from the published wind data. These
#' records identify storms that cannot be retrieved with [gtropic_data()].
#' Published `STORM_NAME` convenience labels can duplicate; the unique
#' `STORM_ID` values in the returned rows distinguish excluded records.
#'
#' Date filters are applied to `STORM_GENESIS_DATE_UTC`, the UTC date of the
#' first best-track observation. They do not select storms by landfall, closest
#' approach, or date of exposure.
#'
#' @param date_range Optional vector of one or two endpoints defining an
#'   inclusive range of storm genesis dates; a single value is used for both
#'   endpoints, so `date_range = 2005` selects storms that formed in 2005. Bare
#'   years, year-month values, complete dates, and `Date` or `POSIXct` values
#'   are accepted as described in [gtropic_data()]. Years outside the
#'   published collection are omitted.
#' @param STORM_NAME Optional character vector of published convenience labels
#'   in `Name-YYYY` form. Labels can duplicate across distinct `STORM_ID`s.
#'   Matching is case-insensitive; every supplied value must match an
#'   excluded-storm record. The suffix is the storm season; partitions for
#'   genesis years `YYYY - 1` and `YYYY` are considered and combined with a
#'   `date_range` filter using AND.
#' @inheritParams gtropic_data
#'
#' @return A tibble with `STORM_ID`, `STORM_NAME`, and
#'   `STORM_GENESIS_DATE_UTC` for published excluded storms satisfying the
#'   requested genesis period and name filters. If no exclusion files are
#'   available for the selected years, a warning is issued and an empty tibble is
#'   returned. `STORM_GENESIS_DATE_UTC` is returned as a `Date`.
#'
#' @examplesIf interactive()
#' # Review exclusion records for storms that formed during calendar year 2005
#' gtropic_excluded_storms(date_range = 2005)
#'
#' @family discovery functions
#' @export
gtropic_excluded_storms <- function(date_range = NULL,
                                     STORM_NAME = NULL,
                                     version = ":latest",
                                     cache = NULL) {
  storm_name <- as_filter_chr(STORM_NAME, "STORM_NAME")
  storm_name_year <- storm_name_years(storm_name)
  ctx <- helper_context(version, cache)
  on.exit(cache_manage_ctx(ctx), add = TRUE)

  date_spec <- parse_date_input(date_range = date_range)
  years <- resolve_helper_years(date_spec, storm_name_year, ctx)

  excluded <- fetch_excluded_storms(years, ctx)
  if (nrow(excluded) == 0L) {
    cli::cli_warn("No excluded-storm records found for the years requested.")
    return(tibble::tibble())
  }
  excluded <- filter_genesis_dates(excluded, date_spec)

  if (!is.null(storm_name)) {
    canonical <- match_values(storm_name, excluded$STORM_NAME,
      arg = "STORM_NAME",
      exact = FALSE, helper = "gtropic_excluded_storms()",
      what = "excluded storm"
    )
    excluded <- excluded[
      norm_key(excluded$STORM_NAME) %in% norm_key(canonical), ,
      drop = FALSE
    ]
    warn_ambiguous_storm_names(excluded, canonical, excluded = TRUE)
  }

  excluded
}

#' Genesis-year partitions a discovery helper reads
#'
#' Years come from the date filter (or every published year), narrowed to the
#' genesis years a `Name-YYYY` season suffix allows, then to what is published.
#'
#' @param date_spec Output of `parse_date_input()`.
#' @param storm_name_year Season years from `storm_name_years()`, or `NULL`.
#' @param ctx Fetch context.
#'
#' @return Integer years.
#' @keywords internal
#' @noRd
resolve_helper_years <- function(date_spec, storm_name_year, ctx) {
  published <- ctx$discovery$year_index$year
  has_dates <- !identical(date_spec$type, "none")
  years <- if (has_dates) year_span(date_spec$start, date_spec$end) else published
  if (!is.null(storm_name_year)) {
    storm_genesis_year <- storm_name_genesis_years(storm_name_year)
    years <- if (has_dates) intersect(years, storm_genesis_year) else storm_genesis_year
  }
  intersect(years, published)
}

#' Keep rows whose genesis date lies in the requested range
#'
#' @param x A tibble with `STORM_GENESIS_DATE_UTC`.
#' @param date_spec Output of `parse_date_input()`.
#'
#' @return `x`, filtered when a date range was given.
#' @keywords internal
#' @noRd
filter_genesis_dates <- function(x, date_spec) {
  if (identical(date_spec$type, "none")) {
    return(x)
  }
  genesis <- as.Date(x$STORM_GENESIS_DATE_UTC)
  x[
    !is.na(genesis) & genesis >= date_spec$start & genesis <= date_spec$end, ,
    drop = FALSE
  ]
}

#' Fetch excluded-storm records for selected years
#'
#' @param years Integer years.
#' @param ctx Fetch context.
#' @param col_select Optional projected columns.
#'
#' @return A tibble, possibly with zero rows.
#' @keywords internal
#' @noRd
fetch_excluded_storms <- function(years, ctx, col_select = NULL) {
  excluded <- fetch_yearly("excluded_storms", years, ctx,
    col_select = col_select
  )
  if (!is.null(excluded)) {
    return(excluded)
  }
  tibble::tibble(STORM_ID = character(), STORM_NAME = character())
}

#' Report the temporal coverage of G-TROPIC
#'
#' @description
#' Reports the genesis calendar years represented in the historical and
#' current-year G-TROPIC datasets. Storm-linked annual coverage is based on
#' `STORM_GENESIS_DATE_UTC`, not the season suffix in `STORM_NAME`. This
#' information can be used to define feasible study periods before retrieving
#' storm exposure data.
#'
#' The historical collection contains completed genesis calendar years. When
#' available, the separate current-year collection is updated as storms with
#' genesis dates in the current calendar year are published, so coverage for
#' that calendar year may be provisional.
#'
#' @inheritParams gtropic_data
#'
#' @return A list of integer vectors with components `historical`, `current`,
#'   and `all`. `all` is the sorted union of the two collections; `current` is
#'   empty when no separate current-year dataset is available.
#'
#' @examplesIf interactive()
#' coverage <- gtropic_years()
#' range(coverage$all)
#'
#' @family discovery functions
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
#' @inheritParams gtropic_data
#'
#' @return A nested list preserving the structure of the published JSON
#'   codebook.
#'
#' @examplesIf interactive()
#' codebook <- gtropic_codebook()
#' names(codebook)
#'
#' @family discovery functions
#' @export
gtropic_codebook <- function(version = ":latest", cache = NULL) {
  ctx <- helper_context(version, cache = cache)
  on.exit(cache_manage_ctx(ctx), add = TRUE)
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
  fetch_context(discovery, cache_resolve(cache, quiet = TRUE),
    quiet = TRUE,
    latest_dois = latest_dois(discovery, versions),
    notice_quiet = opt("quiet")
  )
}

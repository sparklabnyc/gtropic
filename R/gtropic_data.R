#' Retrieve tropical cyclone exposure data from G-TROPIC
#'
#' @description
#' Retrieves and filters published G-TROPIC data for assessing tropical cyclone
#' exposures at the second-level administrative unit (ADM2). Available data
#' include wind exposure, daily precipitation, flood events, population,
#' administrative-unit characteristics, storm tracks, and storm metadata.
#'
#' The function subsets published values; it does not recalculate hazard or
#' exposure measures. This preserves the definitions and provenance of the
#' source data for environmental health and epidemiologic analyses.
#'
#' @details
#' ## Combining filters
#'
#' Several values in one filter are combined with OR; different filters are
#' combined with AND. `WHO_ENTITY = c("United States", "Mexico")` selects units
#' in either country, whereas `ADM2_NAME = "San Jose", ADM2_GROUP = "CRI"`
#' requires both. A `polygon` is combined with the other geographic filters
#' the same way. Identifiers (`ADM2_ID`, `STORM_ID`, `USA_ATCF_ID`) match
#' exactly; names and classifications match case-insensitively after
#' whitespace is normalised. Every value must match a published value, and the
#' filters together must select something: [gtropic_adm2()], [gtropic_storms()]
#' and [gtropic_excluded_storms()] list the valid and excluded values.
#'
#' The filter arguments are written in capitals because each is named after
#' the published column it filters, as that column appears in the returned
#' tables and the codebook.
#'
#' ## Dates
#'
#' `date_range` and `date_list` select storms by `STORM_GENESIS_DATE_UTC`, the
#' UTC date of the first best-track observation, not by landfall or by the date
#' of local exposure (see `vignette("filtering", package = "gtropic")`). The
#' endpoints of `date_range` are read as follows:
#'
#' | Input | Start endpoint | End endpoint |
#' | --- | --- | --- |
#' | `"2001"` or `2001` | `2001-01-01` | `2001-12-31` |
#' | `"2001-03"` | `2001-03-01` | `2001-03-31` |
#' | `"2001-03-04"` | unchanged | unchanged |
#' | `Date` or `POSIXct` | calendar date | calendar date |
#'
#' Values in `date_list` must be complete dates. Non-ISO dates are read in one
#' order for all values; when more than one order fits, `ymd` is preferred,
#' then `mdy`, then `dmy`, with a warning. `date_format` fixes the order.
#'
#' ## Defaults that shape the result
#'
#' - `wind_radius_km = 500` keeps wind records for pairs whose published
#'   `STORM_DIST_KM` is at most 500 km.
#' - Precipitation and flood records are kept for pairs within
#'   `precip_radius_km` and `flood_radius_km` (500 km each), in windows from 2
#'   days before to 1 day after the local date of closest approach. `links`
#'   keeps every pair inside the largest requested radius, so it can hold pairs
#'   beyond the wind radius. Each precipitation or flood record appears once,
#'   even when it falls in several storms' windows: join it to `links` to
#'   assign it to storms (see `vignette("linkage", package = "gtropic")`).
#' - A download estimated above `getOption("gtropic.confirm_threshold_gb")`
#'   (1 GB) needs confirmation; in a non-interactive session it is an error
#'   unless `confirm = FALSE`.
#'
#' Each result carries a manifest of the dataset versions, files and checksums
#' it used. Passing it back as `manifest` retrieves the same source files and
#' replays the resolved query, including the resolved ADM2 and storm
#' identifiers (see `vignette("versions-and-caching", package = "gtropic")`).
#'
#' @param ADM2_ID Optional character vector of published ADM2 identifiers.
#'   Matching is exact and case-sensitive.
#' @param ADM2_NAME Optional character vector of ADM2 names, matched
#'   case-insensitively.
#' @param ADM2_GROUP Optional character vector of published ADM2 groupings,
#'   typically country or territory codes, matched case-insensitively.
#' @param WHO_ENTITY Optional character vector of WHO entity names, matched
#'   case-insensitively.
#' @param WHO_REGION Optional character vector of WHO region names or codes,
#'   matched case-insensitively.
#' @param polygon Optional `sf` or `sfc` study area with a coordinate
#'   reference system; needs the `sf` package. A unit is selected when its
#'   published population-weighted centroid lies inside the polygon in at
#'   least one year the query covers, and units whose membership changes
#'   between years are reported. Centroids are read from the yearly population
#'   file of every candidate year (about 2 MB each), so without a date filter a
#'   polygon query downloads one such file for every published year.
#' @param reproject Logical. If `TRUE` (the default), a `polygon` not already in
#'   EPSG:4326 is transformed before evaluation. If `FALSE`, a non-EPSG:4326
#'   polygon produces an error.
#' @param date_range Optional vector of one or two endpoints defining an
#'   inclusive period of storm genesis dates. A single value is used for both
#'   endpoints. Cannot be combined with `date_list`.
#' @param date_list Optional vector of complete genesis dates to match exactly.
#'   Cannot be combined with `date_range`.
#' @param date_format Optional character scalar specifying the order of
#'   non-ISO dates, such as `"ymd"`, `"mdy"`, or `"dmy"`. When supplied,
#'   automatic date-order inference is not used.
#' @param STORM_ID Optional character vector of published G-TROPIC storm
#'   identifiers. Each value uniquely identifies one storm. Matching is exact
#'   and case-sensitive; this is the preferred storm key.
#' @param STORM_NAME Optional character vector of published convenience labels
#'   in `Name-YYYY` form, such as `Katrina-2005`, matched case-insensitively.
#'   Complete values can match multiple `STORM_ID`s, including across basins.
#'   `YYYY` is the published `SEASON` year and need not be the genesis calendar
#'   year. Use [gtropic_storms()] to inspect matches, then prefer `STORM_ID` for
#'   retrieval and downstream work. Storm and date filters combine with AND.
#' @param USA_ATCF_ID Optional character vector of US Automated Tropical Cyclone
#'   Forecasting System identifiers. Matching is exact and case-sensitive.
#' @param wind_radius_km Maximum published storm-track distance (km) for
#'   returned wind records; default 500. Records at exactly this distance are
#'   kept. `Inf` or `NULL` removes the bound and also keeps records without a
#'   published distance. It does not filter the returned `zero_pairs`.
#' @param tables Tables to return; default all of `"wind"`, `"precip"`,
#'   `"flood"`, `"pop"`, `"adm2"`, `"codebook"`, `"zero_pairs"`, `"tracks"`,
#'   `"geometry"` and `"storm_metadata"`. A shorter vector downloads less.
#'   `"geometry"` needs the suggested `sf` package: when `tables` is not
#'   supplied and `sf` is missing, it is left out with a message.
#'   `storm_metadata` is added whenever a storm-linked table is requested, and
#'   a `links` table is always returned. Precipitation and flood also need the
#'   `zero_pairs` files to build links, which adds to the download.
#' @param precip_days_before,precip_days_after Days (whole numbers, 0 or more)
#'   before and after the local date of closest approach in each precipitation
#'   window; defaults 2 and 1.
#' @param precip_radius_km Maximum published storm-track distance (km) for a
#'   pair to be linked to precipitation; default 500. `Inf` or `NULL` removes
#'   the bound.
#' @param flood_days_before,flood_days_after Days (whole numbers, 0 or more)
#'   before and after the local date of closest approach in each flood window;
#'   defaults 2 and 1.
#' @param flood_radius_km Maximum published storm-track distance (km) for a
#'   pair to be linked to floods; default 500. `Inf` or `NULL` removes the
#'   bound.
#' @param flood_match How floods are matched to windows: `"start"` (the
#'   default) keeps floods that start inside the window; `"overlap"` keeps any
#'   flood that overlaps it, including one already under way when the window
#'   opens.
#' @param geometry_resolution Resolution of boundaries returned when
#'   `tables` includes `"geometry"`: either `"simplified"` (the default) or
#'   `"full"`.
#' @param make_valid Logical; default `TRUE`, which repairs invalid returned
#'   ADM2 geometries with [sf::st_make_valid()]. Repair can change geometry
#'   types, for example from polygon to multipolygon. `FALSE` keeps the
#'   published geometry exactly and skips the check. Recorded in the manifest.
#' @param version Dataset version specification. Use `":latest"` (the default),
#'   a single version applied to both datasets, or a named character vector such
#'   as `c(historical = "2.0", current = "3.0")`. A pinned version reads the
#'   files of that version, and the manifest records their file ids and
#'   checksums.
#' @param manifest Optional `gtropic_manifest` from [gtropic_manifest()]. When
#'   supplied, its recorded source files and canonical resolved query are used.
#'   Data-shaping arguments cannot also be supplied; only `cache`, `confirm`,
#'   `quiet`, and `offline` may differ during replay.
#' @param cache Whether downloaded source files may be cached. `NULL` uses
#'   `getOption("gtropic.cache_enabled")`; `TRUE` or `FALSE` overrides that
#'   setting for this call.
#' @param confirm Logical; default `TRUE`, which asks before a download
#'   larger than `getOption("gtropic.confirm_threshold_gb")`. `FALSE` downloads
#'   without asking.
#' @param quiet Logical. If `TRUE`, progress and informational messages are
#'   suppressed. Warnings and errors are still reported. Defaults to
#'   `getOption("gtropic.quiet", FALSE)`, so `options(gtropic.quiet = TRUE)`
#'   silences every call in a session.
#' @param offline Logical; default `FALSE`. `TRUE` uses only cached files and
#'   the saved discovery snapshot, makes no request to Dataverse, and reports
#'   the snapshot's age. Requires an enabled cache.
#'
#' @return An object of class `gtropic_data`, implemented as a named list. It
#'   contains the requested data tables and a `links` tibble with one row per
#'   ADM2-storm pair retained by the union of requested hazard radii (or all
#'   available selected pairs when that union is unbounded). `links` includes
#'   `ADM2_ID`, `STORM_ID`, the published local date and distance at closest
#'   approach, logical `WIND_EXPOSURE` (`TRUE` for nonzero wind exposure),
#'   `WIND_IN_FIELD`, `PRECIP_IN_FIELD` and `FLOOD_IN_FIELD`, and the start and
#'   end of each precipitation and flood window. With a finite requested hazard
#'   radius, `tracks` contains only storms with at least one qualifying link.
#'   `pop` has a `YEAR` column taken from each yearly source file name.
#'
#'   Tabular elements are tibbles, `codebook` is a nested list, and `geometry`
#'   is an `sf` object. Date-only columns (genesis, local and closest-approach
#'   dates, flood start and end, window bounds) are `Date` vectors; datetimes
#'   with a local UTC offset stay ISO 8601 text, as published.
#'
#'   The object also stores the source-file manifest, original query, resolved
#'   filters, recorded diagnostics, availability metadata, retrieval time and
#'   package version as attributes. Use [gtropic_manifest()],
#'   [gtropic_filters()], [gtropic_diagnostics()] and [gtropic_availability()]
#'   to read them.
#'
#' @examples
#' # A small synthetic result, with the same structure as a real one
#' x <- gtropic_example()
#' x
#' names(x)
#'
#' @examplesIf interactive()
#' # The queries below download data from Harvard Dataverse.
#'
#' # All available tables for storms with genesis dates in calendar year 2005
#' # that were evaluated in Jamaica
#' jamaica <- gtropic_data(WHO_ENTITY = "Jamaica", date_range = 2005)
#' jamaica
#'
#' # Find a storm by its convenience label, then query by its unique STORM_ID
#' gtropic_storms(STORM_NAME = "Haiyan-2013")
#' haiyan <- gtropic_data(
#'   STORM_ID = "2013306N07162",
#'   date_range = 2013,
#'   tables = c("wind", "adm2")
#' )
#'
#' # Define a seven-day rainfall window and a 300-km storm field for an
#' # epidemiologic study in the Philippines
#' philippines <- gtropic_data(
#'   WHO_ENTITY = "Philippines",
#'   date_range = c("2015-06-01", "2015-12-31"),
#'   precip_days_before = 3,
#'   precip_days_after = 3,
#'   precip_radius_km = 300,
#'   tables = c("precip", "pop", "adm2")
#' )
#'
#' # Retain source-file provenance for a subsequent retrieval
#' source_manifest <- gtropic_manifest(philippines)
#' philippines_retrieved_again <- gtropic_data(manifest = source_manifest)
#'
#' @seealso [gtropic_storms()] and [gtropic_adm2()] to look up filter values,
#'   [gtropic_plot()] to map the result, [gtropic_options] for settings.
#' @family retrieval functions
#' @export
gtropic_data <- function(ADM2_ID = NULL,
                         ADM2_NAME = NULL,
                         ADM2_GROUP = NULL,
                         WHO_ENTITY = NULL,
                         WHO_REGION = NULL,
                         polygon = NULL,
                         reproject = TRUE,
                         date_range = NULL,
                         date_list = NULL,
                         date_format = NULL,
                         STORM_ID = NULL,
                         STORM_NAME = NULL,
                         USA_ATCF_ID = NULL,
                         wind_radius_km = 500,
                         tables = c(
                           "wind", "precip", "flood", "pop", "adm2",
                           "codebook", "zero_pairs", "tracks", "geometry",
                           "storm_metadata"
                         ),
                         precip_days_before = 2,
                         precip_days_after = 1,
                         precip_radius_km = 500,
                         flood_days_before = 2,
                         flood_days_after = 1,
                         flood_radius_km = 500,
                         flood_match = c("start", "overlap"),
                         geometry_resolution = c("simplified", "full"),
                         make_valid = TRUE,
                         version = ":latest",
                         manifest = NULL,
                         cache = NULL,
                         confirm = TRUE,
                         quiet = getOption("gtropic.quiet", FALSE),
                         offline = FALSE) {
  call <- match.call()
  if (missing(tables) && is.null(manifest)) {
    tables <- default_tables(quiet = quiet)
  }

  if (!is.null(manifest)) {
    manifest <- manifest_validate(manifest)
    manifest_assert_replay_arguments(call)
    manifest_warn_server(manifest)
  }

  # The canonical query (and so the manifest) records both endpoints.
  if (!is.null(date_range) && length(date_range) == 1L) {
    date_range <- c(date_range, date_range)
  }

  flood_match <- rlang::arg_match(flood_match)
  geometry_resolution <- rlang::arg_match(geometry_resolution)
  storm_name <- as_filter_chr(STORM_NAME, "STORM_NAME")

  args <- list(
    geo_filters = compact(list(
      ADM2_ID = as_filter_chr(ADM2_ID, "ADM2_ID"),
      ADM2_NAME = as_filter_chr(ADM2_NAME, "ADM2_NAME"),
      ADM2_GROUP = as_filter_chr(ADM2_GROUP, "ADM2_GROUP"),
      WHO_ENTITY = as_filter_chr(WHO_ENTITY, "WHO_ENTITY"),
      WHO_REGION = as_filter_chr(WHO_REGION, "WHO_REGION")
    )),
    storm_filters = compact(list(
      STORM_ID = as_filter_chr(STORM_ID, "STORM_ID"),
      STORM_NAME = storm_name,
      USA_ATCF_ID = as_filter_chr(USA_ATCF_ID, "USA_ATCF_ID")
    )),
    storm_name_years = storm_name_years(storm_name),
    wind_radius_km = check_radius_km(wind_radius_km),
    polygon = polygon,
    reproject = check_flag(reproject),
    date_range = date_range,
    date_list = date_list,
    date_format = date_format,
    tables = normalize_tables(tables),
    precip_days_before = check_nonneg_int(
      precip_days_before,
      "precip_days_before"
    ),
    precip_days_after = check_nonneg_int(
      precip_days_after,
      "precip_days_after"
    ),
    precip_radius_km = check_radius_km(
      precip_radius_km,
      "precip_radius_km"
    ),
    flood_days_before = check_nonneg_int(
      flood_days_before,
      "flood_days_before"
    ),
    flood_days_after = check_nonneg_int(flood_days_after, "flood_days_after"),
    flood_radius_km = check_radius_km(flood_radius_km, "flood_radius_km"),
    flood_match = flood_match,
    geometry_resolution = geometry_resolution,
    make_valid = check_flag(make_valid),
    version = version,
    manifest = manifest,
    cache = cache,
    confirm = check_flag(confirm),
    quiet = check_flag(quiet),
    offline = check_flag(offline),
    # Populated after source metadata has resolved the user-facing filters.
    # They are separate from the original filters so a replay does not need to
    # read ADM2 metadata merely to validate already-canonical IDs.
    resolved_adm2_ids = NULL,
    resolved_storm_ids = NULL,
    storm_metadata_years = NULL
  )

  if (!is.null(manifest)) {
    # Only the data-shaping query is replayed. How it is executed (`cache`,
    # `confirm`, `quiet`, `offline`) stays the caller's choice, whatever the
    # manifest contains.
    q <- manifest$resolved_query
    replayed <- intersect(names(q), manifest_query_fields())
    args[replayed] <- q[replayed]
    args$manifest <- manifest
  }
  args$resolved_query <- manifest_query_from_args(args)

  assemble_gtropic_data(args, call = call)
}

#' Reject conflicting data-shaping arguments on manifest replay
#' @keywords internal
#' @noRd
manifest_assert_replay_arguments <- function(call, call_env = rlang::caller_env()) {
  supplied <- setdiff(names(call), "")
  shaping <- c(
    "ADM2_ID", "ADM2_NAME", "ADM2_GROUP", "WHO_ENTITY", "WHO_REGION",
    "polygon", "reproject", "date_range", "date_list", "date_format",
    "STORM_ID", "STORM_NAME", "USA_ATCF_ID", "wind_radius_km", "tables",
    "precip_days_before", "precip_days_after", "precip_radius_km",
    "flood_days_before", "flood_days_after", "flood_radius_km", "flood_match",
    "geometry_resolution", "make_valid", "version"
  )
  conflicts <- intersect(supplied, shaping)
  if (length(conflicts) > 0L) {
    cli::cli_abort(c(
      "A manifest already defines the resolved query.",
      x = "Do not also supply data-shaping argument{?s}: {.arg {conflicts}}.",
      i = "You may change {.arg cache}, {.arg confirm}, {.arg quiet}, or {.arg offline}."
    ), call = call_env)
  }
  invisible(TRUE)
}

#' Create the canonical, replayable portion of a query
#' @keywords internal
#' @noRd
manifest_query_from_args <- function(args, resolved_adm2_ids = NULL,
                                     resolved_storm_ids = NULL,
                                     storm_metadata_years = NULL) {
  out <- args[manifest_query_fields()]
  if (!is.null(resolved_adm2_ids)) {
    out$resolved_adm2_ids <- sort(unique(as.character(resolved_adm2_ids)))
    # IDs below are the canonical geographic selection; name/country filters
    # and polygon geometry have already done their work.
    out$geo_filters <- list()
    # `$<- NULL` removes a list element. Keep this named field so manifests
    # always satisfy the complete canonical-query schema.
    out["polygon"] <- list(NULL)
  }
  if (!is.null(resolved_storm_ids)) {
    out$resolved_storm_ids <- sort(unique(as.character(resolved_storm_ids)))
    out$storm_filters <- list(STORM_ID = out$resolved_storm_ids)
    out["storm_name_years"] <- list(NULL)
  }
  if (!is.null(storm_metadata_years)) {
    out$storm_metadata_years <- sort(unique(as.integer(storm_metadata_years)))
  }
  out
}

#' Validate a non-negative whole-number argument
#'
#' @param x Value supplied.
#' @param arg Argument name.
#' @param call Calling environment.
#'
#' @return `x` as a numeric scalar.
#' @keywords internal
#' @noRd
check_nonneg_int <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 ||
    x != round(x)) {
    cli::cli_abort(
      c("{.arg {arg}} must be a single non-negative whole number of days.",
        x = "Got {.val {x}}."
      ),
      call = call
    )
  }
  as.numeric(x)
}

#' Validate a radius argument
#'
#' `NULL` is an explicit request for no distance restriction. It is normalized
#' to `Inf` so downstream filtering has one unbounded representation.
#'
#' @param x Value supplied.
#' @param arg Argument name.
#' @param call Calling environment.
#'
#' @return A positive finite numeric scalar or `Inf`.
#' @keywords internal
#' @noRd
check_radius_km <- function(x, arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (is.null(x)) {
    return(Inf)
  }

  valid <- is.numeric(x) && length(x) == 1L &&
    (is.infinite(x) && x > 0 || is.finite(x) && x > 0)
  if (!valid) {
    cli::cli_abort(
      c("{.arg {arg}} must be a single positive finite number, {Inf}, or {.code NULL}.",
        x = "Got {.val {x}}."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# ---------------------------------------------------------------------------
# Orchestration.
#
# gtropic_data() itself performs no I/O. It validates and delegates here, and
# this file is the only place where the planner (pure) and the fetcher
# (networked) meet. Resolution order matters and is fixed:
#
#   1. validate arguments, normalise tables, resolve options
#   2. resolve DOIs and versions -> manifest skeleton
#   3. fetch adm2 (+ pop years if polygon) -> resolve geography
#   4. parse dates -> candidate year span
#   5. fetch storm_metadata for those years -> resolve storms -> assert
#   6. build the file plan; estimate size; APPLY THE GUARDRAIL
#   7. fetch wind (+ zero_pairs) -> pairs
#   8. build links
#   9. derive precip/flood year spans from real window bounds; fetch and filter
#  10. determine pop years from returned data; fetch
#  11. fetch adm2/codebook/geometry/tracks as requested
#  12. finalise the manifest; construct the result
#
# The guardrail sits at step 6 and not later, because its whole purpose is to
# ask before anything large is transferred.
# ---------------------------------------------------------------------------

#' Fetch and read a single planned file
#'
#' Resolution is always on the `(directory_label, label)` pair, then download by
#' numeric id. Labels are not unique within a dataset -- `1980.parquet` exists
#' under exposures, zero_pairs, storm_metadata and storm_tracks -- so matching on
#' label alone would return the wrong file with no error.
#'
#' @param dataset `"historical"` or `"current"`.
#' @param directory_label,label File path components.
#' @param ctx Fetch context from `fetch_context()`.
#' @param col_select Columns to project.
#' @param keep Row filters for parquet and GeoParquet files,
#'   `list(<column> = <values>)`, applied on the Arrow Table before conversion
#'   (see `read_parquet_file()`).
#' @param kind `"parquet"`, `"json"` or `"geoparquet"`.
#' @param optional When `TRUE`, a missing file yields `NULL` instead of an error.
#'
#' @return The parsed object, or `NULL`.
#' @keywords internal
#' @noRd
fetch_file <- function(
  dataset,
  directory_label,
  label,
  ctx,
  col_select = NULL,
  keep = NULL,
  kind = "parquet",
  optional = FALSE
) {
  path <- file.path(directory_label, label)

  # Reproducibility path: resolve against what was recorded, not what is live.
  if (!is.null(ctx$manifest_in)) {
    row <- manifest_lookup(ctx$manifest_in, directory_label, label)
    if (is.null(row)) {
      if (optional) {
        return(NULL)
      }
      cli::cli_abort(c(
        "{.path {path}} is not recorded in the supplied {.arg manifest}.",
        i = "The manifest describes a different query. Supply the filters it
             was created with, or omit {.arg manifest}."
      ))
    }
    doi <- row$dataset_doi
    version <- row$dataset_version
    file_id <- row$file_id
    checksum <- row$checksum
    checksum_type <- row$checksum_type
    size_bytes <- row$size_bytes
  } else {
    slot <- discovery_slot(ctx$discovery, dataset)
    row <- dv_resolve_file(slot$listing, directory_label, label, doi = slot$doi)
    if (is.null(row)) {
      if (optional) {
        return(NULL)
      }
      cli::cli_abort(c(
        "{.path {path}} was not found in {.val {slot$doi}}.",
        i = "The published file layout may have changed."
      ))
    }
    doi <- slot$doi
    version <- slot$version
    file_id <- row$file_id
    checksum <- row$checksum
    checksum_type <- row$checksum_type
    size_bytes <- row$size_bytes
  }

  key <- list(
    server = ctx$discovery$server,
    doi = doi,
    version = version,
    directory_label = directory_label,
    label = label
  )

  cached <- cache_get(ctx$cache, key, expected_checksum = checksum)
  local_file <- cached$path
  downloaded_at <- cached$downloaded_at

  if (is.null(local_file)) {
    if (isTRUE(ctx$offline)) {
      cli::cli_abort(c(
        "Offline retrieval could not find {.path {path}} in the cache.",
        i = "Connect to Dataverse and run this query once, or use {.code offline = FALSE}."
      ))
    }
    inform_unless_quiet("Downloading {.path {path}}.", quiet = ctx$quiet)
    # Removed once read, unless the cache took it over.
    tmp <- cache_download_path(ctx$cache)
    on.exit(unlink(tmp), add = TRUE)
    # The listed size bounds the transfer (SEC-08).
    dv_get_file(
      file_id,
      server = ctx$discovery$server,
      quiet = ctx$quiet,
      dest = tmp,
      size_bytes = size_bytes %||% NA_real_
    )
    verify_checksum(tmp, checksum, label = path, checksum_type = checksum_type)
    stored <- cache_put(
      ctx$cache,
      key,
      tmp,
      checksum = checksum,
      quiet = ctx$notice_quiet
    )
    local_file <- stored$path
    downloaded_at <- stored$downloaded_at
    if (isTRUE(ctx$cache$enabled)) {
      ctx$cache_state$written <- TRUE
    }
    # Caching may be disabled. The timestamp still records this download.
    if (is.null(downloaded_at) || is.na(downloaded_at)) {
      downloaded_at <- Sys.time()
    }
  }

  ctx_manifest_add(
    ctx,
    manifest_entry(
      doi,
      version,
      list(
        directory_label = directory_label,
        label = label,
        file_id = file_id,
        checksum = checksum,
        checksum_type = checksum_type,
        size_bytes = as.numeric(file.size(local_file))
      ),
      downloaded_at = downloaded_at,
      source_role = dataset
    )
  )

  switch(
    kind,
    parquet = read_parquet_file(
      local_file,
      col_select = col_select,
      keep = keep,
      label = path
    ),
    json = read_json_file(local_file, label = path),
    geoparquet = read_geoparquet_file(local_file, keep = keep, label = path),
    cli::cli_abort("Unknown file kind {.val {kind}}.")
  )
}

#' Fetch a yearly table across several years and bind the results
#'
#' @param table Logical table name.
#' @param years Integer years.
#' @param ctx Fetch context.
#' @param col_select Columns to project.
#' @param keep Row filters passed to `fetch_file()`.
#' @param tag_year Add a `.gtropic_file_year` column recording the source file.
#' @param year_col Optional column name in which to record the source file year.
#'
#' @return A tibble, or `NULL` when no years were requested.
#' @keywords internal
#' @noRd
fetch_yearly <- function(
  table,
  years,
  ctx,
  col_select = NULL,
  keep = NULL,
  tag_year = FALSE,
  year_col = NULL
) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    return(NULL)
  }

  spec <- TABLE_SPEC[[table]]
  parts <- list()

  for (y in years) {
    dataset <- dataset_for_year(ctx$discovery, y)
    if (is.na(dataset)) {
      next
    }

    tbl <- fetch_file(
      dataset = dataset,
      directory_label = spec$dir,
      label = paste0(y, ".parquet"),
      ctx = ctx,
      col_select = col_select,
      keep = keep,
      optional = TRUE
    )
    if (is.null(tbl)) {
      next
    }

    df <- normalize_table_types(
      tibble::as_tibble(dplyr::collect(tbl)),
      label = file.path(spec$dir, paste0(y, ".parquet"))
    )
    # Recording the source file year is what makes the genesis-year convention
    # assertion possible at all.
    if (tag_year) {
      df$.gtropic_file_year <- y
    }
    if (!is.null(year_col)) {
      df[[year_col]] <- y
    }
    parts[[length(parts) + 1L]] <- df
  }

  if (length(parts) == 0L) {
    return(NULL)
  }
  dplyr::bind_rows(parts)
}

#' Columns of `zero_pairs` that linkage needs
#'
#' `build_links()` uses `ADM2_ID`, `STORM_ID`, `STORM_DIST_KM` and
#' `LOCAL_DATE_STORM_CLOSEST`, which `derive_local_date()` takes from
#' `LOCAL_DATETIME_STORM_CLOSEST`.
#' @keywords internal
#' @noRd
ZERO_PAIRS_LINK_COLS <- c(
  "ADM2_ID",
  "STORM_ID",
  "LOCAL_DATETIME_STORM_CLOSEST",
  "STORM_DIST_KM"
)

#' Build the shared fetch context
#'
#' @param discovery Output of `discover_datasets()`.
#' @param cache Resolved cache list.
#' @param manifest_in User-supplied manifest, or `NULL`.
#' @param quiet Suppress progress messages.
#' @param offline Use only cached files.
#' @param latest_dois DOIs retrieved at `":latest"`; their older cached
#'   versions count as outdated (see `cache_manage()`).
#' @param notice_quiet Suppress the cache's first-use notice and automatic
#'   management messages. Defaults to `quiet`; the discovery helpers, which
#'   are always quiet about progress, pass `getOption("gtropic.quiet")`.
#'
#' @return A list; `manifest_out` and `cache_state` are environments so
#'   accumulation across nested calls does not depend on returning them
#'   everywhere. With the cache enabled, `cache` carries the query's index
#'   handle (`cache_index_handle()`); whoever creates the context must run
#'   `cache_manage_ctx()` on exit, which writes the index.
#' @keywords internal
#' @noRd
fetch_context <- function(
  discovery,
  cache,
  manifest_in = NULL,
  quiet = FALSE,
  offline = FALSE,
  latest_dois = character(0),
  notice_quiet = quiet
) {
  if (isTRUE(cache$enabled)) {
    cache$index <- cache_index_handle(cache$dir)
  }
  out <- new.env(parent = emptyenv())
  out$value <- manifest_new(discovery)
  out$entries <- list()
  cache_state <- new.env(parent = emptyenv())
  cache_state$written <- FALSE

  list(
    discovery = discovery,
    cache = cache,
    manifest_in = manifest_in,
    manifest_out = out,
    cache_state = cache_state,
    quiet = quiet,
    notice_quiet = notice_quiet,
    offline = offline,
    latest_dois = latest_dois
  )
}

#' Assemble a gtropic_data result
#'
#' Runs the numbered steps in the file header in order. Steps 3-5, 7-9, 10-11
#' and 12 live in `resolve_scope()`, `fetch_hazards()`, `fetch_ancillary()` and
#' `build_result()`; the guardrail (step 6) stays here, between scope and the
#' first large transfer.
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param call The matched call, stored on the result.
#'
#' @return A `gtropic_data` object.
#' @keywords internal
#' @noRd
assemble_gtropic_data <- function(args, call = NULL) {
  quiet <- args$quiet

  # --- 1-2. options, versions, discovery ----------------------------------
  cache <- cache_resolve(args$cache, quiet = quiet)
  if (isTRUE(args$offline) && !isTRUE(cache$enabled)) {
    cli::cli_abort("{.code offline = TRUE} requires an enabled cache.")
  }

  if (!is.null(args$manifest)) {
    # A manifest pins the exact files, so version and DOI options are ignored
    # entirely -- honouring them would defeat the point.
    inform_unless_quiet(
      c(
        "Reproducing a previous retrieval from {.arg manifest}.",
        i = "{.arg version} and the DOI options are ignored."
      ),
      quiet = quiet
    )
    discovery <- discovery_from_manifest(args$manifest)
  } else {
    versions <- resolve_versions(args$version)
    discovery <- discover_datasets(
      versions,
      quiet = quiet,
      offline = args$offline,
      cache_dir = if (isTRUE(cache$enabled)) cache$dir else NULL
    )
  }

  ctx <- fetch_context(
    discovery,
    cache,
    args$manifest,
    quiet,
    offline = args$offline,
    latest_dois = if (is.null(args$manifest)) {
      latest_dois(discovery, versions)
    } else {
      character(0)
    }
  )
  # Automatic cache management (CRAN-11) runs once this retrieval has read
  # everything it needs, and never removes the files it used.
  on.exit(cache_manage_ctx(ctx), add = TRUE)

  date_spec <- parse_date_input(
    args$date_range,
    args$date_list,
    args$date_format
  )

  scope <- resolve_scope(args, ctx, date_spec)

  # --- 6. file plan and guardrail -------------------------------------------
  # Precip and flood years are unknown until the windows exist, so the estimate
  # uses the storm-year span for them. It is an estimate, and it errs high.
  provisional_years <- years_for_tables(
    storm_years = scope$storm_years,
    window_years = list(precip = scope$storm_years, flood = scope$storm_years),
    tables = args$tables,
    flood_match = args$flood_match,
    pop_years = scope$storm_years
  )

  plan <- plan_files(
    provisional_years,
    args$tables,
    discovery$year_index,
    args$geometry_resolution
  )
  plan <- attach_plan_sizes(plan, discovery)

  apply_guardrail(
    estimate_download(plan, ctx$cache, discovery),
    confirm = args$confirm,
    quiet = quiet
  )

  hazards <- fetch_hazards(args, ctx, scope, date_spec)
  ancillary <- fetch_ancillary(args, ctx, scope, hazards)
  build_result(args, ctx, scope, hazards, ancillary, date_spec, call)
}

#' Resolve the query scope: years, geography and storms (steps 3-5)
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param ctx Fetch context.
#' @param date_spec Output of `parse_date_input()`.
#'
#' @return A list with `candidate_years`, `adm2` (or `NULL`), `geo`,
#'   `storm_metadata` (tagged with `.gtropic_file_year`), `storm_ids`,
#'   `storm_years` and `warnings`.
#' @keywords internal
#' @noRd
resolve_scope <- function(args, ctx, date_spec) {
  discovery <- ctx$discovery
  warnings <- list()

  # --- 4. candidate years -----------------------------------------------------
  # A replay carries the exact metadata files scanned originally. This avoids
  # broadening its plan if the collection gains a new year after the manifest
  # was made.
  candidate_years <- if (!is.null(args$storm_metadata_years)) {
    # A manifest records the exact partitions used by the original query.
    args$storm_metadata_years
  } else {
    years <- if (identical(date_spec$type, "none")) {
      discovery$year_index$year
    } else {
      year_span(date_spec$start, date_spec$end)
    }
    if (!is.null(args$storm_name_years)) {
      # The suffix is a season. Yearly files are genesis-calendar-year
      # partitions, so season S can occur in S - 1 or S. Explicit genesis-date
      # filters remain an AND condition on those candidate partitions.
      years <- intersect(
        years,
        storm_name_genesis_years(args$storm_name_years)
      )
    }
    years
  }
  candidate_years <- intersect(candidate_years, discovery$year_index$year)

  # --- 3. geography -----------------------------------------------------------
  # ADM2 metadata is required only to resolve user-facing geography or to
  # return/clip ADM2-based metadata. An unrestricted hazard query can instead
  # derive its selected IDs from the current source files, avoiding a spurious
  # historical provenance record.
  resolve_geo <- is.null(args$resolved_adm2_ids) &&
    (length(args$geo_filters) > 0L || !is.null(args$polygon))
  need_adm2 <- resolve_geo || any(c("adm2", "geometry") %in% args$tables)
  adm2 <- NULL
  if (need_adm2) {
    adm2 <- tibble::as_tibble(dplyr::collect(fetch_file(
      "historical",
      "metadata",
      "adm2.parquet",
      ctx
    )))
  }

  # Polygon membership needs centroids, which live in the yearly pop files, so
  # the candidate year span has to be known before geography can be resolved.
  pop_by_year <- NULL
  if (resolve_geo && !is.null(args$polygon)) {
    pop_by_year <- lapply(candidate_years, function(y) {
      fetch_yearly(
        "pop",
        y,
        ctx,
        col_select = c(
          "ADM2_ID",
          "POP_CENTROID_LAT",
          "POP_CENTROID_LON"
        )
      )
    })
    names(pop_by_year) <- as.character(candidate_years)
    pop_by_year <- compact(pop_by_year)
  }

  geo <- if (!is.null(args$resolved_adm2_ids)) {
    list(
      adm2_ids = args$resolved_adm2_ids,
      inconsistent_adm2_ids = character(0),
      warnings = list()
    )
  } else if (resolve_geo) {
    resolve_geography(
      adm2 = adm2,
      pop_by_year = pop_by_year,
      filters = args$geo_filters,
      polygon = args$polygon,
      reproject = args$reproject,
      quiet = args$quiet
    )
  } else {
    list(
      adm2_ids = NULL,
      inconsistent_adm2_ids = character(0),
      warnings = list()
    )
  }
  warnings <- c(warnings, geo$warnings)

  # --- 5. storms ------------------------------------------------------------
  storm_metadata <- fetch_yearly(
    "storm_metadata",
    candidate_years,
    ctx,
    tag_year = TRUE
  )
  if (is.null(storm_metadata)) {
    gtropic_abort(
      c(
        "No storm metadata was found for year{?s} {.val {candidate_years}}.",
        i = "Check the date filters, or run {.code gtropic_years()} to see what
           is published."
      ),
      class = "gtropic_error_no_match"
    )
  }

  assertion <- assert_genesis_year_convention(storm_metadata, candidate_years)
  if (assertion$violated) {
    warnings$genesis_year_convention <-
      "Storm year-file assignment violated the genesis-year convention; the
       year scan was widened."
    candidate_years <- intersect(assertion$years, discovery$year_index$year)
    storm_metadata <- fetch_yearly(
      "storm_metadata",
      candidate_years,
      ctx,
      tag_year = TRUE
    )
  }

  excluded_lookup <- if (
    any(c("STORM_ID", "STORM_NAME") %in% names(args$storm_filters))
  ) {
    function() {
      fetch_excluded_storms(
        candidate_years,
        ctx,
        col_select = c("STORM_ID", "STORM_NAME")
      )
    }
  } else {
    NULL
  }
  storm_ids <- resolve_storms(
    storm_metadata,
    date_spec,
    args$storm_filters,
    excluded_lookup,
    warn_ambiguous_name = is.null(args$manifest)
  )
  storm_metadata <- storm_metadata[
    storm_metadata$STORM_ID %in% storm_ids,
    ,
    drop = FALSE
  ]

  list(
    candidate_years = candidate_years,
    adm2 = adm2,
    geo = geo,
    storm_metadata = storm_metadata,
    storm_ids = storm_ids,
    storm_years = sort(unique(storm_metadata$.gtropic_file_year)),
    warnings = warnings
  )
}

#' Fetch the hazard tables and build links (steps 7-9)
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param ctx Fetch context.
#' @param scope Output of `resolve_scope()`.
#' @param date_spec Output of `parse_date_input()`.
#'
#' @return A list with `wind`, `zero`, `links`, `adm2_ids` (the resolved ADM2
#'   ids, derived from the pair files when no geography filter was given),
#'   `hazard_scope`, the scoped `storm_metadata`, `scoped_storm_years`,
#'   `scoped_links`, `availability`, `precip`, `flood` and `warnings`.
#' @keywords internal
#' @noRd
fetch_hazards <- function(args, ctx, scope, date_spec) {
  storm_ids <- scope$storm_ids
  storm_years <- scope$storm_years
  adm2_ids <- scope$geo$adm2_ids
  warnings <- list()

  # --- 7. wind and zero pairs ----------------------------------------------
  # Rows are filtered on the Arrow side (PERF-02); the subsetting below is a
  # no-op safety net, kept until the pushdown has been stable for a release.
  pair_keep <- list(STORM_ID = storm_ids, ADM2_ID = adm2_ids)
  wind_cols <- NULL # full schema: users want the exposure columns
  wind_links <- fetch_yearly(
    "wind",
    storm_years,
    ctx,
    col_select = wind_cols,
    keep = pair_keep
  )

  zero <- NULL
  if (needs_zero_pairs(args$tables)) {
    # Downloaded whenever precip or flood is requested, regardless of whether
    # the user asked for the table itself. STORM_DIST_KM for far-field ADM2s
    # exists nowhere else, and those are the compound-hazard cases. Unless the
    # table is returned, only the columns build_links() uses are read.
    zero_cols <- if ("zero_pairs" %in% args$tables) {
      NULL
    } else {
      ZERO_PAIRS_LINK_COLS
    }
    zero <- fetch_yearly(
      "zero_pairs",
      storm_years,
      ctx,
      col_select = zero_cols,
      keep = pair_keep
    )
  }

  if (!is.null(wind_links)) {
    wind_links <- derive_local_date(wind_links, label = "02_wind/exposures")
    wind_links <- wind_links[wind_links$STORM_ID %in% storm_ids, , drop = FALSE]
    if (!is.null(adm2_ids)) {
      wind_links <- wind_links[wind_links$ADM2_ID %in% adm2_ids, , drop = FALSE]
    }
  }
  if (!is.null(zero)) {
    zero <- derive_local_date(zero, label = "02_wind/zero_pairs")
    zero <- zero[zero$STORM_ID %in% storm_ids, , drop = FALSE]
    if (!is.null(adm2_ids)) {
      zero <- zero[zero$ADM2_ID %in% adm2_ids, , drop = FALSE]
    }
  }
  if (is.null(adm2_ids)) {
    adm2_ids <- sort(unique(c(
      wind_links$ADM2_ID %||% character(0),
      zero$ADM2_ID %||% character(0)
    )))
  }

  # --- 8. links -------------------------------------------------------------
  # Links retain pairs needed by any requested hazard field. This is separate
  # from the returned wind table, which remains restricted to its own radius.
  link_radius_km <- link_radius_for_tables(
    args$tables,
    wind_radius_km = args$wind_radius_km,
    precip_radius_km = args$precip_radius_km,
    flood_radius_km = args$flood_radius_km
  )
  links <- build_links(
    wind = wind_links,
    zero_pairs = zero,
    adm2_ids = adm2_ids,
    storm_ids = storm_ids,
    wind_radius_km = args$wind_radius_km,
    link_radius_km = link_radius_km,
    precip_window = list(
      before = args$precip_days_before,
      after = args$precip_days_after,
      radius_km = args$precip_radius_km
    ),
    flood_window = list(
      before = args$flood_days_before,
      after = args$flood_days_after,
      radius_km = args$flood_radius_km
    )
  )

  # Wind is the one returned table narrowed by the wind radius. Zero-pair
  # records remain intact when returned, even when their links extend farther.
  wind <- filter_wind_radius(wind_links, args$wind_radius_km)

  # Apply finite requested hazard fields only after published pair links are
  # known. STORM_DIST_KM is authoritative; never infer distances from tracks.
  hazard_scope <- resolve_hazard_scope(
    links = links,
    adm2_ids = adm2_ids,
    storm_ids = storm_ids,
    tables = args$tables,
    wind_radius_km = args$wind_radius_km,
    precip_radius_km = args$precip_radius_km,
    flood_radius_km = args$flood_radius_km
  )

  # The metadata and ancillary tables describe the returned hazard scope.
  # Keep `storm_years` and `links` intact as selection/provenance diagnostics.
  storm_metadata <- scope$storm_metadata[
    scope$storm_metadata$STORM_ID %in% hazard_scope$storm_ids,
    ,
    drop = FALSE
  ]
  scoped_storm_years <- sort(unique(storm_metadata$.gtropic_file_year))
  scoped_links <- links[
    links$ADM2_ID %in%
      hazard_scope$adm2_ids &
      links$STORM_ID %in% hazard_scope$storm_ids,
    ,
    drop = FALSE
  ]

  # --- 9. precip and flood from real window bounds --------------------------
  wy <- window_years(links)
  real_years <- years_for_tables(
    storm_years = storm_years,
    window_years = wy,
    tables = args$tables,
    flood_match = args$flood_match
  )

  # Availability diagnostics describe the resolved storms and the actual
  # precip/flood windows, rather than an omitted date filter or a provisional
  # year-level estimate.
  availability <- availability_metadata(
    date_spec,
    args$tables,
    storm_metadata = storm_metadata,
    links = links
  )
  availability_messages <- availability_warnings(availability)
  warnings <- c(warnings, availability_messages)
  warn_availability(availability_messages)

  precip <- NULL
  if ("precip" %in% args$tables) {
    precip <- fetch_yearly(
      "precip",
      real_years$precip,
      ctx,
      keep = list(ADM2_ID = adm2_ids)
    )
    if (!is.null(precip)) {
      precip <- precip[precip$ADM2_ID %in% adm2_ids, , drop = FALSE]
      precip <- select_precip(links, precip)
    }

    precip_scope_ids <- unique(links$ADM2_ID[links$PRECIP_IN_FIELD %in% TRUE])
    precip_missing_ids <- precip_all_missing_adm2_ids(
      precip,
      adm2_ids = precip_scope_ids
    )
    if (length(precip_missing_ids)) {
      warnings$precip_all_missing_adm2_ids <- precip_missing_ids
      warn_precip_all_missing(precip_missing_ids)
    }
  }

  flood <- NULL
  if ("flood" %in% args$tables) {
    flood <- fetch_yearly(
      "flood",
      real_years$flood,
      ctx,
      keep = list(ADM2_ID = adm2_ids)
    )
    if (!is.null(flood)) {
      flood <- rename_flood_columns(flood)
      flood <- flood[flood$ADM2_ID %in% adm2_ids, , drop = FALSE]
      flood <- select_flood(links, flood, match = args$flood_match)
    }
    if (!is.null(flood) && nrow(flood) > 0L) {
      warnings$flood_reports <- FLOOD_REPORTS_NOTE
      warn_flood_reports()
    }
  }

  list(
    wind = wind,
    zero = zero,
    links = links,
    adm2_ids = adm2_ids,
    hazard_scope = hazard_scope,
    storm_metadata = storm_metadata,
    scoped_storm_years = scoped_storm_years,
    scoped_links = scoped_links,
    availability = availability,
    precip = precip,
    flood = flood,
    warnings = warnings
  )
}

#' Fetch population, tracks, codebook and geometry (steps 10-11)
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param ctx Fetch context.
#' @param scope Output of `resolve_scope()`.
#' @param hazards Output of `fetch_hazards()`.
#'
#' @return A list with `pop`, `tracks`, `codebook` and `geometry`, each `NULL`
#'   when not requested.
#' @keywords internal
#' @noRd
fetch_ancillary <- function(args, ctx, scope, hazards) {
  hazard_scope <- hazards$hazard_scope
  scoped_links <- hazards$scoped_links

  # --- 10. pop over the years actually returned -----------------------------
  pop <- NULL
  if ("pop" %in% args$tables) {
    returned_years <- sort(unique(c(
      hazards$scoped_storm_years,
      year_span(
        scoped_links$PRECIP_WINDOW_START,
        scoped_links$PRECIP_WINDOW_END
      ),
      year_span(scoped_links$FLOOD_WINDOW_START, scoped_links$FLOOD_WINDOW_END)
    )))
    pop <- fetch_yearly(
      "pop",
      returned_years,
      ctx,
      keep = list(ADM2_ID = hazard_scope$adm2_ids),
      year_col = "YEAR"
    )
    if (!is.null(pop)) {
      pop <- pop[pop$ADM2_ID %in% hazard_scope$adm2_ids, , drop = FALSE]
    }
  }

  # --- 11. remaining requested tables ---------------------------------------
  tracks <- NULL
  if ("tracks" %in% args$tables) {
    tracks <- fetch_yearly(
      "tracks",
      scope$storm_years,
      ctx,
      keep = list(STORM_ID = hazard_scope$storm_ids)
    )
    if (!is.null(tracks)) {
      tracks <- tracks[
        tracks$STORM_ID %in% hazard_scope$storm_ids,
        ,
        drop = FALSE
      ]
    }
  }

  codebook <- NULL
  if ("codebook" %in% args$tables) {
    codebook <- fetch_file(
      "historical",
      "metadata",
      "codebook.json",
      ctx,
      kind = "json"
    )
  }

  geometry <- NULL
  if ("geometry" %in% args$tables) {
    spec <- TABLE_SPEC[[paste0("geometry_", args$geometry_resolution)]]
    geometry <- fetch_file(
      "historical",
      spec$dir,
      spec$label,
      ctx,
      kind = "geoparquet",
      keep = list(ADM2_ID = hazard_scope$adm2_ids)
    )
    geometry <- geometry[
      geometry$ADM2_ID %in% hazard_scope$adm2_ids,
      ,
      drop = FALSE
    ]
    geometry <- repair_invalid_geometry(geometry, args$make_valid)
  }

  list(pop = pop, tracks = tracks, codebook = codebook, geometry = geometry)
}

#' Tabular result elements, in the order they are returned
#' @keywords internal
#' @noRd
RESULT_TABLES <- c(
  "wind",
  "precip",
  "flood",
  "zero_pairs",
  "tracks",
  "pop",
  "storm_metadata",
  "adm2"
)

#' Finalise the manifest and construct the result (step 12)
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param ctx Fetch context.
#' @param scope Output of `resolve_scope()`.
#' @param hazards Output of `fetch_hazards()`.
#' @param ancillary Output of `fetch_ancillary()`.
#' @param date_spec Output of `parse_date_input()`.
#' @param call The matched call, stored on the result.
#'
#' @return A `gtropic_data` object.
#' @keywords internal
#' @noRd
build_result <- function(
  args,
  ctx,
  scope,
  hazards,
  ancillary,
  date_spec,
  call
) {
  hazard_scope <- hazards$hazard_scope

  # --- 12. build the result -------------------------------------------------
  out <- list()
  for (table in intersect(RESULT_TABLES, args$tables)) {
    x <- switch(
      table,
      wind = hazards$wind,
      precip = hazards$precip,
      flood = hazards$flood,
      zero_pairs = hazards$zero,
      tracks = ancillary$tracks,
      pop = ancillary$pop,
      storm_metadata = dplyr::select(
        hazards$storm_metadata,
        -".gtropic_file_year"
      ),
      adm2 = scope$adm2[
        scope$adm2$ADM2_ID %in% hazard_scope$adm2_ids,
        ,
        drop = FALSE
      ]
    )
    out[[table]] <- finalize_table(x %||% tibble::tibble())
  }
  out <- c(
    out,
    compact(list(
      geometry = ancillary$geometry,
      codebook = ancillary$codebook,
      links = hazards$links
    ))
  )

  manifest <- manifest_finalize(
    ctx_manifest(ctx),
    resolved_query = manifest_query_from_args(
      args,
      resolved_adm2_ids = hazards$adm2_ids,
      resolved_storm_ids = scope$storm_ids,
      storm_metadata_years = scope$candidate_years
    ),
    availability = hazards$availability
  )

  new_gtropic_data(
    tables = out,
    manifest = manifest,
    query = call,
    filters_resolved = list(
      adm2_ids = hazard_scope$adm2_ids,
      storm_ids = hazard_scope$storm_ids,
      date_start = date_spec$start,
      date_end = date_spec$end,
      date_order = date_spec$inferred_order,
      years_scanned = scope$storm_years,
      wind_radius_km = args$wind_radius_km,
      precip_days_before = args$precip_days_before,
      precip_days_after = args$precip_days_after,
      precip_radius_km = args$precip_radius_km,
      flood_days_before = args$flood_days_before,
      flood_days_after = args$flood_days_after,
      flood_radius_km = args$flood_radius_km,
      flood_match = args$flood_match,
      inconsistent_adm2_ids = scope$geo$inconsistent_adm2_ids
    ),
    warnings = c(scope$warnings, hazards$warnings),
    availability = hazards$availability,
    retrieved_at = manifest$retrieved_at
  )
}

#' Join published file sizes onto a file plan
#'
#' Sizes come from the Dataverse listing, which we already hold, so the
#' guardrail costs no extra requests.
#'
#' @param plan Output of `plan_files()`.
#' @param discovery Output of `discover_datasets()`.
#'
#' @return `plan` with a `size_bytes` column.
#' @keywords internal
#' @noRd
attach_plan_sizes <- function(plan, discovery) {
  if (nrow(plan) == 0L) {
    plan$size_bytes <- numeric(0)
    return(plan)
  }

  key <- c("dataset", "directory_label", "label")
  sizes <- dplyr::left_join(
    plan[key],
    discovery_files(discovery)[c(key, "size_bytes")],
    by = key,
    relationship = "many-to-one"
  )
  plan$size_bytes <- sizes$size_bytes
  plan
}

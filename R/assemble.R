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
#' @param kind `"parquet"`, `"json"` or `"geoparquet"`.
#' @param optional When `TRUE`, a missing file yields `NULL` instead of an error.
#'
#' @return The parsed object, or `NULL`.
#' @keywords internal
#' @noRd
fetch_file <- function(dataset, directory_label, label, ctx,
                        col_select = NULL, kind = "parquet",
                        optional = FALSE) {
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
  }

  key <- list(
    server = ctx$discovery$server, doi = doi, version = version,
    directory_label = directory_label, label = label
  )

  cached <- cache_get_record(ctx$cache, key, expected_checksum = checksum)
  raw <- if (is.null(cached)) NULL else cached$raw
  downloaded_at <- if (is.null(cached)) NULL else cached$downloaded_at

  if (is.null(raw)) {
    if (isTRUE(ctx$offline)) {
      cli::cli_abort(c(
        "Offline retrieval could not find {.path {path}} in the cache.",
        i = "Connect to Dataverse and run this query once, or use {.code offline = FALSE}."
      ))
    }
    inform_unless_quiet("Downloading {.path {path}}.", quiet = ctx$quiet)
    raw <- dv_get_file(file_id,
      server = ctx$discovery$server,
      quiet = ctx$quiet
    )
    verify_checksum(raw, checksum, label = path, checksum_type = checksum_type)
    downloaded_at <- cache_put(ctx$cache, key, raw, checksum = checksum)
    # Caching may be disabled. The timestamp still records this download.
    if (is.null(downloaded_at) || is.na(downloaded_at)) downloaded_at <- Sys.time()
  }

  ctx$manifest_out$value <- manifest_add(
    ctx$manifest_out$value, doi, version,
    tibble::tibble(
      directory_label = directory_label, label = label,
       file_id = file_id, checksum = checksum,
      checksum_type = checksum_type,
      size_bytes = as.numeric(length(raw))
    ), downloaded_at = downloaded_at, source_role = dataset
  )

  switch(kind,
    parquet = read_parquet_raw(raw, col_select = col_select, label = path),
    json = read_json_raw(raw, label = path),
    geoparquet = read_geoparquet_raw(raw, label = path),
    cli::cli_abort("Unknown file kind {.val {kind}}.")
  )
}

#' Fetch a yearly table across several years and bind the results
#'
#' @param table Logical table name.
#' @param years Integer years.
#' @param ctx Fetch context.
#' @param col_select Columns to project.
#' @param tag_year Add a `.gtropic_file_year` column recording the source file.
#' @param year_col Optional column name in which to record the source file year.
#'
#' @return A tibble, or `NULL` when no years were requested.
#' @keywords internal
#' @noRd
fetch_yearly <- function(table, years, ctx, col_select = NULL,
                         tag_year = FALSE, year_col = NULL) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    return(NULL)
  }

  spec <- TABLE_SPEC[[table]]
  parts <- list()

  for (y in years) {
    dataset <- dataset_for_year(ctx$discovery, y)
    if (is.na(dataset)) next

    tbl <- fetch_file(
      dataset = dataset,
      directory_label = spec$dir,
      label = paste0(y, ".parquet"),
      ctx = ctx,
      col_select = col_select,
      optional = TRUE
    )
    if (is.null(tbl)) next

    df <- tibble::as_tibble(dplyr::collect(tbl))
    # Recording the source file year is what makes the genesis-year convention
    # assertion possible at all.
    if (tag_year) df$.gtropic_file_year <- y
    if (!is.null(year_col)) df[[year_col]] <- y
    parts[[length(parts) + 1L]] <- df
  }

  if (length(parts) == 0L) {
    return(NULL)
  }
  dplyr::bind_rows(parts)
}

#' Build the shared fetch context
#'
#' @param discovery Output of `discover_datasets()`.
#' @param cache Resolved cache list.
#' @param manifest_in User-supplied manifest, or `NULL`.
#' @param quiet Suppress informational output.
#'
#' @return A list; `manifest_out` is an environment so accumulation across
#'   nested calls does not depend on returning it everywhere.
#' @keywords internal
#' @noRd
fetch_context <- function(discovery, cache, manifest_in = NULL, quiet = FALSE,
                          offline = FALSE) {
  out <- new.env(parent = emptyenv())
  out$value <- manifest_new(discovery)

  list(
    discovery = discovery,
    cache = cache,
    manifest_in = manifest_in,
    manifest_out = out,
    quiet = quiet,
    offline = offline
  )
}

#' Assemble a gtropic_data result
#'
#' @param args Validated argument list from `gtropic_data()`.
#' @param call The matched call, stored on the result.
#'
#' @return A `gtropic_data` object.
#' @keywords internal
#' @noRd
assemble_gtropic_data <- function(args, call = NULL) {
  quiet <- args$quiet
  warnings <- list()

  # --- 1-2. options, versions, discovery ----------------------------------
  cache <- cache_resolve(args$cache, quiet = quiet)
  if (isTRUE(args$offline) && !isTRUE(cache$enabled)) {
    cli::cli_abort("{.code offline = TRUE} requires an enabled cache.")
  }

  if (!is.null(args$manifest)) {
    # A manifest pins the exact files, so version and DOI options are ignored
    # entirely -- honouring them would defeat the point.
    inform_unless_quiet(
      c("Reproducing a previous retrieval from {.arg manifest}.",
        i = "{.arg version} and the DOI options are ignored."
      ),
      quiet = quiet
    )
    discovery <- discovery_from_manifest(args$manifest)
  } else {
    versions <- resolve_versions(args$version)
    discovery <- discover_datasets(
      versions, quiet = quiet, offline = args$offline,
      cache_dir = if (isTRUE(cache$enabled)) cache$dir else NULL
    )
  }

  ctx <- fetch_context(discovery, cache, args$manifest, quiet,
    offline = args$offline
  )

  date_spec <- parse_date_input(
    args$date_range, args$date_list,
    args$date_format
  )

  # A replay carries the exact metadata files scanned originally. This avoids
  # broadening its plan if the collection gains a new year after the manifest
  # was made.
  candidate_years <- if (!is.null(args$storm_metadata_years)) {
    args$storm_metadata_years
  } else if (identical(date_spec$type, "none")) {
    discovery$year_index$year
  } else {
    year_span(date_spec$start, date_spec$end)
  }
  if (!is.null(args$storm_name_years)) {
    # Name suffixes safely identify the only metadata files that can contain
    # these storms. resolve_storms() still applies date filters afterwards.
    candidate_years <- args$storm_name_years
  }
  candidate_years <- intersect(candidate_years, discovery$year_index$year)

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
      "historical", "metadata", "adm2.parquet", ctx
    )))
  }

  # Polygon membership needs centroids, which live in the yearly pop files, so
  # the candidate year span has to be known before geography can be resolved.
  pop_by_year <- NULL
  if (resolve_geo && !is.null(args$polygon)) {
    pop_by_year <- lapply(candidate_years, function(y) {
      fetch_yearly("pop", y, ctx,
        col_select = c(
          "ADM2_ID", "POP_CENTROID_LAT",
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
      inconsistent_adm2_ids = character(0), warnings = list()
    )
  } else if (resolve_geo) {
    resolve_geography(
      adm2 = adm2,
      pop_by_year = pop_by_year,
      filters = args$geo_filters,
      polygon = args$polygon,
      reproject = args$reproject
    )
  } else {
    list(adm2_ids = NULL, inconsistent_adm2_ids = character(0), warnings = list())
  }
  warnings <- c(warnings, geo$warnings)

  # --- 5. storms ------------------------------------------------------------
  storm_metadata <- fetch_yearly("storm_metadata", candidate_years, ctx,
    tag_year = TRUE
  )
  if (is.null(storm_metadata)) {
    cli::cli_abort(c(
      "No storm metadata was found for year{?s} {.val {candidate_years}}.",
      i = "Check the date filters, or run {.code gtropic_years()} to see what
           is published."
    ))
  }

  assertion <- assert_genesis_year_convention(storm_metadata, candidate_years)
  if (assertion$violated) {
    warnings$genesis_year_convention <-
      "Storm year-file assignment violated the genesis-year convention; the
       year scan was widened."
    candidate_years <- intersect(assertion$years, discovery$year_index$year)
    storm_metadata <- fetch_yearly("storm_metadata", candidate_years, ctx,
      tag_year = TRUE
    )
  }

  excluded_lookup <- if (any(c("STORM_ID", "STORM_NAME") %in% names(args$storm_filters))) {
    function() {
      fetch_excluded_storms(candidate_years, ctx,
        col_select = c("STORM_ID", "STORM_NAME")
      )
    }
  } else {
    NULL
  }
  storm_ids <- resolve_storms(
    storm_metadata, date_spec, args$storm_filters, excluded_lookup
  )
  storm_metadata <- storm_metadata[storm_metadata$STORM_ID %in% storm_ids, ,
    drop = FALSE
  ]
  storm_years <- sort(unique(storm_metadata$.gtropic_file_year))

  # --- 6. file plan and guardrail -------------------------------------------
  # Precip and flood years are unknown until the windows exist, so the estimate
  # uses the storm-year span for them. It is an estimate, and it errs high.
  provisional_years <- years_for_tables(
    storm_years = storm_years,
    window_years = list(precip = storm_years, flood = storm_years),
    tables = args$tables,
    flood_match = args$flood_match,
    pop_years = storm_years
  )

  plan <- plan_files(
    provisional_years, args$tables, discovery$year_index,
    args$geometry_resolution
  )
  plan <- attach_plan_sizes(plan, discovery)

  apply_guardrail(estimate_download(plan, cache, discovery),
    confirm = args$confirm, quiet = quiet
  )

  # --- 7. wind and zero pairs ----------------------------------------------
  wind_cols <- NULL # full schema: users want the exposure columns
  wind_links <- fetch_yearly("wind", storm_years, ctx, col_select = wind_cols)

  zero <- NULL
  if (needs_zero_pairs(args$tables)) {
    # Downloaded whenever precip or flood is requested, regardless of whether
    # the user asked for the table itself. STORM_DIST_KM for far-field ADM2s
    # exists nowhere else, and those are the compound-hazard cases.
    zero <- fetch_yearly("zero_pairs", storm_years, ctx)
  }

  if (!is.null(wind_links)) {
    wind_links <- derive_local_date(wind_links, label = "02_wind/exposures")
    wind_links <- wind_links[wind_links$STORM_ID %in% storm_ids, , drop = FALSE]
    if (!is.null(geo$adm2_ids)) {
      wind_links <- wind_links[wind_links$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
    }
  }
  if (!is.null(zero)) {
    zero <- derive_local_date(zero, label = "02_wind/zero_pairs")
    zero <- zero[zero$STORM_ID %in% storm_ids, , drop = FALSE]
    if (!is.null(geo$adm2_ids)) {
      zero <- zero[zero$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
    }
  }
  if (is.null(geo$adm2_ids)) {
    geo$adm2_ids <- sort(unique(c(
      wind_links$ADM2_ID %||% character(0), zero$ADM2_ID %||% character(0)
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
    adm2_ids = geo$adm2_ids,
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
    adm2_ids = geo$adm2_ids,
    storm_ids = storm_ids,
    tables = args$tables,
    wind_radius_km = args$wind_radius_km,
    precip_radius_km = args$precip_radius_km,
    flood_radius_km = args$flood_radius_km
  )

  # The metadata and ancillary tables describe the returned hazard scope.
  # Keep `storm_years` and `links` intact as selection/provenance diagnostics.
  storm_metadata <- storm_metadata[
    storm_metadata$STORM_ID %in% hazard_scope$storm_ids,
    , drop = FALSE
  ]
  scoped_storm_years <- sort(unique(storm_metadata$.gtropic_file_year))
  scoped_links <- links[
    links$ADM2_ID %in% hazard_scope$adm2_ids &
      links$STORM_ID %in% hazard_scope$storm_ids,
    , drop = FALSE
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
    date_spec, args$tables, storm_metadata = storm_metadata, links = links
  )
  availability_messages <- availability_warnings(availability)
  warnings <- c(warnings, availability_messages)
  for (message in unname(availability_messages)) cli::cli_warn(message)

  precip <- NULL
  if ("precip" %in% args$tables) {
    precip <- fetch_yearly("precip", real_years$precip, ctx)
    if (!is.null(precip)) {
      precip <- precip[precip$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
      precip <- select_precip(links, precip)
    }
  }

  flood <- NULL
  if ("flood" %in% args$tables) {
    flood <- fetch_yearly("flood", real_years$flood, ctx)
    if (!is.null(flood)) {
      flood <- flood[flood$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
      flood <- select_flood(links, flood, match = args$flood_match)
    }
  }

  # --- 10. pop over the years actually returned -----------------------------
  pop <- NULL
  if ("pop" %in% args$tables) {
    returned_years <- sort(unique(c(
      scoped_storm_years,
      year_span(scoped_links$PRECIP_WINDOW_START, scoped_links$PRECIP_WINDOW_END),
      year_span(scoped_links$FLOOD_WINDOW_START, scoped_links$FLOOD_WINDOW_END)
    )))
    pop <- fetch_yearly("pop", returned_years, ctx, year_col = "YEAR")
    if (!is.null(pop)) {
      pop <- pop[pop$ADM2_ID %in% hazard_scope$adm2_ids, , drop = FALSE]
    }
  }

  # --- 11. remaining requested tables ---------------------------------------
  tracks <- NULL
  if ("tracks" %in% args$tables) {
    tracks <- fetch_yearly("tracks", storm_years, ctx)
    if (!is.null(tracks)) {
      tracks <- tracks[tracks$STORM_ID %in% hazard_scope$storm_ids, , drop = FALSE]
    }
  }

  codebook <- NULL
  if ("codebook" %in% args$tables) {
    codebook <- fetch_file("historical", "metadata", "codebook.json", ctx,
      kind = "json"
    )
  }

  geometry <- NULL
  if ("geometry" %in% args$tables) {
    spec <- TABLE_SPEC[[paste0("geometry_", args$geometry_resolution)]]
    geometry <- fetch_file("historical", spec$dir, spec$label, ctx,
      kind = "geoparquet"
    )
    geometry <- geometry[geometry$ADM2_ID %in% hazard_scope$adm2_ids, , drop = FALSE]
  }

  # --- 12. build the result -------------------------------------------------
  out <- compact(list(
    wind = if ("wind" %in% args$tables) {
      finalize_table(wind %||%
        tibble::tibble(), args$as)
    },
    precip = if ("precip" %in% args$tables) {
      finalize_table(precip %||%
        tibble::tibble(), args$as)
    },
    flood = if ("flood" %in% args$tables) {
      finalize_table(flood %||%
        tibble::tibble(), args$as)
    },
    zero_pairs = if ("zero_pairs" %in% args$tables) {
      finalize_table(zero %||%
        tibble::tibble(), args$as)
    },
    tracks = if ("tracks" %in% args$tables) {
      finalize_table(tracks %||%
        tibble::tibble(), args$as)
    },
    pop = if ("pop" %in% args$tables) {
      finalize_table(pop %||%
        tibble::tibble(), args$as)
    },
    storm_metadata = if ("storm_metadata" %in% args$tables) {
      finalize_table(
        dplyr::select(storm_metadata, -".gtropic_file_year"),
        args$as
      )
    },
    adm2 = if ("adm2" %in% args$tables) {
      finalize_table(
        adm2[adm2$ADM2_ID %in% hazard_scope$adm2_ids, , drop = FALSE],
        args$as
      )
    },
    geometry = geometry,
    codebook = codebook,
    links = links
  ))

  manifest <- manifest_finalize(
    ctx$manifest_out$value,
    resolved_query = manifest_query_from_args(
      args,
      resolved_adm2_ids = geo$adm2_ids,
      resolved_storm_ids = storm_ids,
      storm_metadata_years = candidate_years
    ),
    availability = availability
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
      years_scanned = storm_years,
      wind_radius_km = args$wind_radius_km,
      precip_days_before = args$precip_days_before,
      precip_days_after = args$precip_days_after,
      precip_radius_km = args$precip_radius_km,
      flood_days_before = args$flood_days_before,
      flood_days_after = args$flood_days_after,
      flood_radius_km = args$flood_radius_km,
      flood_match = args$flood_match,
      inconsistent_adm2_ids = geo$inconsistent_adm2_ids
    ),
    warnings = warnings,
    availability = availability,
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

  plan$size_bytes <- vapply(seq_len(nrow(plan)), function(i) {
    slot <- discovery[[plan$dataset[i]]]
    if (is.null(slot)) {
      return(NA_real_)
    }
    row <- slot$listing[
      slot$listing$directory_label == plan$directory_label[i] &
        slot$listing$label == plan$label[i], ,
      drop = FALSE
    ]
    if (nrow(row) == 0L) NA_real_ else as.numeric(row$size_bytes[1])
  }, numeric(1))

  plan
}

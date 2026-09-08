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
  slot <- discovery_slot(ctx$discovery, dataset)
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
    md5 <- row$md5
  } else {
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
    md5 <- row$md5
  }

  key <- list(
    server = ctx$discovery$server, doi = doi, version = version,
    directory_label = directory_label, label = label
  )

  raw <- cache_get(ctx$cache, key, expected_md5 = md5)

  if (is.null(raw)) {
    inform_unless_quiet("Downloading {.path {path}}.", quiet = ctx$quiet)
    raw <- dv_get_file(file_id,
      server = ctx$discovery$server,
      quiet = ctx$quiet
    )
    verify_md5(raw, md5, label = path)
    cache_put(ctx$cache, key, raw, md5 = md5)
  }

  ctx$manifest_out$value <- manifest_add(
    ctx$manifest_out$value, doi, version,
    tibble::tibble(
      directory_label = directory_label, label = label,
      file_id = file_id, md5 = md5,
      size_bytes = as.numeric(length(raw))
    )
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
fetch_context <- function(discovery, cache, manifest_in = NULL, quiet = FALSE) {
  out <- new.env(parent = emptyenv())
  out$value <- manifest_new(discovery)

  list(
    discovery = discovery,
    cache = cache,
    manifest_in = manifest_in,
    manifest_out = out,
    quiet = quiet
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
  versions <- resolve_versions(args$version)
  cache <- cache_resolve(args$cache, quiet = quiet)

  if (!is.null(args$manifest)) {
    manifest_validate(args$manifest)
    # A manifest pins the exact files, so version and DOI options are ignored
    # entirely -- honouring them would defeat the point.
    inform_unless_quiet(
      c("Reproducing a previous retrieval from {.arg manifest}.",
        i = "{.arg version} and the DOI options are ignored."
      ),
      quiet = quiet
    )
    versions <- list(
      historical = args$manifest$historical_version,
      current = args$manifest$current_version
    )
  }

  discovery <- discover_datasets(versions, quiet = quiet)
  ctx <- fetch_context(discovery, cache, args$manifest, quiet)

  # --- 3. adm2 and geography ----------------------------------------------
  adm2 <- tibble::as_tibble(dplyr::collect(fetch_file(
    "historical", "metadata", "adm2.parquet", ctx
  )))

  date_spec <- parse_date_input(
    args$date_range, args$date_list,
    args$date_format
  )

  # Polygon membership needs centroids, which live in the yearly pop files, so
  # the candidate year span has to be known before geography can be resolved.
  candidate_years <- if (identical(date_spec$type, "none")) {
    discovery$year_index$year
  } else {
    year_span(date_spec$start, date_spec$end)
  }
  candidate_years <- intersect(candidate_years, discovery$year_index$year)

  pop_by_year <- NULL
  if (!is.null(args$polygon)) {
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

  geo <- resolve_geography(
    adm2 = adm2,
    pop_by_year = pop_by_year,
    filters = args$geo_filters,
    polygon = args$polygon,
    reproject = args$reproject
  )
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

  storm_ids <- resolve_storms(storm_metadata, date_spec, args$storm_filters)
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
  wind <- fetch_yearly("wind", storm_years, ctx, col_select = wind_cols)

  zero <- NULL
  if (needs_zero_pairs(args$tables)) {
    # Downloaded whenever precip or flood is requested, regardless of whether
    # the user asked for the table itself. STORM_DIST_KM for far-field ADM2s
    # exists nowhere else, and those are the compound-hazard cases.
    zero <- fetch_yearly("zero_pairs", storm_years, ctx)
  }

  if (!is.null(wind)) {
    wind <- derive_local_date(wind, label = "02_wind/exposures")
    wind <- wind[wind$ADM2_ID %in% geo$adm2_ids &
      wind$STORM_ID %in% storm_ids, , drop = FALSE]
  }
  if (!is.null(zero)) {
    zero <- derive_local_date(zero, label = "02_wind/zero_pairs")
    zero <- zero[zero$ADM2_ID %in% geo$adm2_ids &
      zero$STORM_ID %in% storm_ids, , drop = FALSE]
  }

  # --- 8. links -------------------------------------------------------------
  links <- build_links(
    wind = wind,
    zero_pairs = zero,
    adm2_ids = geo$adm2_ids,
    storm_ids = storm_ids,
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

  # --- 9. precip and flood from real window bounds --------------------------
  wy <- window_years(links)
  real_years <- years_for_tables(
    storm_years = storm_years,
    window_years = wy,
    tables = args$tables,
    flood_match = args$flood_match
  )

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
      storm_years,
      year_span(links$PRECIP_WINDOW_START, links$PRECIP_WINDOW_END),
      year_span(links$FLOOD_WINDOW_START, links$FLOOD_WINDOW_END)
    )))
    pop <- fetch_yearly("pop", returned_years, ctx, year_col = "YEAR")
    if (!is.null(pop)) pop <- pop[pop$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
  }

  # --- 11. remaining requested tables ---------------------------------------
  tracks <- NULL
  if ("tracks" %in% args$tables) {
    tracks <- fetch_yearly("tracks", storm_years, ctx)
    if (!is.null(tracks)) {
      tracks <- tracks[tracks$STORM_ID %in% storm_ids, , drop = FALSE]
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
    geometry <- geometry[geometry$ADM2_ID %in% geo$adm2_ids, , drop = FALSE]
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
        adm2[adm2$ADM2_ID %in% geo$adm2_ids, , drop = FALSE],
        args$as
      )
    },
    geometry = geometry,
    codebook = codebook,
    links = links
  ))

  new_gtropic_data(
    tables = out,
    manifest = manifest_finalize(ctx$manifest_out$value),
    query = call,
    filters_resolved = list(
      adm2_ids = geo$adm2_ids,
      storm_ids = storm_ids,
      date_start = date_spec$start,
      date_end = date_spec$end,
      date_order = date_spec$inferred_order,
      years_scanned = storm_years,
      precip_days_before = args$precip_days_before,
      precip_days_after = args$precip_days_after,
      precip_radius_km = args$precip_radius_km,
      flood_days_before = args$flood_days_before,
      flood_days_after = args$flood_days_after,
      flood_radius_km = args$flood_radius_km,
      flood_match = args$flood_match,
      inconsistent_adm2_ids = geo$inconsistent_adm2_ids
    ),
    warnings = warnings
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

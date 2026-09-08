# ---------------------------------------------------------------------------
# Geographic resolution.
#
# Rule: OR within the vector passed to one argument, AND across arguments,
# polygon included. This mirrors dplyr::filter() and SQL WHERE, which is what
# users predict. ADM2_NAME = "San Jose" AND ADM2_GROUP = "CRI" means the San
# Jose that is in Costa Rica, not every San Jose plus every Costa Rican unit.
#
# An empty intersection is an error, never an empty result: an empty tibble
# looks like a finding, and a user who mistyped a country code should not be
# left concluding that no storms hit it.
# ---------------------------------------------------------------------------

#' Resolve geographic filters to a set of ADM2 identifiers
#'
#' @param adm2 The `adm2` table, materialised as a data frame.
#' @param pop_by_year Named list of `pop` tables keyed by year, needed only when
#'   `polygon` is supplied. Centroids live in the yearly files and move.
#' @param filters Named list of the named geographic filter arguments.
#' @param polygon An `sf`/`sfc` object, or `NULL`.
#' @param reproject Whether to transform `polygon` to EPSG:4326.
#' @param call Calling environment for error reporting.
#'
#' @return A list with `adm2_ids`, `inconsistent_adm2_ids` and `warnings`.
#' @keywords internal
#' @noRd
resolve_geography <- function(adm2,
                              pop_by_year = NULL,
                              filters = list(),
                              polygon = NULL,
                              reproject = TRUE,
                              call = rlang::caller_env()) {
  filters <- compact(filters)
  warnings <- list()

  keep <- rep(TRUE, nrow(adm2))
  used_args <- character(0)

  # --- named filters -------------------------------------------------------
  spec <- list(
    ADM2_ID = list(column = "ADM2_ID", exact = TRUE),
    ADM2_NAME = list(column = "ADM2_NAME", exact = FALSE),
    ADM2_GROUP = list(column = "ADM2_GROUP", exact = FALSE),
    WHO_ENTITY = list(column = "WHO_ENTITY", exact = FALSE),
    WHO_REGION = list(column = "WHO_REGION", exact = FALSE)
  )

  for (arg in names(spec)) {
    values <- filters[[arg]]
    if (is.null(values)) next

    column <- spec[[arg]]$column
    if (!column %in% names(adm2)) {
      cli::cli_abort(
        c("Column {.field {column}} is not present in the published
           {.field adm2} table.",
          i = "The dataset schema may have changed."
        ),
        call = call
      )
    }

    canonical <- match_values(
      values,
      candidates = adm2[[column]],
      arg = arg,
      exact = spec[[arg]]$exact,
      helper = "gtropic_adm2()",
      what = "ADM2 unit",
      call = call
    )

    # OR within the argument; AND across, by successive intersection of `keep`.
    hit <- if (spec[[arg]]$exact) {
      adm2[[column]] %in% canonical
    } else {
      norm_key(adm2[[column]]) %in% norm_key(canonical)
    }

    keep <- keep & hit
    used_args <- c(used_args, arg)
  }

  if (!any(keep)) abort_empty_intersection(used_args, call = call)

  adm2_ids <- adm2$ADM2_ID[keep]

  # --- polygon -------------------------------------------------------------
  inconsistent <- character(0)

  if (!is.null(polygon)) {
    poly_result <- resolve_polygon(
      polygon = polygon,
      pop_by_year = pop_by_year,
      candidate_ids = adm2_ids,
      reproject = reproject,
      call = call
    )
    inconsistent <- poly_result$inconsistent
    warnings <- c(warnings, poly_result$warnings)

    adm2_ids <- intersect(adm2_ids, poly_result$adm2_ids)
    used_args <- c(used_args, "polygon")

    if (length(adm2_ids) == 0L) abort_empty_intersection(used_args, call = call)
  }

  list(
    adm2_ids = unique(adm2_ids),
    inconsistent_adm2_ids = inconsistent,
    warnings = warnings
  )
}

#' Resolve polygon membership across years
#'
#' `POP_CENTROID_LAT`/`LON` live in the yearly `pop` files and move over time,
#' so membership in a polygon is year-dependent. The rule is inclusive: an ADM2
#' is in if its centroid falls inside the polygon in *any* year the query spans.
#' Anything else would make a unit blink in and out of a time series.
#'
#' @param polygon `sf` or `sfc` object.
#' @param pop_by_year Named list of yearly `pop` tables.
#' @param candidate_ids ADM2 ids surviving the named filters.
#' @param reproject Whether to transform to EPSG:4326.
#' @param call Calling environment.
#'
#' @return A list with `adm2_ids`, `inconsistent` and `warnings`.
#' @keywords internal
#' @noRd
resolve_polygon <- function(polygon,
                            pop_by_year,
                            candidate_ids,
                            reproject = TRUE,
                            call = rlang::caller_env()) {
  check_installed_geo(what = "The {.arg polygon} filter", call = call)

  if (!inherits(polygon, c("sf", "sfc"))) {
    cli::cli_abort(
      c("{.arg polygon} must be an {.cls sf} or {.cls sfc} object.",
        x = "Got {.cls {class(polygon)[1]}}.",
        i = "Read a shapefile or GeoPackage with {.code sf::st_read()}."
      ),
      call = call
    )
  }

  crs <- sf::st_crs(polygon)
  if (is.na(crs)) {
    cli::cli_abort(
      c("{.arg polygon} has no coordinate reference system.",
        i = "Set one with {.code sf::st_set_crs()}; without it there is no way
             to know what the coordinates mean.",
        i = "The published centroids are in EPSG:4326 (longitude/latitude)."
      ),
      call = call
    )
  }

  if (crs != sf::st_crs(4326)) {
    if (!isTRUE(reproject)) {
      cli::cli_abort(
        c("{.arg polygon} is not in EPSG:4326 and {.arg reproject} is
           {.code FALSE}.",
          i = "Set {.code reproject = TRUE} to transform it automatically, or
               transform it yourself with {.code sf::st_transform(4326)}."
        ),
        call = call
      )
    }
    polygon <- sf::st_transform(polygon, 4326)
  }

  if (is.null(pop_by_year) || length(pop_by_year) == 0L) {
    cli::cli_abort(
      c("Polygon membership needs population centroids, but no {.field pop}
         data was loaded.",
        i = "This is an internal error; please report it."
      ),
      call = call
    )
  }

  years <- names(pop_by_year)
  warnings <- list()

  membership <- lapply(years, function(y) {
    pop <- pop_by_year[[y]]
    pop <- pop[pop$ADM2_ID %in% candidate_ids, , drop = FALSE]
    if (nrow(pop) == 0L) {
      return(character(0))
    }

    pts <- sf::st_as_sf(
      pop,
      coords = c("POP_CENTROID_LON", "POP_CENTROID_LAT"),
      crs = 4326,
      remove = FALSE
    )
    inside <- lengths(sf::st_intersects(pts, polygon)) > 0L
    as.character(pop$ADM2_ID[inside])
  })
  names(membership) <- years

  all_in <- unique(unlist(membership, use.names = FALSE))
  always_in <- Reduce(intersect, membership)
  inconsistent <- setdiff(all_in, always_in %||% character(0))

  if (length(years) > 1L) {
    # Warn on every multi-year polygon query, not only when membership actually
    # varies, so users understand that the possibility exists at all.
    warnings$polygon_multiyear <- glue::glue(
      "Polygon membership was evaluated across {length(years)} years; ",
      "population-weighted centroids move between years."
    )
    cli::cli_warn(c(
      "Polygon membership is year-dependent.",
      i = "Centroids were evaluated for {length(years)} year{?s}
           ({years[1]}-{years[length(years)]}); an ADM2 unit is included if its
           centroid fell inside the polygon in any of them."
    ))
  }

  if (length(inconsistent) > 0L) {
    warnings$polygon_inconsistent <- inconsistent
    cli::cli_warn(c(
      "{length(inconsistent)} ADM2 unit{?s} moved in or out of the polygon
       across years.",
      "!" = "{truncate_display(inconsistent)}",
      i = "{?It was/They were} included. The full vector is available via
           {.code gtropic_warnings()}."
    ))
  }

  list(adm2_ids = all_in, inconsistent = inconsistent, warnings = warnings)
}

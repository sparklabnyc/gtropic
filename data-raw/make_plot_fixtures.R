# Build the small synthetic example object returned by gtropic_example().
#
# Run from the repository root with: Rscript data-raw/make_plot_fixtures.R
#
# Writes inst/extdata/gtropic-example.rds, a complete `gtropic_data` object
# (tables, manifest, resolved filters, availability) built from invented values.
# It is the single source of truth for offline examples and for the plotting
# tests (tests/testthat/helper-plot.R reads it through gtropic_example()).
#
# Everything is obviously fake: the server is `example.invalid`, the DOI is
# `doi:10.0000/EXAMPLE`, places are "Example Coast" and "Example Pacific".
# Geometry is stored as WKT text so the object can be read without `sf`;
# gtropic_example() rebuilds the `sf` table when `sf` is installed.
# Budget: < 20 KB (hard limit 50 KB, Plan 06 CRAN-07).
rlang::check_installed(c("pkgload", "sf", "tibble", "dplyr"))
pkgload::load_all(".", quiet = TRUE)

local({
  adm2 <- tibble::tibble(
    ADM2_ID = paste0("PLOT.", 1:6),
    ADM2_NAME = c(
      "Coast",
      "Inland",
      "North",
      "South",
      "Date West",
      "Date East"
    ),
    ADM2_GROUP = rep(c("REGIONAL", "PACIFIC"), c(4, 2)),
    WHO_ENTITY = rep(c("Example Coast", "Example Pacific"), c(4, 2)),
    WHO_REGION = rep(c("AMRO", "WPRO"), c(4, 2))
  )
  square <- function(x, y) {
    lon <- c(x, x + 1, x + 1, x, x)
    lon[lon > 180] <- lon[lon > 180] - 360
    sf::st_polygon(list(cbind(lon, c(y, y, y + 1, y + 1, y))))
  }
  geometry <- tibble::tibble(
    ADM2_ID = adm2$ADM2_ID,
    geometry_wkt = sf::st_as_text(sf::st_sfc(
      square(-81, 25),
      square(-80, 25),
      square(-81, 26),
      square(-80, 24),
      square(179.5, -17),
      square(179.5, -15)
    ))
  )
  storm_metadata <- tibble::tibble(
    STORM_ID = c("PLOT_A", "PLOT_B", "PLOT_C"),
    STORM_NAME = c("Alpha", "Beta", "Dateline"),
    USA_ATCF_ID = c("AL012020", "AL022020", "SP012020"),
    STORM_GENESIS_DATE_UTC = c("2020-08-01", "2020-08-02", "2020-08-03")
  )
  pairs <- tibble::as_tibble(expand.grid(
    ADM2_ID = adm2$ADM2_ID,
    STORM_ID = storm_metadata$STORM_ID,
    stringsAsFactors = FALSE
  ))
  dates <- rep(c("2020-08-04", "2020-08-05", "2020-08-06"), each = 6)
  offsets <- rep(c(rep("-05:00", 4), "+12:00", "-12:00"), 3)
  pairs$LOCAL_DATETIME_STORM_CLOSEST <- paste0(dates, "T23:00:00", offsets)
  pairs$STORM_DIST_KM <- rep(c(10, 80, 200, 550, 90, 300), 3)
  pairs$LOCAL_DATE_STORM_CLOSEST <- dates
  wind <- pairs[-c(4, 10, 16), ]
  wind$LOCAL_DATETIME_MAX_WIND <- wind$LOCAL_DATETIME_STORM_CLOSEST
  wind$VMAX_SUST_MS <- rep(c(25, 17.5, 10, 30, NA_real_), 3)
  wind$SUST_DUR_MINUTES <- rep(c(120, 60, 0, 180, NA_real_), 3)
  wind$VMAX_GUST_MS <- wind$VMAX_SUST_MS * 1.2
  wind$GUST_DUR_MINUTES <- wind$SUST_DUR_MINUTES
  wind$WIND_RADII_VMAX_SUST_MS <- wind$VMAX_SUST_MS
  wind$WIND_RADII_SUST_DUR_MINUTES <- wind$SUST_DUR_MINUTES
  zero_pairs <- pairs[c(4, 10, 16), ]
  # The zero-wind unit lies inside the precipitation/flood field for storm B.
  zero_pairs$STORM_DIST_KM[2] <- 400
  links <- dplyr::bind_rows(
    dplyr::mutate(wind[, names(pairs)], WIND_EXPOSURE = TRUE),
    dplyr::mutate(zero_pairs, WIND_EXPOSURE = FALSE)
  )
  links$LOCAL_DATE_STORM_CLOSEST <- as.Date(links$LOCAL_DATE_STORM_CLOSEST)
  links$LOCAL_DATETIME_STORM_CLOSEST <- NULL
  links$PRECIP_IN_FIELD <- links$STORM_DIST_KM <= 500
  links$PRECIP_WINDOW_START <- links$LOCAL_DATE_STORM_CLOSEST - 2
  links$PRECIP_WINDOW_END <- links$LOCAL_DATE_STORM_CLOSEST + 1
  links$FLOOD_IN_FIELD <- links$PRECIP_IN_FIELD
  links$FLOOD_WINDOW_START <- links$PRECIP_WINDOW_START
  links$FLOOD_WINDOW_END <- links$PRECIP_WINDOW_END
  precip <- tibble::as_tibble(expand.grid(
    ADM2_ID = adm2$ADM2_ID,
    LOCAL_DATE = as.character(seq(
      as.Date("2020-08-01"),
      by = "day",
      length.out = 10
    )),
    stringsAsFactors = FALSE
  ))
  precip$PRECIP_MM <- rep(c(0, 20, 75, 100, NA_real_, 125), 10)
  flood <- tibble::tibble(
    ADM2_ID = adm2$ADM2_ID[c(1, 1, 2, 4, 5)],
    FLOOD_ID = paste0("PF", 1:5),
    FLOOD_START_DATE = c(
      "2020-07-31",
      "2020-08-03",
      "2020-08-04",
      "2020-08-05",
      "2020-08-06"
    ),
    FLOOD_END_DATE = c(
      "2020-08-04",
      "2020-08-06",
      "2020-08-07",
      "2020-08-06",
      "2020-08-08"
    ),
    PROP_AREA_IN_FLOOD_REPORT = c(0.1, 0.2, 0, 0.4, NA_real_),
    PROP_POP_IN_FLOOD_REPORT = c(0.05, 0.1, 0, 0.3, NA_real_),
    POP_CENTROID_IN_FLOOD_REPORT = c(TRUE, TRUE, FALSE, TRUE, NA)
  )
  tracks <- tibble::tibble(
    STORM_ID = rep(c("PLOT_A", "PLOT_C"), each = 4),
    ISO_TIME = c(
      paste0("2020-08-04T", c("00", "06", "12", "18"), ":00:00"),
      paste0("2020-08-06T", c("00", "06", "12", "18"), ":00:00")
    ),
    LAT = c(24, 25, 26, 27, -17, -16.5, -16, -15),
    LON = c(-82, -81, -80, -79, 178, 179.5, -179.5, -178),
    USA_WIND = c(30, 45, 60, 40, 35, 55, 65, 40),
    USA_PRES = c(1000, 990, 980, 995, 1000, 985, 970, 990),
    USA_STATUS = rep(c("TS", "TS", "HU", "TS"), 2),
    USA_RMW = rep(20, 8),
    BASIN = rep(c("NA", "SP"), each = 4),
    SEASON = rep(2020L, 8)
  )
  codebook <- list(variables = list(PRECIP_MM = list(units = "mm")))

  tables <- list(
    wind = wind,
    precip = precip,
    flood = flood,
    zero_pairs = zero_pairs,
    tracks = tracks,
    storm_metadata = storm_metadata,
    adm2 = adm2,
    geometry = geometry,
    codebook = codebook,
    links = links
  )

  # --- manifest ---------------------------------------------------------------
  # One invented file per source table, as a real retrieval would record.
  downloaded_at <- as.POSIXct("2026-01-05 12:00:00", tz = "UTC")
  files <- tibble::tibble(
    directory_label = c(
      "02_wind/exposures",
      "02_wind/zero_pairs",
      "02_wind/storm_metadata",
      "02_wind/storm_tracks",
      "04_precip",
      "03_flood",
      "metadata",
      "metadata",
      "metadata"
    ),
    label = c(
      rep("2020.parquet", 6),
      "adm2.parquet",
      "codebook.json",
      "adm2_geometry_simplified.parquet"
    )
  )
  files$file_id <- seq_len(nrow(files))
  files$checksum <- vapply(
    file.path(files$directory_label, files$label),
    digest::digest,
    character(1),
    algo = "md5",
    serialize = FALSE
  )
  files$checksum_type <- "MD5"
  files$size_bytes <- 1000 * files$file_id
  manifest <- manifest_new(list(server = "example.invalid"))
  for (i in seq_len(nrow(files))) {
    manifest <- manifest_add(
      manifest,
      "doi:10.0000/EXAMPLE",
      "1.0",
      files[i, ],
      downloaded_at = downloaded_at,
      source_role = "historical"
    )
  }

  args <- list(
    geo_filters = list(WHO_ENTITY = c("Example Coast", "Example Pacific")),
    storm_filters = list(),
    storm_name_years = NULL,
    wind_radius_km = 500,
    polygon = NULL,
    reproject = TRUE,
    date_range = c("2020-08-01", "2020-08-31"),
    date_list = NULL,
    date_format = NULL,
    tables = setdiff(VALID_TABLES, "pop"),
    precip_days_before = 2,
    precip_days_after = 1,
    precip_radius_km = 500,
    flood_days_before = 2,
    flood_days_after = 1,
    flood_radius_km = 500,
    flood_match = "start",
    geometry_resolution = "simplified",
    make_valid = TRUE,
    resolved_adm2_ids = NULL,
    resolved_storm_ids = NULL,
    storm_metadata_years = NULL
  )
  date_spec <- parse_date_input(args$date_range)
  availability <- availability_metadata(
    date_spec,
    args$tables,
    storm_metadata = storm_metadata,
    links = links,
    today = as.Date("2026-01-05")
  )
  manifest <- manifest_finalize(
    manifest,
    resolved_query = manifest_query_from_args(
      args,
      resolved_adm2_ids = adm2$ADM2_ID,
      resolved_storm_ids = storm_metadata$STORM_ID,
      storm_metadata_years = 2020L
    ),
    availability = availability
  )

  example <- new_gtropic_data(
    tables = tables,
    manifest = manifest,
    query = NULL,
    filters_resolved = list(
      adm2_ids = adm2$ADM2_ID,
      storm_ids = storm_metadata$STORM_ID,
      date_start = date_spec$start,
      date_end = date_spec$end,
      date_order = date_spec$inferred_order,
      years_scanned = 2020L,
      wind_radius_km = 500,
      precip_days_before = 2,
      precip_days_after = 1,
      precip_radius_km = 500,
      flood_days_before = 2,
      flood_days_after = 1,
      flood_radius_km = 500,
      flood_match = "start",
      inconsistent_adm2_ids = character(0)
    ),
    warnings = list(),
    availability = availability,
    retrieved_at = manifest$retrieved_at
  )
  attr(example, "gtropic_version") <- NULL

  path <- file.path("inst", "extdata", "gtropic-example.rds")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(example, path, version = 3, compress = "xz")
  message("Wrote ", path, " (", file.size(path), " bytes)")
})

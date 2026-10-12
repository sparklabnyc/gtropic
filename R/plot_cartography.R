#' Natural Earth basemaps cached for the current R session
#'
#' Countries use the locally installed Natural Earth data package. First use of
#' an administrative-level-1 layer requires the optional, non-CRAN
#' `rnaturalearthhires` package. No layer is downloaded by this function.
#' @param scale Country resolution: `"small"` (110) or `"medium"` (50).
#'   Large (10) country layers are deliberately unsupported. Ignored for admin1,
#'   which uses the resolution supplied by `rnaturalearth::ne_states()`.
#' @param what One of `"countries"`, `"admin1"`, or `"none"`.
#' @return An sf object, or NULL for `what = "none"`. Layers are cached by
#'   resolution and type until the R session ends.
#' @examples
#' \donttest{
#' gtropic_basemap(what = "none")
#' if (requireNamespace("rnaturalearth", quietly = TRUE) &&
#'   requireNamespace("rnaturalearthdata", quietly = TRUE)) {
#'   gtropic_basemap()
#' }
#' }
#' @export
gtropic_basemap <- function(scale = "medium",
                            what = c("countries", "admin1", "none")) {
  what <- match.arg(what)
  if (what == "none") {
    return(NULL)
  }
  if (what == "countries") {
    if (
      length(scale) != 1L || is.na(scale) ||
        !as.character(scale) %in% c("small", "medium", "110", "50")
    ) {
      cli::cli_abort("Country {.arg scale} must be 'small' (110) or
                      'medium' (50); large (10) countries are not supported.")
    }
    scale <- if (as.character(scale) %in% c("small", "110")) 110 else 50
  } else {
    scale <- 10
  }
  key <- paste(what, scale, sep = "_")
  if (exists(key, envir = .plot_basemap_cache, inherits = FALSE)) {
    return(.plot_basemap_cache[[key]])
  }
  rlang::check_installed(c("sf", "rnaturalearth"), reason = "to use basemaps")
  if (what == "countries") {
    rlang::check_installed("rnaturalearthdata",
      reason = "to use country basemaps"
    )
  } else {
    # Gate by name only: this non-CRAN package is not an imported dependency.
    rlang::check_installed("rnaturalearthhires", reason = paste0(
      "to use admin1 basemaps. Install it with ",
      "install.packages('rnaturalearthhires', repos = c(",
      "'https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))"
    ))
  }
  layer <- plot_fetch_basemap(scale, what)
  .plot_basemap_cache[[key]] <- layer
  layer
}

#' Session-local storage for unmodified Natural Earth layers
#' @keywords internal
#' @noRd
.plot_basemap_cache <- new.env(parent = emptyenv())

#' Read installed Natural Earth layers after dependency checks
#' @keywords internal
#' @noRd
plot_fetch_basemap <- function(scale, what) {
  if (what == "countries") {
    rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
  } else {
    rnaturalearth::ne_states(returnclass = "sf")
  }
}

#' Normalize map geometry to the geographic plotting frame
#' @keywords internal
#' @noRd
plot_geographic_geometry <- function(x, label = "data") {
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort("{.arg {label}} must have a known CRS.")
  }
  sf::st_transform(x, 4326)
}

#' Apply a longitude-only operation while retaining rings, holes, and dimensions
#' @keywords internal
#' @noRd
plot_longitude_coordinates <- function(x, fun) {
  if (is.matrix(x)) {
    x[, 1] <- fun(x[, 1])
  } else if (is.numeric(x)) {
    if (length(x)) x[1] <- fun(x[1])
  } else {
    x[] <- lapply(x, plot_longitude_coordinates, fun = fun)
  }
  x
}

#' Rebuild geometry bounds after changing longitudes
#' @keywords internal
#' @noRd
plot_geometry_longitudes <- function(x, fun) {
  parts <- lapply(sf::st_geometry(x), plot_longitude_coordinates, fun = fun)
  geometry <- sf::st_sfc(parts, crs = 4326)
  if (inherits(x, "sf")) sf::st_set_geometry(x, geometry) else geometry
}

#' Split polygons at the seam opposite a central longitude using GDAL
#'
#' Rotate to GDAL's conventional dateline, wrap, then rotate back. This keeps
#' Greenwich-crossing land in separate pieces at the edges of a Pacific map.
#' Unlike global spherical intersections, GDAL wrapping needs no s2 rebuild and
#' keeps one geometry per input row. A sub-metre gap assigns seam vertices to
#' their proper side when PROJ normalizes longitudes during projection.
#' @keywords internal
#' @noRd
plot_wrap_longitude <- function(x, centre) {
  x <- plot_geometry_longitudes(x, function(lon) {
    (lon - centre + 180) %% 360 - 180
  })
  x <- sf::st_wrap_dateline(x,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")
  )
  plot_geometry_longitudes(x, function(lon) {
    pmax(-180 + 1e-7, pmin(180 - 1e-7, lon)) + centre
  })
}

#' Split at both the display-frame seam and the requested projection seam
#' @keywords internal
#' @noRd
plot_seam_geometry <- function(x, shifted, crs = NULL) {
  centre <- if (shifted) 180 else 0
  if (!is.null(crs) && !isTRUE(sf::st_is_longlat(crs))) {
    proj <- crs$proj4string
    lon_0 <- regmatches(proj, regexpr("\\+lon_0=[-+0-9.eE]+", proj))
    projection_centre <- if (length(lon_0)) {
      as.numeric(sub("+lon_0=", "", lon_0, fixed = TRUE))
    } else {
      0
    }
    if (projection_centre != centre) {
      # Projected coordinates follow the projection's seam. Numeric 0--360
      # bounds and track longitudes still identify the selected Pacific frame.
      return(plot_wrap_longitude(x, projection_centre))
    }
  }
  plot_wrap_longitude(x, centre)
}

#' Preserve published longitudes for track segmentation before shifting
#' @keywords internal
#' @noRd
plot_shift_tracks <- function(tracks, shifted) {
  if (is.null(tracks)) {
    return(NULL)
  }
  if (!"original_LON" %in% names(tracks)) tracks$original_LON <- tracks$LON
  if (shifted) {
    tracks$LON <- ifelse(tracks$LON < 0, tracks$LON + 360, tracks$LON)
  }
  tracks
}

#' Calculate geographic bounds of geometry and optional track observations
#' @keywords internal
#' @noRd
plot_frame_bbox <- function(data, tracks = NULL, shifted = FALSE) {
  x <- y <- numeric()
  if (inherits(data, "sf") || inherits(data, "sfc")) {
    if (shifted) data <- plot_seam_geometry(data, shifted = TRUE)
    bounds <- sf::st_bbox(data)
    x <- unname(bounds[c("xmin", "xmax")])
    y <- unname(bounds[c("ymin", "ymax")])
  } else if (!is.null(data)) {
    tracks <- data
  }
  if (!is.null(tracks)) {
    tracks <- plot_shift_tracks(tracks, shifted)
    valid <- is.finite(tracks$LON) & is.finite(tracks$LAT)
    x <- c(x, tracks$LON[valid])
    y <- c(y, tracks$LAT[valid])
  }
  if (!any(is.finite(x)) || !any(is.finite(y))) {
    cli::cli_abort("Cannot determine map extent: no finite coordinates remain.")
  }
  stats::setNames(
    c(range(x[is.finite(x)]), range(y[is.finite(y)])),
    c("xmin", "xmax", "ymin", "ymax")
  )
}

#' Read explicit bounds without confusing numeric order with sf bbox order
#' @keywords internal
#' @noRd
plot_numeric_bbox <- function(bbox) {
  order <- c("xmin", "xmax", "ymin", "ymax")
  if (inherits(bbox, "bbox")) {
    if (is.na(sf::st_crs(bbox))) {
      cli::cli_abort("{.arg bbox} must have a known CRS.")
    }
    bbox <- sf::st_transform(bbox, 4326)
  }
  if (!is.numeric(bbox) || length(bbox) != 4L || any(!is.finite(bbox))) {
    cli::cli_abort("{.arg bbox} must be sf, sfc, bbox, or four finite numbers
                    in order c(xmin, xmax, ymin, ymax).")
  }
  if (!is.null(names(bbox))) {
    if (!setequal(names(bbox), order)) {
      cli::cli_abort("Named {.arg bbox} must have
                      xmin, xmax, ymin, ymax names.")
    }
    bbox <- bbox[order]
  }
  bbox <- stats::setNames(as.numeric(bbox), order)
  if (bbox[1] > bbox[2] || bbox[3] > bbox[4]) {
    cli::cli_abort("{.arg bbox} minima must not exceed maxima;
                    use longitudes such as 170, 190 to cross the dateline.")
  }
  if (
    bbox[1] < -180 || bbox[2] > 360 || diff(bbox[1:2]) > 360 ||
      bbox[3] < -90 || bbox[4] > 90
  ) {
    cli::cli_abort("{.arg bbox} must contain geographic longitude/latitude
                    bounds, with latitude between -90 and 90.")
  }
  bbox
}

#' Resolve extent, projection, and a common longitude frame for map builders
#'
#' Geometry is normalized to EPSG:4326; coord_sf performs the final projection.
#' Bounds always use c(xmin, xmax, ymin, ymax), in geographic degrees. Track
#' segmentation belongs to the track module, which can use original_LON.
#' Wide shifted maps use Pacific-centered Equal Earth automatically; other wide
#' maps use EPSG:8857. An explicit CRS always wins, with geometry split at its seam.
#' @keywords internal
#' @noRd
plot_cartography <- function(data, basemap = "countries", bbox = NULL,
                             expand = 2, crs = NULL) {
  rlang::check_installed("sf", reason = "to prepare map cartography")
  basemap <- rlang::arg_match(basemap, c("countries", "admin1", "none"))
  if (
    !is.numeric(expand) || length(expand) != 1L ||
      !is.finite(expand) || expand < 0
  ) {
    cli::cli_abort("{.arg expand} must be one nonnegative finite number.")
  }
  info <- attr(data, "plot_info", exact = TRUE)
  tracks <- info$tracks
  if (inherits(data, "sf")) {
    data <- plot_geographic_geometry(data)
  } else if (is.data.frame(data) && all(c("LON", "LAT") %in% names(data))) {
    if (is.null(tracks)) {
      tracks <- data
      attr(tracks, "plot_info") <- NULL
    }
  } else {
    cli::cli_abort("Map {.arg data} must be prepared sf or a track data frame.")
  }
  if (is.null(bbox) || inherits(bbox, "sf") || inherits(bbox, "sfc")) {
    extent_data <- if (is.null(bbox)) {
      data
    } else {
      plot_geographic_geometry(bbox, "bbox")
    }
    extent_tracks <- if (is.null(bbox)) tracks else NULL
    bounds <- plot_frame_bbox(extent_data, extent_tracks)
    shifted_bounds <- plot_frame_bbox(extent_data, extent_tracks,
      shifted = TRUE
    )
    shifted <- diff(bounds[1:2]) > 180 &&
      diff(shifted_bounds[1:2]) < diff(bounds[1:2])
    if (shifted) bounds <- shifted_bounds
    # Already-shifted inputs also define a 0-360 plotting frame.
    shifted <- shifted || bounds[2] > 180
    bounds <- plot_numeric_bbox(bounds)
  } else {
    bounds <- plot_numeric_bbox(bbox)
    shifted <- bounds[2] > 180
  }
  bounds <- bounds + c(-expand, expand, -expand, expand)
  bounds[1:2] <- pmax(
    if (shifted) 0 else -180,
    pmin(if (shifted) 360 else 180, bounds[1:2])
  )
  bounds[3:4] <- pmax(-90, pmin(90, bounds[3:4]))
  projection <- if (is.null(crs)) {
    sf::st_crs(if (diff(bounds[1:2]) < 40) {
      4326
    } else if (shifted) {
      "+proj=eqearth +lon_0=180 +datum=WGS84 +units=m +no_defs"
    } else {
      8857
    })
  } else {
    tryCatch(sf::st_crs(crs), error = function(e) {
      cli::cli_abort("{.arg crs} must identify a valid coordinate reference
                      system.", parent = e)
    })
  }
  if (is.na(projection)) {
    cli::cli_abort("{.arg crs} must identify a valid
                    coordinate reference system.")
  }
  if (inherits(data, "sf")) {
    data <- plot_seam_geometry(data, shifted, projection)
  } else {
    data <- plot_shift_tracks(data, shifted)
  }
  tracks <- plot_shift_tracks(tracks, shifted)
  info$tracks <- tracks
  attr(data, "plot_info") <- info
  layer <- gtropic_basemap(what = basemap)
  if (!is.null(layer)) {
    layer <- plot_geographic_geometry(layer, "basemap")
    layer <- plot_seam_geometry(layer, shifted, projection)
  }
  list(
    data = data, tracks = tracks, basemap = layer, bbox = bounds,
    crs = projection, shifted = unname(shifted)
  )
}

#' Start a map with an optional, independent basemap layer
#' @keywords internal
#' @noRd
plot_map_base <- function(data, cartography) {
  rlang::check_installed("ggplot2", reason = "to build maps")
  p <- ggplot2::ggplot(data)
  if (!is.null(cartography$basemap)) {
    p <- p + ggplot2::geom_sf(
      data = cartography$basemap, inherit.aes = FALSE,
      fill = "grey95", colour = "grey75", linewidth = 0.25
    )
  }
  p
}

#' Apply shared map coordinates, facets, labels, and publication defaults
#' @keywords internal
#' @noRd
plot_map_finish <- function(p, data, cartography, title = NULL, subtitle = NULL,
                            caption = NULL, gridlines = TRUE) {
  rlang::check_installed(c("ggplot2", "sf"), reason = "to finish maps")
  info <- attr(data, "plot_info", exact = TRUE)
  facets <- unique(c(if (length(info$metric) > 1L) "metric", info$facet_by))
  if (length(facets)) {
    labels <- if ("metric" %in% facets) {
      ggplot2::labeller(metric = info$metric_labels)
    } else {
      ggplot2::label_value
    }
    p <- p + ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(facets)),
      labeller = labels
    )
  }
  default_title <- switch(info$type,
    exposure = "Tropical cyclone exposure",
    count = paste("Tropical cyclone exposure", info$count_unit),
    track = "Tropical cyclone tracks",
    "G-TROPIC"
  )
  if (isTRUE(info$track) && !is.null(info$tracks) &&
    dplyr::n_distinct(info$tracks$STORM_ID) == 1L) {
    label <- plot_track_display_name(info$tracks)[1L]
    default_title <- paste0(default_title, ": ", label)
  }
  default_subtitle <- plot_date_subtitle(info)
  projected <- !isTRUE(sf::st_is_longlat(cartography$crs))
  key_band <- if (info$type %in% c("exposure", "count") &&
    is.null(info$track_legend_position)) {
    diff(cartography$bbox[3:4]) * 0.14
  } else {
    0
  }
  p <- p + ggplot2::coord_sf(
    crs = cartography$crs, default_crs = sf::st_crs(4326),
    xlim = cartography$bbox[1:2],
    ylim = c(cartography$bbox[3] - key_band, cartography$bbox[4]),
    # Curved projected bbox edges need a small panel margin to avoid clipping
    # observations exactly on the requested geographic boundary.
    expand = if (projected) c(FALSE, TRUE, FALSE, FALSE) else FALSE
  ) + theme_gtropic() + ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text = if (gridlines) ggplot2::element_text() else ggplot2::element_blank(),
    axis.ticks = if (gridlines) ggplot2::element_line() else ggplot2::element_blank(),
    panel.grid = if (gridlines) {
      ggplot2::element_line(
        colour = "grey90", linewidth = 0.25
      )
    } else {
      ggplot2::element_blank()
    },
    legend.position = if (isTRUE(info$track)) "bottom" else "none",
    legend.box = "horizontal",
    legend.direction = "horizontal"
  ) + ggplot2::labs(
    title = title %||% default_title,
    subtitle = subtitle %||% default_subtitle,
    caption = plot_wrap_caption(plot_caption(info, caption, cartography$crs))
  )
  attr(p, "gtropic_bbox") <- cartography$bbox
  p
}

#' Insert stable line breaks before long captions reach the plot boundary
#' @keywords internal
#' @noRd
plot_wrap_caption <- function(caption, width = 80L) {
  stringr::str_wrap(caption, width = width)
}

#' Format finite numeric values without scientific notation
#' @keywords internal
#' @noRd
plot_format_number <- function(x, digits = 5L) {
  out <- format(x, scientific = FALSE, trim = TRUE, digits = digits)
  out <- sub("(\\.[0-9]*?)0+$", "\\1", out)
  sub("\\.$", "", out)
}

#' Format metric scale labels with a fixed decimal precision
#' @keywords internal
#' @noRd
plot_format_key_number <- function(x) {
  formatC(x, format = "f", digits = 2L)
}

#' Format the displayed observation range as a map subtitle
#' @keywords internal
#' @noRd
plot_date_subtitle <- function(info) {
  dates <- info$observed_range
  if (is.null(dates) || length(dates) != 2L || anyNA(dates)) {
    return(NULL)
  }
  paste(format(as.Date(dates), "%m/%d/%Y"), collapse = " - ")
}

#' Format a CRS for the automatic map caption
#' @keywords internal
#' @noRd
plot_crs_label <- function(crs) {
  epsg <- sf::st_crs(crs)$epsg
  if (!is.na(epsg)) {
    return(paste0("EPSG:", epsg))
  }
  sf::st_crs(crs)$input %||% "unknown CRS"
}

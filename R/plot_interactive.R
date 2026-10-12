#' Build an offline Leaflet map of prepared exposure or count values
#'
#' Metric/facet panels are selectable overlays, initially showing the first
#' panel. Facets share their metric's scale. Leaflet receives geographic
#' coordinates in the cartography longitude frame, irrespective of static CRS.
#' Extra arguments style addPolygons(); data, groups, fills and popups are owned
#' by this builder. No tiles, rendering, or scientific calculations are needed.
#' @keywords internal
#' @noRd
build_interactive_plot <- function(data, cartography, title = NULL,
                                   subtitle = NULL, caption = NULL,
                                   gridlines = TRUE, ...) {
  info <- attr(data, "plot_info", exact = TRUE)
  dots <- list(...)
  rlang::check_installed(c("leaflet", "sf", "ggplot2"),
    reason = "to build interactive maps"
  )
  dots <- plot_interactive_options(dots)
  mapped <- plot_interactive_geometry(cartography$data %||% data, cartography)
  map <- leaflet::leaflet()
  groups <- character()
  if (!is.null(cartography$basemap)) {
    map <- leaflet::addPolygons(map,
      data = plot_interactive_geometry(cartography$basemap, cartography),
      group = "Basemap", color = "grey75", weight = 0.5,
      fillColor = "grey95", fillOpacity = 1,
      options = leaflet::pathOptions(interactive = FALSE)
    )
    groups <- "Basemap"
  }

  panels <- character()
  for (i in seq_len(nrow(info$metrics))) {
    spec <- info$metrics[i, ]
    part <- mapped[mapped$metric == spec$metric, , drop = FALSE]
    values <- if (info$binary) part$exposed else part$value
    if (!info$binary) values[!is.finite(values)] <- NA_real_
    scale <- plot_interactive_scale(values, spec, info$binary)
    facet <- setdiff(info$facet_by, "metric")
    levels <- if (length(facet)) unique(part[[facet]]) else NA_character_
    for (j in seq_along(levels)) {
      selected <- if (length(facet)) part[[facet]] %in% levels[j] else TRUE
      panel <- part[selected, , drop = FALSE]
      label <- paste0(
        spec$label,
        if (length(facet)) paste0(" | ", facet, "=", levels[j])
      )
      # Layer-control names are HTML too; distinguish a missing facet from
      # a literal "NA" label without collapsing their geometries together.
      label <- utils::tail(make.unique(c(panels, label)), 1L)
      panels <- c(panels, label)
      group <- plot_interactive_escape(label)
      map <- do.call(leaflet::addPolygons, c(list(
        map = map, data = panel, group = group,
        layerId = paste0("panel-", length(panels), "-", panel$ADM2_ID),
        fillColor = scale$map(values[selected]),
        popup = plot_interactive_popup(panel, spec, info)
      ), dots))
      map <- plot_interactive_legend(map, scale, values, group, spec, info)
    }
  }
  panel_groups <- plot_interactive_escape(panels)
  groups <- c(groups, panel_groups)
  if (isTRUE(info$track) && !is.null(cartography$tracks)) {
    overlay <- plot_interactive_tracks(
      map, cartography$tracks, info$track_map_key, info$track_interval
    )
    map <- overlay$map
    groups <- c(groups, overlay$groups)
  }
  map <- leaflet::addLayersControl(map,
    overlayGroups = groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  if (length(panel_groups) > 1L) {
    map <- leaflet::hideGroup(map, panel_groups[-1L])
  }
  default_title <- if (info$type == "count") {
    paste("Tropical cyclone exposure", info$count_unit)
  } else {
    "Tropical cyclone exposure"
  }
  if (isTRUE(info$track) && !is.null(info$tracks) &&
    dplyr::n_distinct(info$tracks$STORM_ID) == 1L) {
    default_title <- paste0(
      default_title, ": ", plot_track_display_name(info$tracks)[1L]
    )
  }
  heading <- paste0(
    "<strong>", plot_interactive_escape(title %||% default_title), "</strong>",
    "<br/>", plot_interactive_escape(
      subtitle %||% plot_date_subtitle(info)
    )
  )
  map <- leaflet::addControl(map, heading,
    position = "topleft", layerId = "gtropic-title"
  )
  map <- leaflet::addControl(map,
    plot_interactive_escape(plot_caption(info, caption, cartography$crs)),
    position = "bottomleft", layerId = "gtropic-caption"
  )
  bounds <- cartography$bbox
  map <- leaflet::fitBounds(map,
    lng1 = unname(bounds[1]), lat1 = unname(bounds[3]),
    lng2 = unname(bounds[2]), lat2 = unname(bounds[4])
  )
  plot_interactive_background(map)
}

#' Force a light background for tile-free Leaflet maps
#' @keywords internal
#' @noRd
plot_interactive_background <- function(map) {
  htmlwidgets::onRender(map, paste0(
    "function(el) {",
    "el.style.backgroundColor = '#FFFFFF';",
    "var container = el.querySelector('.leaflet-container');",
    "if (container) container.style.backgroundColor = '#FFFFFF';",
    "}"
  ))
}

#' Escape text inserted into Leaflet HTML without another dependency
#' @keywords internal
#' @noRd
plot_interactive_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  for (pair in list(
    c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"),
    c('"', "&quot;"), c("'", "&#39;")
  )) {
    x <- gsub(pair[1], pair[2], x, fixed = TRUE)
  }
  x
}

#' Validate Leaflet polygon styling without allowing data overrides
#' @keywords internal
#' @noRd
plot_interactive_options <- function(dots) {
  nms <- names(dots)
  invalid_names <- is.null(nms) || anyNA(nms) ||
    any(!nzchar(nms)) || anyDuplicated(nms)
  if (length(dots) && invalid_names) {
    cli::cli_abort("Interactive arguments in {.arg ...} must be uniquely named.")
  }
  reserved <- c(
    "map", "data", "lng", "lat", "layerId", "group", "fillColor", "popup", "label"
  )
  supported <- setdiff(names(formals(leaflet::addPolygons)), reserved)
  unknown <- setdiff(nms, supported)
  if (length(unknown)) {
    cli::cli_abort("Unsupported or reserved interactive arguments in {.arg ...}:
                    {.val {unknown}}. Supply Leaflet polygon styling options.")
  }
  utils::modifyList(list(
    color = "grey50", weight = 0.5, opacity = 1, fillOpacity = 0.8
  ), dots, keep.null = TRUE)
}

#' Normalize vector layers to the shared geographic longitude frame
#' @keywords internal
#' @noRd
plot_interactive_geometry <- function(data, cartography) {
  data <- plot_geographic_geometry(data)
  if (isTRUE(cartography$shifted)) data <- sf::st_shift_longitude(data)
  data
}

#' Train native ggplot2 scales over every facet of a metric
#' @keywords internal
#' @noRd
plot_interactive_scale <- function(values, spec, binary) {
  if (binary) {
    scale <- scale_fill_gtropic_d(
      metric = spec$metric, palette = spec$palette, direction = spec$direction,
      limits = c(FALSE, TRUE), drop = FALSE
    )
  } else {
    observed <- values[is.finite(values)]
    scale <- scale_fill_gtropic_c(
      metric = spec$metric, palette = spec$palette, direction = spec$direction,
      limits = if (length(observed)) range(observed) else c(0, 1)
    )
  }
  scale$train(values)
  scale
}

#' Attach group-aware legends, including constant and wholly missing metrics
#' @keywords internal
#' @noRd
plot_interactive_legend <- function(map, scale, values, group, spec, info) {
  title <- plot_interactive_escape(spec$legend)
  observed <- if (info$binary) c(FALSE, TRUE) else sort(unique(stats::na.omit(values)))
  if (!info$binary && length(observed) > 1L) {
    limits <- range(observed)
    observed <- pretty(limits, n = 5L)
    observed <- observed[observed >= limits[1] & observed <= limits[2]]
    if (!length(observed)) observed <- limits
  }
  labels <- if (info$binary) {
    c("Not exposed", "Exposed")
  } else {
    plot_format_key_number(observed)
  }
  colors <- scale$map(observed)
  if (anyNA(values)) {
    labels <- c(labels, "NA")
    colors <- c(colors, scale$na.value)
  }
  leaflet::addLegend(map,
    colors = colors, labels = labels, title = title, group = group,
    position = "bottomright", opacity = 0.8
  )
}

#' Show prepared values and published distances, never track-derived distances
#' @keywords internal
#' @noRd
plot_interactive_popup <- function(data, spec, info) {
  field <- function(column) {
    value <- data[[column]] %||% rep(NA_character_, nrow(data))
    if (is.numeric(value)) value <- plot_format_number(value)
    plot_interactive_escape(value)
  }
  combined <- if ("STORM_ID" %in% names(data)) {
    is.na(data$STORM_ID)
  } else {
    rep(TRUE, nrow(data))
  }
  distance_label <- ifelse(combined,
    "STORM_DIST_KM (nearest published distance)",
    "STORM_DIST_KM (published distance)"
  )
  unit <- if (spec$aggregate == "n" && info$type == "exposure") {
    "observations"
  } else {
    spec$value_unit
  }
  paste0(
    "<b>ADM2_NAME:</b> ", field("ADM2_NAME"),
    "<br/><b>WHO_ENTITY:</b> ", field("WHO_ENTITY"),
    "<br/><b>", plot_interactive_escape(spec$metric), ":</b> ", field("value"),
    " ", plot_interactive_escape(unit),
    if (info$binary) paste0("<br/><b>Exposed:</b> ", field("exposed")),
    "<br/><b>", distance_label, ":</b> ", field("STORM_DIST_KM"), " km"
  )
}

#' Add independently selectable storm tracks with original seam splits
#' @keywords internal
#' @noRd
plot_interactive_tracks <- function(map, tracks, track_map_key, track_interval) {
  tracks <- plot_split_tracks(tracks)
  key <- plot_validate_track_map_key(track_map_key)
  groups <- character()
  for (storm in unique(tracks$STORM_ID)) {
    part <- tracks[tracks$STORM_ID %in% storm, , drop = FALSE]
    group <- plot_interactive_escape(paste(
      "Tracks:", plot_track_display_name(part)[1L]
    ))
    groups <- c(groups, group)
    for (segment in unique(part$.plot_group)) {
      path <- part[part$.plot_group == segment, , drop = FALSE]
      path <- path[is.finite(path$LON) & is.finite(path$LAT), , drop = FALSE]
      if (nrow(path) < 2L) next
      map <- leaflet::addPolylines(map,
        lng = path$LON, lat = path$LAT, group = group,
        color = "grey30", weight = 2, opacity = 1
      )
    }
    part <- part[is.finite(part$LON) & is.finite(part$LAT), , drop = FALSE]
    if (!nrow(part)) next
    part <- plot_sample_track_points(part, track_interval)
    wind <- plot_convert_track_wind(part$USA_WIND, key$unit[1L])
    category <- plot_categorize_track_wind(part$USA_WIND, key)
    colors <- unname(stats::setNames(key$colour, key$category)[as.character(category)])
    map <- leaflet::addCircleMarkers(map,
      lng = part$LON, lat = part$LAT, group = group, radius = 5,
      color = "grey20", fillColor = colors, weight = 1, fillOpacity = 0.9,
      popup = paste0(
        "<b>STORM_ID:</b> ", plot_interactive_escape(part$STORM_ID),
        "<br/><b>ISO_TIME (UTC):</b> ",
        plot_interactive_escape(format(part$ISO_TIME, tz = "UTC")),
        "<br/><b>USA_WIND:</b> ", plot_interactive_escape(plot_format_number(wind)),
        " ", key$unit[1L],
        "<br/><b>Category:</b> ", plot_interactive_escape(category)
      )
    )
  }
  list(map = map, groups = groups)
}

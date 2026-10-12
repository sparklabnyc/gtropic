#' Normalize locally available plotting tables without fetching data
#'
#' Codebooks retain their nested structure and geometry retains its sf class.
#' Query, manifest, and resolved filters survive normalization of plain lists.
#' @keywords internal
#' @noRd
normalize_plot_data <- function(data) {
  if (
    !is.list(data) || is.data.frame(data) || is.null(names(data)) ||
      anyNA(names(data)) || any(!nzchar(names(data))) ||
      anyDuplicated(names(data))
  ) {
    cli::cli_abort("{.arg data} must be a uniquely named list of tables
                    or a gtropic_data object.")
  }
  # readRDS() restores sf classes without registering their subset methods.
  if (any(vapply(data, inherits, logical(1), what = "sf"))) {
    rlang::check_installed("sf", reason = "to normalize spatial plotting tables")
  }
  out <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    if (nm == "codebook" || is.null(x) || inherits(x, "sf")) {
      return(x)
    }
    arrow_classes <- c("arrow_dplyr_query", "Dataset", "Table", "ArrowTabular")
    if (inherits(x, arrow_classes)) {
      x <- dplyr::collect(x)
    }
    if (!is.data.frame(x)) {
      cli::cli_abort("Table {.val {nm}} must be a data frame
                      or a collectable Arrow table.")
    }
    tibble::as_tibble(x)
  })
  names(out) <- names(data)
  metadata <- c("query", "manifest", "filters_resolved", "geometry_resolution")
  for (nm in metadata) {
    attr(out, nm) <- attr(data, nm, exact = TRUE)
  }
  out
}

#' Construct a copyable query without evaluating any stored expressions
#' @keywords internal
#' @noRd
plot_query_hint <- function(data, tables, simplified = FALSE) {
  query <- attr(data, "query", exact = TRUE)
  if (!is.call(query)) query <- quote(gtropic_data())
  # Preserve the original expression, including a symbolic tables argument.
  old <- query$tables
  extra <- setdiff(tables, "links") # Retrieval always supplies links.
  if (is.null(old)) {
    extra <- unique(c(intersect(names(data), VALID_TABLES), extra))
    query$tables <- as.call(c(list(as.name("c")), as.list(extra)))
  } else {
    query$tables <- as.call(c(list(as.name("c"), old), as.list(extra)))
  }
  if (simplified) query$geometry_resolution <- "simplified"
  paste(deparse(query, width.cutoff = 100L), collapse = "\n")
}

#' Require named tables and columns and show how to retrieve them
#' @keywords internal
#' @noRd
plot_require <- function(data, requirements, type) {
  for (nm in names(requirements)) {
    if (is.null(data[[nm]])) {
      cli::cli_abort(c(
        "{.code type = {dQuote(type)}} needs the {.val {nm}} table,
         which is not in {.arg data}.",
        i = "Re-run your query with the required tables included:
             {plot_query_hint(data, names(requirements))}"
      ))
    }
    missing <- setdiff(requirements[[nm]], names(data[[nm]]))
    if (length(missing)) {
      cli::cli_abort(c(
        "Table {.val {nm}} is missing required column{?s}: {.field {missing}}.",
        i = "Re-run your query without dropping these columns:
             {plot_query_hint(data, names(requirements))}"
      ))
    }
  }
  invisible(data)
}

#' Resolve table/column requirements for a prepared plot
#' @keywords internal
#' @noRd
plot_requirements <- function(type, metrics, track = FALSE,
                              count_unit = "storms") {
  req <- list()
  if (type %in% c("exposure", "count")) req$geometry <- "ADM2_ID"
  if (type != "track") req$adm2 <- c("ADM2_ID", "ADM2_NAME")
  for (i in if (type == "track") integer() else seq_len(nrow(metrics))) {
    tab <- metrics$table[i]
    cols <- c("ADM2_ID", metrics$metric[i])
    if (tab == "wind") {
      cols <- c(cols, "STORM_ID", "STORM_DIST_KM")
      if (type == "count" && count_unit == "days") {
        cols <- c(cols, "LOCAL_DATE_STORM_CLOSEST")
      }
    }
    if (tab == "precip") cols <- c(cols, "LOCAL_DATE")
    if (tab == "flood") {
      cols <- c(cols, "FLOOD_ID", "FLOOD_START_DATE", "FLOOD_END_DATE")
    }
    req[[tab]] <- unique(c(req[[tab]], cols))
    if (tab %in% c("precip", "flood")) {
      prefix <- toupper(tab)
      req$links <- unique(c(
        req$links, "ADM2_ID", "STORM_ID", "STORM_DIST_KM",
        "LOCAL_DATE_STORM_CLOSEST", paste0(prefix, "_IN_FIELD"),
        paste0(prefix, "_WINDOW_START"), paste0(prefix, "_WINDOW_END")
      ))
    }
  }
  if (type == "track" || track) {
    req$tracks <- c("STORM_ID", "ISO_TIME", "LAT", "LON", "USA_WIND")
  }
  req
}

#' Validate a scalar logical argument
#' @keywords internal
#' @noRd
plot_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {name}} must be TRUE or FALSE.")
  }
  invisible(x)
}

#' Validate optional placement of per-panel metric scale strips
#' @keywords internal
#' @noRd
plot_validate_track_legend_position <- function(position) {
  if (is.null(position)) return(NULL)
  choices <- c(
    "top", "bottom", "left", "right", "top left", "top right",
    "bottom left", "bottom right"
  )
  if (!is.character(position) || length(position) != 1L || is.na(position) ||
    !position %in% choices) {
    cli::cli_abort("{.arg track_legend_position} must be NULL or one of {.val {choices}}.")
  }
  position
}

#' Parse timestamps to UTC, honoring explicit local offsets
#' @keywords internal
#' @noRd
plot_parse_utc <- function(x, label = "timestamp", require_offset = FALSE) {
  if (inherits(x, "POSIXt")) {
    if (anyNA(x)) {
      cli::cli_abort("{.field {label}} contains missing timestamps.")
    }
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (!is.character(x)) {
    cli::cli_abort("{.field {label}} must contain ISO 8601 timestamps.")
  }
  offset_missing <- !grepl("(Z|[+-][0-9]{2}:?[0-9]{2})$", x) | is.na(x)
  if (require_offset && any(offset_missing)) {
    cli::cli_abort("{.field {label}} must include a UTC offset
                    in every timestamp.")
  }
  out <- lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE)
  if (anyNA(out)) {
    cli::cli_abort("{.field {label}} contains missing or invalid timestamps.")
  }
  out
}

#' Check a requested facet against the actually assembled frame
#' @keywords internal
#' @noRd
validate_plot_facet <- function(data, facet_by) {
  if (is.null(facet_by)) {
    return(invisible(data))
  }
  if (
    !is.character(facet_by) || length(facet_by) != 1L ||
      is.na(facet_by) || !facet_by %in% names(data)
  ) {
    cli::cli_abort(c(
      "Unknown {.arg facet_by} column.",
      i = "Valid columns: {.field {names(data)}}."
    ))
  }
  invisible(data)
}

#' Validate values needed for quantitative scales and field filtering
#' @keywords internal
#' @noRd
validate_plot_values <- function(data, metrics, track = FALSE) {
  for (i in seq_len(nrow(metrics))) {
    metric <- metrics$metric[i]
    value <- data[[metrics$table[i]]][[metric]]
    if (metric != "USA_STATUS" && !is.numeric(value)) {
      cli::cli_abort("Metric {.field {metric}} must be numeric.")
    }
  }
  if (track) {
    for (column in c("LAT", "LON", "USA_WIND")) {
      if (!is.numeric(data$tracks[[column]])) {
        cli::cli_abort("Track column {.field {column}} must be numeric.")
      }
    }
  }
  hazards <- intersect(metrics$table, c("precip", "flood"))
  for (hazard in hazards) {
    column <- paste0(toupper(hazard), "_IN_FIELD")
    if (!is.logical(data$links[[column]])) {
      cli::cli_abort("Links column {.field {column}} must be logical.")
    }
  }
  invisible(data)
}

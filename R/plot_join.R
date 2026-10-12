#' Link published observations to their in-field storm windows
#'
#' Source identities are assigned before the non-equi join. As in the linkage
#' vignette, a shared observation belongs to every matching storm; deduplication
#' is deferred until the target summary groups are known. Unmatched in-field
#' links carry missing observations, never invented zero exposure.
#' @keywords internal
#' @noRd
plot_join_metric <- function(data, spec, flood_match = "start") {
  flood_match <- rlang::arg_match(flood_match, c("start", "overlap"))
  rows <- tibble::as_tibble(data[[spec$table]])
  rows$.plot_row_id <- seq_len(nrow(rows))
  if ("LOCAL_DATE_STORM_CLOSEST" %in% names(rows)) {
    rows$LOCAL_DATE_STORM_CLOSEST <- plot_join_date(
      rows$LOCAL_DATE_STORM_CLOSEST, "LOCAL_DATE_STORM_CLOSEST"
    )
  }
  if (spec$table == "wind") {
    rows$metric <- rep(spec$metric, nrow(rows))
    rows$value <- as.numeric(rows[[spec$metric]])
    return(rows)
  }

  prefix <- toupper(spec$table)
  field <- data$links[data$links[[paste0(prefix, "_IN_FIELD")]] %in% TRUE, ]
  date_columns <- c(
    "LOCAL_DATE_STORM_CLOSEST",
    paste0(prefix, c("_WINDOW_START", "_WINDOW_END"))
  )
  for (column in date_columns) {
    field[[column]] <- plot_join_date(field[[column]], column)
  }
  if (spec$table == "precip") {
    rows$date <- plot_join_date(rows$LOCAL_DATE, "LOCAL_DATE")
    by <- dplyr::join_by(
      ADM2_ID, dplyr::between(date, PRECIP_WINDOW_START, PRECIP_WINDOW_END)
    )
  } else {
    rows$.plot_start <- plot_join_date(
      rows$FLOOD_START_DATE, "FLOOD_START_DATE"
    )
    rows$.plot_end <- plot_join_date(rows$FLOOD_END_DATE, "FLOOD_END_DATE")
    by <- if (flood_match == "start") {
      dplyr::join_by(
        ADM2_ID,
        dplyr::between(".plot_start", FLOOD_WINDOW_START, FLOOD_WINDOW_END)
      )
    } else {
      dplyr::join_by(
        ADM2_ID,
        ".plot_start" <= FLOOD_WINDOW_END, ".plot_end" >= FLOOD_WINDOW_START
      )
    }
  }
  rows <- dplyr::right_join(rows, field,
    by = by, relationship = "many-to-many", na_matches = "never",
    suffix = c("", ".link")
  )
  rows <- dplyr::select(rows, -dplyr::any_of(c(".plot_start", ".plot_end")))
  rows$metric <- rep(spec$metric, nrow(rows))
  rows$value <- as.numeric(rows[[spec$metric]])
  rows
}

#' Parse local calendar dates without timezone conversion
#' @keywords internal
#' @noRd
plot_join_date <- function(x, column) {
  out <- tryCatch(as.Date(x), error = function(e) NULL)
  if (is.null(out) || any(!is.na(x) & is.na(out))) {
    cli::cli_abort("Column {.field {column}} must contain valid
                    calendar dates.")
  }
  out
}

#' Summarize each ADM2, metric, and requested facet at its own source grain
#' @keywords internal
#' @noRd
plot_aggregate_metric <- function(rows, spec, type, count_unit = "storms",
                                  facet_by = NULL, binary = FALSE) {
  keys <- unique(c("ADM2_ID", "metric", facet_by))
  if (!nrow(rows)) {
    out <- rows[, keys, drop = FALSE]
    out$value <- numeric()
    if (binary) out$exposed <- logical()
    return(out)
  }
  groups <- dplyr::group_by(rows, !!!rlang::syms(keys))
  parts <- lapply(dplyr::group_split(groups, .keep = TRUE), function(group) {
    # Keep labels and other unambiguous published metadata. A combined-storm
    # summary must not arbitrarily claim the first storm's name or timestamp.
    constant <- vapply(group, function(x) {
      dplyr::n_distinct(x) == 1L
    }, logical(1))
    omit <- c("value", "exposed", ".plot_row_id", spec$metric)
    columns <- union(keys, setdiff(names(group)[constant], omit))
    out <- group[1L, columns, drop = FALSE]
    if ("STORM_DIST_KM" %in% names(group)) {
      out$STORM_DIST_KM <- plot_summary_value(group$STORM_DIST_KM, "min")
    }

    source <- dplyr::distinct(group, .data$.plot_row_id, .keep_all = TRUE)
    if (type == "count") {
      out$value <- plot_count_values(group, spec, count_unit)
    } else {
      out$value <- plot_summary_value(source$value, spec$aggregate)
    }
    if (binary) {
      qualified <- plot_threshold_values(group$value, spec)
      out$exposed <- if (all(is.na(qualified))) {
        NA
      } else {
        any(qualified, na.rm = TRUE)
      }
    }
    out
  })
  dplyr::bind_rows(parts)
}

#' Reduce observed numeric values while preserving entirely missing groups
#' @keywords internal
#' @noRd
plot_summary_value <- function(value, aggregate) {
  value <- value[!is.na(value)]
  if (!length(value)) {
    return(NA_real_)
  }
  as.numeric(switch(aggregate,
    max = max(value),
    min = min(value),
    sum = sum(value),
    mean = mean(value),
    n = length(value),
    cli::cli_abort("Unknown plot aggregation {.val {aggregate}}.")
  ))
}

#' Classify the original daily, event, or storm-pair observations
#' @keywords internal
#' @noRd
plot_threshold_values <- function(value, spec) {
  switch(spec$operator,
    `>=` = value >= spec$threshold,
    `>` = value > spec$threshold,
    `<=` = value <= spec$threshold,
    cli::cli_abort("Unsupported threshold operator {.val {spec$operator}}.")
  )
}

#' Count distinct qualifying storms or local dates, not duplicated join rows
#' @keywords internal
#' @noRd
plot_count_values <- function(rows, spec, count_unit) {
  qualified <- plot_threshold_values(rows$value, spec)
  if (all(is.na(qualified))) {
    return(NA_real_)
  }
  selected <- rows[qualified %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) {
    return(0)
  }
  if (count_unit == "storms") {
    if (all(is.na(selected$STORM_ID))) {
      return(NA_real_)
    }
    return(as.numeric(dplyr::n_distinct(selected$STORM_ID, na.rm = TRUE)))
  }
  if (spec$table == "flood") {
    return(plot_flood_days(selected))
  }
  dates <- if (spec$table == "precip") {
    selected$date
  } else {
    plot_join_date(
      selected$LOCAL_DATE_STORM_CLOSEST, "LOCAL_DATE_STORM_CLOSEST"
    )
  }
  if (all(is.na(dates))) {
    return(NA_real_)
  }
  as.numeric(dplyr::n_distinct(dates, na.rm = TRUE))
}

#' Count the union of qualifying flood intervals clipped to matching windows
#'
#' Merge intervals rather than expanding long events into individual dates.
#' Shared events retain every matching window until clipping is complete.
#' @keywords internal
#' @noRd
plot_flood_days <- function(rows) {
  start <- pmax(
    plot_join_date(rows$FLOOD_START_DATE, "FLOOD_START_DATE"),
    plot_join_date(rows$FLOOD_WINDOW_START, "FLOOD_WINDOW_START")
  )
  end <- pmin(
    plot_join_date(rows$FLOOD_END_DATE, "FLOOD_END_DATE"),
    plot_join_date(rows$FLOOD_WINDOW_END, "FLOOD_WINDOW_END")
  )
  valid <- !is.na(start) & !is.na(end)
  if (!any(valid)) {
    return(NA_real_)
  }
  valid <- valid & start <= end
  start <- as.numeric(start[valid])
  end <- as.numeric(end[valid])
  if (!length(start)) {
    return(0)
  }
  order <- order(start, end)
  start <- start[order]
  end <- end[order]
  # Each sorted interval contributes only days beyond the union so far.
  previous_end <- c(-Inf, utils::head(cummax(end), -1L))
  sum(pmax(0, end - pmax(start, previous_end + 1) + 1))
}

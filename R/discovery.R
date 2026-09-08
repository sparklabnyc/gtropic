# ---------------------------------------------------------------------------
# Which DOI holds which year?
#
# The collection is split across a historical dataset (1980 onwards, plus the
# only copy of metadata/) and a current-year dataset that is re-minted annually.
# Hardcoding the current-year DOI would break every January, and trusting
# Sys.Date() would break whenever the pipeline runs late. So we ask the server.
# ---------------------------------------------------------------------------

#' Discover the historical and current-year datasets
#'
#' Lists the collection, reads each dataset's file inventory, and decides which
#' is which from the years present. Year coverage is the primary discriminator;
#' the presence of `metadata/` corroborates. When the two signals disagree we
#' warn rather than pick silently, because a wrong assignment sends every
#' subsequent request to the wrong DOI.
#'
#' The result is memoised for the session: discovery costs one request per
#' dataset in the collection and the answer cannot change mid-session in any way
#' the user would want us to notice.
#'
#' @param versions List with `historical` and `current` version strings.
#' @param server Dataverse host.
#' @param collection Collection alias.
#' @param refresh Force rediscovery, ignoring the session cache.
#' @param quiet Suppress progress messages.
#'
#' @return A list with `historical` and `current` (each `doi`, `version`,
#'   `listing`, `years`), plus `year_index`, a tibble of `year` and `dataset`.
#' @keywords internal
#' @noRd
discover_datasets <- function(versions = list(
                                historical = ":latest",
                                current = ":latest"
                              ),
                              server = opt_server(),
                              collection = opt_collection(),
                              refresh = FALSE,
                              quiet = FALSE) {
  key <- paste(server, collection, versions$historical, versions$current,
    sep = "|"
  )
  if (!refresh && !is.null(.gtropic_session[[key]])) {
    return(.gtropic_session[[key]])
  }

  result <- tryCatch(
    discover_datasets_impl(versions, server, collection, quiet),
    error = function(e) {
      # Fall back to the configured DOIs so a discovery outage degrades to the
      # previous behaviour rather than to a hard failure.
      cli::cli_warn(
        c("Automatic dataset discovery failed; falling back to the configured
           DOI options.",
          x = "{conditionMessage(e)}",
          i = "If the current-year DOI has rolled over, set
               {.code options(gtropic.doi_current = )} explicitly."
        )
      )
      discovery_from_options(versions, server, quiet)
    }
  )

  .gtropic_session[[key]] <- result
  result
}

#' @keywords internal
#' @noRd
discover_datasets_impl <- function(versions, server, collection, quiet) {
  inform_unless_quiet("Discovering datasets in collection {.val {collection}}.",
    quiet = quiet
  )

  dois <- dv_list_datasets(
    collection = collection, server = server,
    quiet = quiet
  )
  if (nrow(dois) < 1L) {
    cli::cli_abort("Collection {.val {collection}} contains no datasets.")
  }

  # One listing per dataset. We ask for :latest here regardless of the requested
  # version, because identification is about which dataset is which, not about
  # which snapshot of it we will end up reading.
  profiles <- lapply(dois$doi, function(d) {
    listing <- dv_list_files(d,
      version = ":latest", server = server,
      quiet = quiet
    )
    list(
      doi = d,
      listing = listing,
      years = listing_years(listing),
      has_metadata = any(startsWith(listing$directory_label, "metadata"))
    )
  })

  by_years <- vapply(profiles, function(p) min(c(p$years, Inf)), numeric(1))
  n_years <- vapply(profiles, function(p) length(p$years), integer(1))

  # Primary discriminator: the historical dataset is the long run that starts
  # earliest (1980 by construction).
  hist_idx <- which.min(by_years + (n_years == 0L) * 1e6)
  meta_idx <- which(vapply(profiles, function(p) p$has_metadata, logical(1)))

  if (length(meta_idx) != 1L || !identical(meta_idx, hist_idx)) {
    cli::cli_warn(
      c("Dataset identification signals disagree.",
        i = "Year coverage points at {.val {profiles[[hist_idx]]$doi}} as the
             historical dataset; {length(meta_idx)} dataset{?s} carry a
             {.path metadata/} directory.",
        i = "Proceeding on year coverage. Verify the collection contents."
      )
    )
  }

  hist <- profiles[[hist_idx]]
  others <- profiles[-hist_idx]

  # The current-year dataset is the one contributing years the historical
  # dataset does not have. If several qualify, take the one with the newest year.
  current <- NULL
  if (length(others) > 0L) {
    extra <- vapply(others, function(p) {
      max(c(setdiff(p$years, hist$years), -Inf))
    }, numeric(1))
    if (any(is.finite(extra))) current <- others[[which.max(extra)]]
  }

  if (is.null(current)) {
    # Perfectly legitimate: early in the year, before the current-year dataset
    # has been minted, everything lives in the historical dataset.
    inform_unless_quiet(
      "No separate current-year dataset found; using the historical dataset
       alone.",
      quiet = quiet
    )
  }

  if (!is.null(current) && any(startsWith(
    current$listing$directory_label,
    "metadata"
  ))) {
    cli::cli_warn(
      c("The current-year dataset now contains {.path metadata/} files.",
        i = "There should be exactly one copy, in the historical dataset.",
        i = "Continuing to use the historical copy; the publication contract
             may have changed upstream."
      )
    )
  }

  build_discovery(hist, current, versions, server, quiet)
}

#' @keywords internal
#' @noRd
discovery_from_options <- function(versions, server, quiet) {
  hist_doi <- opt("doi_historical")
  curr_doi <- opt("doi_current")

  hist <- list(
    doi = hist_doi,
    listing = dv_list_files(hist_doi, versions$historical, server, quiet),
    years = NULL
  )
  hist$years <- listing_years(hist$listing)

  current <- NULL
  if (!is.null(curr_doi) && nzchar(curr_doi) &&
    !identical(curr_doi, hist_doi)) {
    current <- tryCatch(
      {
        lst <- dv_list_files(curr_doi, versions$current, server, quiet)
        list(doi = curr_doi, listing = lst, years = listing_years(lst))
      },
      error = function(e) NULL
    )
  }

  build_discovery(hist, current, versions, server, quiet)
}

#' @keywords internal
#' @noRd
build_discovery <- function(hist, current, versions, server, quiet) {
  hist_version <- dv_version_label(hist$doi, versions$historical, server, quiet)

  out <- list(
    server = server,
    historical = list(
      doi = hist$doi,
      version = hist_version,
      listing = hist$listing,
      years = hist$years
    ),
    current = NULL
  )

  if (!is.null(current)) {
    out$current <- list(
      doi = current$doi,
      version = dv_version_label(current$doi, versions$current, server, quiet),
      listing = current$listing,
      years = current$years
    )
  }

  # The year index is the single lookup every later stage uses to decide which
  # DOI a given year-file lives in. Years present in both resolve to the
  # historical dataset: it is the published, citable copy.
  current_only <- setdiff(out$current$years %||% integer(0), hist$years)
  out$year_index <- dplyr::arrange(
    tibble::tibble(
      year = c(hist$years, current_only),
      dataset = c(
        rep("historical", length(hist$years)),
        rep("current", length(current_only))
      )
    ),
    .data$year
  )

  out
}

#' Extract the set of years present in a file listing
#'
#' Year-files are named `YYYY.parquet` in every yearly directory, so the year
#' set is read off the labels. Non-year labels (metadata) are ignored.
#'
#' @param listing Output of `dv_list_files()`.
#' @return Sorted integer vector of years.
#' @keywords internal
#' @noRd
listing_years <- function(listing) {
  labels <- listing$label[grepl("^\\d{4}\\.parquet$", listing$label)]
  if (length(labels) == 0L) {
    return(integer(0))
  }
  sort(unique(as.integer(sub("\\.parquet$", "", labels))))
}

#' Look up which dataset holds a given year
#'
#' @param discovery Output of `discover_datasets()`.
#' @param year Integer year.
#' @return `"historical"`, `"current"`, or `NA_character_` when absent.
#' @keywords internal
#' @noRd
dataset_for_year <- function(discovery, year) {
  idx <- match(as.integer(year), discovery$year_index$year)
  ifelse(is.na(idx), NA_character_, discovery$year_index$dataset[idx])
}

#' Listing and DOI for a named dataset
#'
#' @param discovery Output of `discover_datasets()`.
#' @param which `"historical"` or `"current"`.
#' @return The corresponding element of `discovery`.
#' @keywords internal
#' @noRd
discovery_slot <- function(discovery, which) {
  slot <- discovery[[which]]
  if (is.null(slot)) {
    cli::cli_abort("No {which} dataset is available in this collection.")
  }
  slot
}

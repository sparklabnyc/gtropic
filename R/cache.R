# ---------------------------------------------------------------------------
# The cache.
#
# Granularity is the immutable unit Dataverse serves: one published year-file at
# one dataset version. Filtered results are NEVER cached. Filters are applied at
# read time from cached raw files.
#
# That single decision makes every overlap case fall out for free: asking for
# 2001-2003 and then 2002-2005 re-downloads only 2004-2005; a world-wide pull
# followed by a USA-only pull for the same year downloads nothing. There is no
# subset-containment logic over a 15-dimensional filter space, because there is
# nothing filter-shaped in the cache at all.
#
# Storage is content-addressed by checksum, which deduplicates across dataset
# versions automatically: a year-file unchanged between v1.0 and v2.0 has one
# checksum and is therefore stored once.
# ---------------------------------------------------------------------------

#' Locate the G-TROPIC cache
#'
#' @description
#' Returns the directory used to store downloaded G-TROPIC source files.
#' Reusing these files can reduce data transfer and retrieval time for repeated
#' or overlapping analyses.
#'
#' @details
#' Unless changed through `options(gtropic.cache_dir = ...)` or
#' [gtropic_cache_enable()], the path is
#' `tools::R_user_dir("gtropic", "cache")`. Calling this function reports the
#' path but does not create it.
#'
#' @return A character scalar containing the cache directory path.
#'
#' @examples
#' gtropic_cache_dir()
#'
#' @seealso [gtropic_cache_enable()], [gtropic_cache_clear()]
#' @export
gtropic_cache_dir <- function() {
  opt("cache_dir", tools::R_user_dir("gtropic", "cache"))
}

#' Enable or disable caching
#'
#' @description
#' `gtropic_cache_enable()` enables storage of downloaded G-TROPIC source files
#' for reuse. `gtropic_cache_disable()` stops using the cache for subsequent
#' retrievals but leaves existing files unchanged.
#'
#' @details
#' Caching is enabled by default for each R session. These functions change the
#' `gtropic.cache_enabled` option for the current session. Set `persist = TRUE`
#' to use the same choice in future sessions; this writes the setting under
#' `tools::R_user_dir("gtropic", "config")`. The package never edits
#' `.Rprofile`. Enabling creates the cache directory if needed; disabling does
#' not delete cached files. Use [gtropic_cache_clear()] to delete them.
#'
#' @param dir Optional path for the cache. If `NULL`, the current value returned
#'   by [gtropic_cache_dir()] is used. A custom location may be useful when
#'   exposure files must be stored on a larger disk.
#' @param persist Logical. Should this enabled or disabled state be used in
#'   future R sessions? Defaults to `FALSE`, so the change affects only the
#'   current session.
#'
#' @return Invisibly, a character scalar containing the cache directory path.
#'
#' @examples
#' \donttest{
#' # Bypass the cache for the current session
#' gtropic_cache_disable()
#'
#' # Keep that choice for future sessions
#' gtropic_cache_disable(persist = TRUE)
#' }
#'
#' @seealso [gtropic_cache_dir()], [gtropic_cache_prune()]
#' @export
gtropic_cache_enable <- function(dir = NULL, persist = FALSE) {
  cache_validate_persist(persist)
  if (!is.null(dir)) {
    if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
      cli::cli_abort("{.arg dir} must be a non-empty character path or {.code NULL}.")
    }
    options(gtropic.cache_dir = path.expand(dir))
  }
  options(gtropic.cache_enabled = TRUE)
  if (persist) cache_config_write(TRUE)

  path <- gtropic_cache_dir()
  cache_ensure_dirs(path)
  invisible(path)
}

#' @rdname gtropic_cache_enable
#' @export
gtropic_cache_disable <- function(persist = FALSE) {
  cache_validate_persist(persist)
  options(gtropic.cache_enabled = FALSE)
  if (persist) cache_config_write(FALSE)
  invisible(gtropic_cache_dir())
}

#' Inspect cached G-TROPIC files
#'
#' @description
#' `gtropic_cache_list()` reports the dataset file references recorded in the
#' cache index. `gtropic_cache_size()` reports the physical disk space occupied
#' by cached file content.
#'
#' Cached content is identified by checksum and may be shared by more than one
#' dataset version. Consequently, multiple index entries can refer to one file
#' on disk.
#'
#' @return `gtropic_cache_list()` returns a tibble with one row per indexed
#'   dataset file reference and columns `server`, `doi`, `version`,
#'   `directory_label`, `label`, `checksum`, `size_bytes`, and `fetched_at` (the
#'   original download time, retained when a cached file is reused).
#'   `gtropic_cache_size()` returns a numeric scalar giving the physical cache
#'   size in bytes, or `0` when the cache is empty.
#'
#' @examples
#' gtropic_cache_list()
#' gtropic_cache_size()
#'
#' @export
gtropic_cache_list <- function() {
  cache_index_read(gtropic_cache_dir())
}

#' @rdname gtropic_cache_list
#' @export
gtropic_cache_size <- function() {
  blobs <- list.files(file.path(gtropic_cache_dir(), "blobs"), full.names = TRUE)
  if (length(blobs) == 0L) {
    return(0)
  }
  sum(file.info(blobs)$size, na.rm = TRUE)
}

#' Delete all cached G-TROPIC files
#'
#' Removes all downloaded file content from the configured cache and clears its
#' index. This operation does not change whether caching is enabled.
#'
#' @param confirm Logical. If `TRUE` (the default), confirmation is requested
#'   before deletion in an interactive R session. No prompt is issued in a
#'   non-interactive session. Set to `FALSE` to proceed without prompting.
#'
#' @return Invisibly, the number of cached files selected for removal. Returns
#'   `0` if the cache is empty or interactive confirmation is declined.
#'
#' @examples
#' \donttest{
#' gtropic_cache_clear(confirm = FALSE)
#' }
#'
#' @export
gtropic_cache_clear <- function(confirm = TRUE) {
  dir <- gtropic_cache_dir()
  blobs <- list.files(file.path(dir, "blobs"), full.names = TRUE)

  if (length(blobs) == 0L) {
    cli::cli_alert_info("Cache is already empty.")
    return(invisible(0L))
  }

  if (isTRUE(confirm) && is_interactive()) {
    cli::cli_inform(
      "About to delete {length(blobs)} cached file{?s}
       ({format_bytes(gtropic_cache_size())}) from {.path {dir}}."
    )
    ans <- readline("Proceed? [y/N] ")
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled.")
      return(invisible(0L))
    }
  }

  unlink(blobs)
  cache_index_write(dir, cache_index_empty())
  cli::cli_alert_success("Removed {length(blobs)} cached file{?s}.")
  invisible(length(blobs))
}

#' Prune cached G-TROPIC files by size or age
#'
#' @description
#' Removes cache entries according to their original retrieval time. Entries
#' older than the specified age are removed, and the oldest remaining entries
#' are removed as needed to satisfy the size limit. When both criteria are
#' supplied, an entry is removed if required by either criterion. With no
#' arguments, nothing is removed.
#'
#' If identical file content is referenced by multiple dataset versions, the
#' physical file is retained until no remaining cache entry references it.
#'
#' @param max_size_gb Optional maximum indexed cache size in gigabytes. The
#'   oldest entries are removed until the indexed size is at or below this
#'   value.
#' @param older_than_days Optional age threshold in days. Entries fetched before
#'   the resulting cutoff are removed.
#'
#' @return Invisibly, a tibble containing the removed cache-index entries. An
#'   empty tibble is returned when no entries are removed.
#'
#' @examples
#' \donttest{
#' gtropic_cache_prune(max_size_gb = 5, older_than_days = 90)
#' }
#'
#' @export
gtropic_cache_prune <- function(max_size_gb = NULL, older_than_days = NULL) {
  dir <- gtropic_cache_dir()
  idx <- cache_index_read(dir)
  if (nrow(idx) == 0L) {
    cli::cli_alert_info("Cache is empty; nothing to prune.")
    return(invisible(idx[0, , drop = FALSE]))
  }

  drop <- rep(FALSE, nrow(idx))

  if (!is.null(older_than_days)) {
    cutoff <- Sys.time() - as.numeric(older_than_days) * 86400
    drop <- drop | (idx$fetched_at < cutoff)
  }

  if (!is.null(max_size_gb)) {
    # LRU: sort newest first, keep taking until the budget is exhausted.
    budget <- as.numeric(max_size_gb) * 1024^3
    ord <- order(idx$fetched_at, decreasing = TRUE)
    running <- cumsum(idx$size_bytes[ord])
    over <- ord[running > budget]
    drop[over] <- TRUE
  }

  if (!any(drop)) {
    cli::cli_alert_info("Nothing to prune.")
    return(invisible(idx[0, , drop = FALSE]))
  }

  removed <- idx[drop, , drop = FALSE]
  kept <- idx[!drop, , drop = FALSE]

  # A blob may be referenced by more than one index entry (the same bytes at two
  # dataset versions). Only delete blobs no surviving entry still points at.
  orphans <- setdiff(unique(removed$checksum), unique(kept$checksum))
  unlink(file.path(dir, "blobs", paste0(orphans, ".blob")))

  cache_index_write(dir, kept)
  cli::cli_alert_success(
    "Pruned {nrow(removed)} entr{?y/ies}, freeing
     {format_bytes(sum(removed$size_bytes, na.rm = TRUE))}."
  )
  invisible(removed)
}

# --- internals -------------------------------------------------------------

#' @keywords internal
#' @noRd
cache_index_empty <- function() {
  tibble::tibble(
    server = character(0),
    doi = character(0),
    version = character(0),
    directory_label = character(0),
    label = character(0),
    checksum = character(0),
    size_bytes = numeric(0),
    fetched_at = as.POSIXct(character(0))
  )
}

#' @keywords internal
#' @noRd
cache_index_path <- function(dir) file.path(dir, "index.rds")

#' @keywords internal
#' @noRd
cache_ensure_dirs <- function(dir) {
  dir.create(file.path(dir, "blobs"), recursive = TRUE, showWarnings = FALSE)
  invisible(dir)
}

#' @keywords internal
#' @noRd
cache_index_read <- function(dir) {
  path <- cache_index_path(dir)
  if (!file.exists(path)) {
    return(cache_index_empty())
  }
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(out)) {
    cli::cli_warn("Cache index at {.path {path}} was unreadable; treating the
                   cache as empty.")
    return(cache_index_empty())
  }
  out <- tibble::as_tibble(out)
  required <- names(cache_index_empty())
  if (!identical(names(out), required)) {
    cli::cli_abort(
      "Cache index at {.path {path}} does not match the current cache format. Clear the cache and retrieve the files again."
    )
  }
  out$fetched_at <- as.POSIXct(out$fetched_at, origin = "1970-01-01", tz = "UTC")
  out
}

#' @keywords internal
#' @noRd
cache_index_write <- function(dir, idx) {
  cache_ensure_dirs(dir)
  # Write-then-rename so an interrupted write cannot leave a truncated index.
  tmp <- paste0(cache_index_path(dir), ".tmp")
  saveRDS(idx, tmp)
  file.rename(tmp, cache_index_path(dir))
  invisible(idx)
}

#' Resolve whether the cache may be used
#'
#' The cache is enabled by default. Its directory is created only when a
#' retrieval actually needs it, never while the package is loading.
#'
#' @param cache The call-level `cache` argument (`NULL` inherits the option).
#' @param quiet Suppress informational output.
#'
#' @return A list with `enabled` (logical) and `dir` (path).
#' @keywords internal
#' @noRd
cache_resolve <- function(cache = NULL, quiet = FALSE) {
  enabled <- cache %||% isTRUE(opt("cache_enabled", TRUE))
  if (!isTRUE(enabled)) {
    return(list(enabled = FALSE, dir = NULL))
  }

  dir <- gtropic_cache_dir()
  list(enabled = TRUE, dir = dir)
}

#' Return the location for persistent cache settings
#'
#' @return A character scalar.
#' @keywords internal
#' @noRd
cache_config_dir <- function() tools::R_user_dir("gtropic", "config")

#' @keywords internal
#' @noRd
cache_config_path <- function() file.path(cache_config_dir(), "cache.rds")

#' Read the persisted cache setting without creating files or directories
#'
#' @return `TRUE`, `FALSE`, or `NULL` when no valid setting is available.
#' @keywords internal
#' @noRd
cache_config_read <- function() {
  path <- cache_config_path()
  if (!file.exists(path)) return(NULL)

  settings <- tryCatch(readRDS(path), error = function(e) NULL)
  value <- if (is.list(settings)) settings$cache_enabled else NULL
  if (is.logical(value) && length(value) == 1L && !is.na(value)) value else NULL
}

#' Write the persisted cache setting
#'
#' @param enabled Logical scalar.
#' @return Invisibly, `enabled`.
#' @keywords internal
#' @noRd
cache_config_write <- function(enabled) {
  dir <- cache_config_dir()
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    cli::cli_abort("Could not create the gtropic configuration directory {.path {dir}}.")
  }

  path <- cache_config_path()
  tmp <- paste0(path, ".tmp")
  saveRDS(list(cache_enabled = enabled), tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    cli::cli_abort("Could not save the gtropic cache setting at {.path {path}}.")
  }
  invisible(enabled)
}

#' @keywords internal
#' @noRd
cache_validate_persist <- function(persist) {
  if (!is.logical(persist) || length(persist) != 1L || is.na(persist)) {
    cli::cli_abort("{.arg persist} must be a single `TRUE` or `FALSE` value.")
  }
  invisible(persist)
}

#' Retrieve a cached file's bytes
#'
#' @param cache Resolved cache list from `cache_resolve()`.
#' @param key Named list with `server`, `doi`, `version`, `directory_label`,
#'   `label`.
#' @param expected_checksum Checksum from the live listing, when known.
#'
#' @return A raw vector, or `NULL` on a miss.
#' @keywords internal
#' @noRd
cache_get <- function(cache, key, expected_checksum = NA_character_) {
  if (!isTRUE(cache$enabled)) {
    return(NULL)
  }

  idx <- cache_index_read(cache$dir)
  hit <- idx[
    idx$server == key$server & idx$doi == key$doi &
      idx$version == key$version &
      idx$directory_label == key$directory_label & idx$label == key$label, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) {
    return(NULL)
  }

  blob <- file.path(cache$dir, "blobs", paste0(hit$checksum[1], ".blob"))
  if (!file.exists(blob)) {
    # Index and blobs out of step: drop the dangling entry and miss.
    cache_index_write(cache$dir, idx[!(idx$checksum %in% hit$checksum[1]), , drop = FALSE])
    return(NULL)
  }

  raw <- readBin(blob, "raw", n = file.info(blob)$size)

  # Verify on read as well as on write: silent bit-rot in a cache would be
  # indistinguishable from a data error in someone's analysis.
  reference <- if (!is.na(expected_checksum) && nzchar(expected_checksum)) {
    expected_checksum
  } else {
    hit$checksum[1]
  }
  got <- checksum_raw(raw, reference)

  if (!identical(tolower(got), tolower(reference))) {
    cli::cli_warn(
      c("Cached copy of {.path {file.path(key$directory_label, key$label)}}
         failed checksum verification.",
        i = "Discarding it and downloading a fresh copy."
      )
    )
    unlink(blob)
    cache_index_write(cache$dir, idx[idx$checksum != hit$checksum[1], , drop = FALSE])
    return(NULL)
  }

  raw
}

#' Retrieve cached bytes together with their original acquisition time
#' @keywords internal
#' @noRd
cache_get_record <- function(cache, key, expected_checksum = NA_character_) {
  raw <- cache_get(cache, key, expected_checksum)
  if (is.null(raw)) return(NULL)
  idx <- cache_index_read(cache$dir)
  hit <- idx[
    idx$server == key$server & idx$doi == key$doi &
      idx$version == key$version &
      idx$directory_label == key$directory_label & idx$label == key$label, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) return(NULL)
  list(raw = raw, downloaded_at = hit$fetched_at[1L])
}

#' Store a file's bytes in the cache
#'
#' @param cache Resolved cache list.
#' @param key Named list as for `cache_get()`.
#' @param raw Raw vector to store.
#' @param checksum Published checksum.
#'
#' @return Original download time recorded for the cache entry.
#' @keywords internal
#' @noRd
cache_put <- function(cache, key, raw, checksum = NA_character_,
                      downloaded_at = Sys.time()) {
  if (!isTRUE(cache$enabled)) {
    return(as.POSIXct(downloaded_at, origin = "1970-01-01", tz = "UTC"))
  }

  if (is.na(checksum) || !nzchar(checksum)) checksum <- checksum_raw(raw)

  cache_ensure_dirs(cache$dir)
  blob <- file.path(cache$dir, "blobs", paste0(checksum, ".blob"))

  # Content-addressed: identical bytes at two dataset versions share one blob.
  if (!file.exists(blob)) {
    tmp <- paste0(blob, ".tmp")
    writeBin(raw, tmp)
    file.rename(tmp, blob)
  }

  idx <- cache_index_read(cache$dir)
  entry <- tibble::tibble(
    server = key$server, doi = key$doi, version = key$version,
    directory_label = key$directory_label, label = key$label,
    checksum = checksum, size_bytes = length(raw), fetched_at = downloaded_at
  )
  idx <- dplyr::bind_rows(
    idx[!(idx$server == key$server & idx$doi == key$doi &
      idx$version == key$version &
      idx$directory_label == key$directory_label &
      idx$label == key$label), , drop = FALSE],
    entry
  )
  cache_index_write(cache$dir, idx)

  as.POSIXct(downloaded_at, origin = "1970-01-01", tz = "UTC")
}

#' Is a planned file already cached?
#'
#' Used by the download guardrail so the size estimate reflects what will
#' actually be transferred.
#'
#' @param cache Resolved cache list.
#' @param key Named list as for `cache_get()`.
#' @param expected_checksum Checksum from the resolved dataset listing, when known.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
cache_has <- function(cache, key, expected_checksum = NA_character_) {
  if (!isTRUE(cache$enabled)) {
    return(FALSE)
  }
  idx <- cache_index_read(cache$dir)
  hit <- idx$server == key$server & idx$doi == key$doi &
    idx$version == key$version &
    idx$directory_label == key$directory_label & idx$label == key$label
  if (!is.na(expected_checksum) && nzchar(expected_checksum)) {
    hit <- hit & tolower(idx$checksum) == tolower(expected_checksum)
  }
  any(hit & file.exists(file.path(cache$dir, "blobs", paste0(idx$checksum, ".blob"))))
}

# --- persisted discovery snapshots -----------------------------------------

#' @keywords internal
#' @noRd
cache_discovery_path <- function(dir) file.path(dir, "discovery.rds")

#' Persist a concrete discovery result for intentional offline use
#' @keywords internal
#' @noRd
cache_discovery_write <- function(dir, discovery, collection) {
  if (is.null(dir)) return(invisible(FALSE))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- cache_discovery_path(dir)
  tmp <- paste0(path, ".tmp")
  saveRDS(list(
    saved_at = Sys.time(), server = discovery$server, collection = collection,
    discovery = discovery
  ), tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    cli::cli_warn("Could not save the discovery snapshot at {.path {path}}.")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' Read and validate a persisted discovery result
#' @keywords internal
#' @noRd
cache_discovery_read <- function(dir, server, collection, versions,
                                 call = rlang::caller_env()) {
  path <- cache_discovery_path(dir)
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Offline retrieval needs a persisted discovery snapshot.",
      i = "Run the query online once with caching enabled before using {.code offline = TRUE}."
    ), call = call)
  }
  snapshot <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(snapshot) || is.null(snapshot$discovery) || is.null(snapshot$saved_at)) {
    cli::cli_abort("The discovery snapshot at {.path {path}} is unreadable.", call = call)
  }
  if (!identical(snapshot$server, server) || !identical(snapshot$collection, collection)) {
    cli::cli_abort(c(
      "The persisted discovery snapshot is for a different server or collection.",
      i = "Snapshot: {.val {snapshot$server}} / {.val {snapshot$collection}}.",
      i = "Requested: {.val {server}} / {.val {collection}}."
    ), call = call)
  }
  discovered <- snapshot$discovery
  requested <- unlist(versions, use.names = TRUE)
  concrete <- c(
    historical = discovered$historical$version,
    current = discovered$current$version %||% NA_character_
  )
  incompatible <- names(requested)[requested != ":latest" & requested != concrete[names(requested)]]
  if (length(incompatible) > 0L) {
    cli::cli_abort(c(
      "The discovery snapshot does not contain the requested dataset version{?s}.",
      x = "Incompatible dataset{?s}: {.field {incompatible}}."
    ), call = call)
  }
  snapshot
}

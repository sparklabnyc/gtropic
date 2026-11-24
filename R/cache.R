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
#' @family cache functions
#' @export
gtropic_cache_dir <- function() {
  opt("cache_dir")
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
#' The first time in a session that a file is cached, a message says where.
#' The cache is managed automatically: after each retrieval that adds to it,
#' files downloaded more than `getOption("gtropic.cache_max_age_days")` (180)
#' days ago, files superseded by a newer dataset version retrieved with
#' `version = ":latest"`, and then the least recently downloaded files beyond
#' `getOption("gtropic.cache_max_gb")` (10) gigabytes are removed. Files used
#' by that retrieval are never removed. Set either option to `Inf` to switch
#' its rule off; see [gtropic_options].
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
#' \dontshow{
#' .old <- options(
#'   gtropic.cache_dir = file.path(tempdir(), "gtropic-cache"),
#'   gtropic.cache_enabled = getOption("gtropic.cache_enabled")
#' )
#' }
#' # Stop caching for the rest of this session, then start again
#' gtropic_cache_disable()
#' gtropic_cache_enable()
#'
#' \dontrun{
#' # Not run: persist = TRUE writes the choice to your configuration directory
#' gtropic_cache_disable(persist = TRUE)
#' }
#' \dontshow{
#' unlink(file.path(tempdir(), "gtropic-cache"), recursive = TRUE)
#' options(.old)
#' }
#'
#' @family cache functions
#' @export
gtropic_cache_enable <- function(dir = NULL, persist = FALSE) {
  check_flag(persist)
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
  check_flag(persist)
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
#' \dontshow{
#' .old <- options(
#'   gtropic.cache_dir = file.path(tempdir(), "gtropic-cache"),
#'   gtropic.cache_enabled = getOption("gtropic.cache_enabled")
#' )
#' }
#' gtropic_cache_list()
#' gtropic_cache_size()
#' \dontshow{
#' unlink(file.path(tempdir(), "gtropic-cache"), recursive = TRUE)
#' options(.old)
#' }
#'
#' @family cache functions
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
#' \dontshow{
#' .old <- options(
#'   gtropic.cache_dir = file.path(tempdir(), "gtropic-cache"),
#'   gtropic.cache_enabled = getOption("gtropic.cache_enabled")
#' )
#' }
#' gtropic_cache_enable()
#' gtropic_cache_clear(confirm = FALSE)
#' \dontshow{
#' unlink(file.path(tempdir(), "gtropic-cache"), recursive = TRUE)
#' options(.old)
#' }
#'
#' @family cache functions
#' @export
gtropic_cache_clear <- function(confirm = TRUE) {
  check_flag(confirm)
  dir <- gtropic_cache_dir()
  blobs <- list.files(file.path(dir, "blobs"), full.names = TRUE)

  if (length(blobs) == 0L) {
    inform_unless_quiet(c(i = "Cache is already empty."), quiet = opt("quiet"))
    return(invisible(0L))
  }

  if (confirm && is_interactive()) {
    proceed <- confirm_action(
      "About to delete {length(blobs)} cached file{?s}
       ({format_bytes(gtropic_cache_size())}) from {.path {dir}}."
    )
    if (!proceed) {
      cli::cli_alert_info("Cancelled.")
      return(invisible(0L))
    }
  }

  unlink(blobs, recursive = FALSE, force = FALSE)
  cache_index_write(dir, cache_index_empty())
  inform_unless_quiet(c(v = "Removed {length(blobs)} cached file{?s}."),
    quiet = opt("quiet")
  )
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
#' The cache is also pruned automatically after retrievals; see
#' [gtropic_cache_enable()] and [gtropic_options].
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
#' \dontshow{
#' .old <- options(
#'   gtropic.cache_dir = file.path(tempdir(), "gtropic-cache"),
#'   gtropic.cache_enabled = getOption("gtropic.cache_enabled")
#' )
#' }
#' gtropic_cache_enable()
#' gtropic_cache_prune(max_size_gb = 5, older_than_days = 90)
#' \dontshow{
#' unlink(file.path(tempdir(), "gtropic-cache"), recursive = TRUE)
#' options(.old)
#' }
#'
#' @family cache functions
#' @export
gtropic_cache_prune <- function(max_size_gb = NULL, older_than_days = NULL) {
  dir <- gtropic_cache_dir()
  idx <- cache_index_read(dir)
  if (nrow(idx) == 0L) {
    inform_unless_quiet(c(i = "Cache is empty; nothing to prune."),
      quiet = opt("quiet")
    )
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
    inform_unless_quiet(c(i = "Nothing to prune."), quiet = opt("quiet"))
    return(invisible(idx[0, , drop = FALSE]))
  }

  removed <- cache_remove_entries(dir, idx, drop)
  inform_unless_quiet(
    c(v = "Pruned {nrow(removed)} entr{?y/ies}, freeing
           {format_bytes(sum(removed$size_bytes, na.rm = TRUE))}."),
    quiet = opt("quiet")
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
    fetched_at = as.POSIXct(character(0), tz = "UTC")
  )
}

#' @keywords internal
#' @noRd
cache_index_path <- function(dir) file.path(dir, "index.json")

#' Path of the cached file content for a checksum
#'
#' Blobs are named by checksum. A checksum read from a tampered cache index
#' could otherwise point outside the cache (`"../../x"`), so anything that is
#' not a plain hexadecimal digest is refused.
#'
#' @param dir Cache directory.
#' @param checksum Character vector of checksums.
#' @param call Calling environment for error reporting.
#'
#' @return Character vector of paths under `<dir>/blobs`.
#' @keywords internal
#' @noRd
cache_blob_path <- function(dir, checksum, call = rlang::caller_env()) {
  ok <- is_hex_checksum(checksum)
  if (!all(ok)) {
    cli::cli_abort(
      c("Refusing to use an invalid checksum as a cache file name.",
        x = "Got {.val {checksum[!ok]}}.",
        i = "The cache index may be corrupt; {.code gtropic_cache_clear()}
             resets it."
      ),
      call = call
    )
  }
  file.path(dir, "blobs", paste0(tolower(checksum), ".blob", recycle0 = TRUE))
}

#' @keywords internal
#' @noRd
cache_ensure_dirs <- function(dir) {
  # Private to the user: whoever can write here can change what is read back.
  dir.create(file.path(dir, "blobs"),
    recursive = TRUE, showWarnings = FALSE, mode = "0700"
  )
  cache_check_permissions(dir)
  cache_sweep_tmp(dir)
  invisible(dir)
}

#' Warn once if the cache directory is writable by other users
#'
#' Relocating the cache to a shared disk is supported, so this only warns.
#'
#' @param dir Cache directory.
#' @return Invisibly `TRUE` when the permissions are private (or not checked).
#' @keywords internal
#' @noRd
cache_check_permissions <- function(dir) {
  if (!identical(.Platform$OS.type, "unix") || !dir.exists(dir)) {
    return(invisible(TRUE))
  }
  mode <- file.info(dir, extra_cols = FALSE)$mode
  # 022: group- or world-writable.
  if (is.na(mode) || bitwAnd(as.integer(mode), 18L) == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_warn(
    c("The gtropic cache directory {.path {dir}} is writable by other users.",
      i = "They could change the cached metadata your queries read.
           {.code Sys.chmod(\"{dir}\", \"0700\")} makes it private."
    ),
    .frequency = "once",
    .frequency_id = paste0("gtropic_cache_permissions_", dir)
  )
  invisible(FALSE)
}

#' Read the cache index
#'
#' A missing or invalid `index.json` reads as an empty index (an invalid one
#' with a warning); see `cache_read_json()`.
#'
#' @param dir Cache directory.
#' @return A tibble shaped like `cache_index_empty()`.
#' @keywords internal
#' @noRd
cache_index_read <- function(dir) {
  cache_read_json(cache_index_path(dir), cache_index_schema,
    empty = cache_index_empty()
  )
}

#' @keywords internal
#' @noRd
cache_index_write <- function(dir, idx) {
  cache_ensure_dirs(dir)
  out <- as.data.frame(idx)[names(CACHE_INDEX_FIELDS)]
  out$fetched_at <- format_cache_time(out$fetched_at)
  path <- cache_index_path(dir)
  if (!cache_write_json(path, out)) {
    cli::cli_warn("Could not update the cache index at {.path {path}}.")
  }
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
cache_resolve <- function(cache = NULL, quiet = FALSE,
                          call = rlang::caller_env()) {
  check_flag(cache, "cache", allow_null = TRUE, call = call)
  enabled <- cache %||% opt("cache_enabled")
  if (!isTRUE(enabled)) {
    return(list(enabled = FALSE, dir = NULL))
  }

  dir <- gtropic_cache_dir()
  cache_check_permissions(dir)
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
cache_config_path <- function() file.path(cache_config_dir(), "config.json")

#' Read the persisted cache setting without creating files or directories
#'
#' @return `TRUE`, `FALSE`, or `NULL` when no valid setting is available.
#' @keywords internal
#' @noRd
cache_config_read <- function() {
  cache_read_json(cache_config_path(), cache_config_schema, empty = NULL)
}

#' Write the persisted cache setting
#'
#' @param enabled Logical scalar.
#' @return Invisibly, `enabled`.
#' @keywords internal
#' @noRd
cache_config_write <- function(enabled) {
  dir <- cache_config_dir()
  if (!dir.exists(dir) &&
    !dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")) {
    cli::cli_abort("Could not create the gtropic configuration directory {.path {dir}}.")
  }

  path <- cache_config_path()
  if (!cache_write_json(path, list(cache_enabled = jsonlite::unbox(enabled)))) {
    cli::cli_abort("Could not save the gtropic cache setting at {.path {path}}.")
  }
  invisible(enabled)
}

#' Find a cached file and verify it
#'
#' The blob is hashed from disk (in chunks, never read into an R vector), at
#' most once per session (see `cache_blob_verified()`).
#'
#' @param cache Resolved cache list from `cache_resolve()`, possibly carrying
#'   a query's index handle (see `cache_index_handle()`).
#' @param key Named list with `server`, `doi`, `version`, `directory_label`,
#'   `label`.
#' @param expected_checksum Checksum from the live listing, when known.
#'
#' @return `NULL` on a miss, otherwise a list with `path`, the verified blob,
#'   and `downloaded_at`, the original acquisition time stored in the index.
#' @keywords internal
#' @noRd
cache_get <- function(cache, key, expected_checksum = NA_character_) {
  if (!isTRUE(cache$enabled)) {
    return(NULL)
  }

  idx <- cache_index_get(cache)
  hit <- idx[cache_key_match(idx, key), , drop = FALSE]
  if (nrow(hit) == 0L) {
    return(NULL)
  }

  checksum <- hit$checksum[1]
  blob <- cache_blob_path(cache$dir, checksum)
  if (!file.exists(blob)) {
    # Index and blobs out of step: drop the dangling entry and miss.
    cache_index_drop(cache, checksum)
    return(NULL)
  }

  # Verify on read as well as on write: silent bit-rot in a cache would be
  # indistinguishable from a data error in someone's analysis. Once per
  # session is enough (PERF-03): later uses rely on that check while the file
  # is unchanged on disk.
  reference <- if (!is.na(expected_checksum) && nzchar(expected_checksum)) {
    expected_checksum
  } else {
    checksum
  }
  if (!cache_blob_verified(blob, reference)) {
    got <- checksum_file(blob, reference)
    if (!identical(tolower(got), tolower(reference))) {
      cli::cli_warn(
        c("Cached copy of {.path {file.path(key$directory_label, key$label)}}
           failed checksum verification.",
          i = "Discarding it and downloading a fresh copy."
        )
      )
      unlink(blob)
      cache_blob_forget(blob)
      cache_index_drop(cache, checksum)
      return(NULL)
    }
    cache_blob_mark_verified(blob, reference)
  }

  list(path = blob, downloaded_at = hit$fetched_at[1L])
}

#' Remember which cached blobs were verified in this session
#'
#' Hashing a cached file proves it intact when it is used. Hashing it again
#' for every query of a session would re-read gigabytes, so each blob is
#' verified at most once per session, against the checksum it was verified
#' for. The record also holds the file's size and modification time, and a
#' blob that has changed on disk since is verified again.
#'
#' @param blob Blob path.
#' @param checksum Checksum the blob was verified against.
#'
#' @return `cache_blob_verified()`: logical scalar. The others: invisibly
#'   `blob`.
#' @keywords internal
#' @noRd
cache_blob_verified <- function(blob, checksum) {
  record <- .gtropic_session$verified_blobs[[blob]]
  if (is.null(record) || !identical(record$checksum, tolower(checksum))) {
    return(FALSE)
  }
  info <- file.info(blob, extra_cols = FALSE)
  identical(record$size, info$size) && identical(record$mtime, info$mtime)
}

#' @rdname cache_blob_verified
#' @keywords internal
#' @noRd
cache_blob_mark_verified <- function(blob, checksum) {
  if (is.null(.gtropic_session$verified_blobs)) {
    .gtropic_session$verified_blobs <- new.env(parent = emptyenv())
  }
  info <- file.info(blob, extra_cols = FALSE)
  .gtropic_session$verified_blobs[[blob]] <- list(
    checksum = tolower(checksum), size = info$size, mtime = info$mtime
  )
  invisible(blob)
}

#' @rdname cache_blob_verified
#' @keywords internal
#' @noRd
cache_blob_forget <- function(blob) {
  verified <- .gtropic_session$verified_blobs
  if (!is.null(verified) && exists(blob, envir = verified, inherits = FALSE)) {
    rm(list = blob, envir = verified)
  }
  invisible(blob)
}

#' Where to download a file before it is verified
#'
#' With the cache enabled, inside its `blobs/` directory, so that storing the
#' verified file is a rename on the same disk. Otherwise in the session's
#' temporary directory. The `.tmp-` prefix lets `cache_sweep_tmp()` remove
#' files left by an interrupted download.
#'
#' @param cache Resolved cache list.
#' @return A path that does not exist yet.
#' @keywords internal
#' @noRd
cache_download_path <- function(cache) {
  if (!isTRUE(cache$enabled)) {
    return(tempfile(pattern = "gtropic-", fileext = ".download"))
  }
  cache_ensure_dirs(cache$dir)
  tempfile(pattern = ".tmp-", tmpdir = file.path(cache$dir, "blobs"))
}

#' Store a downloaded file in the cache
#'
#' The file (already verified against its published checksum) is moved into
#' the content-addressed blob store; if identical content is already stored,
#' the file is left where it is for the caller to remove.
#'
#' @param cache Resolved cache list.
#' @param key Named list as for `cache_get()`.
#' @param path The downloaded file.
#' @param checksum Published checksum.
#' @param downloaded_at Download time to record.
#' @param quiet Suppress the first-use notice.
#'
#' @return A list with `path`, the file to read from now on (the blob, or
#'   `path` itself when the cache is disabled or the file could not be
#'   stored), and `downloaded_at`, the original download time recorded for the
#'   cache entry, in whole seconds (as stored in the index, so a later cache
#'   hit reports the same time).
#' @keywords internal
#' @noRd
cache_put <- function(cache, key, path, checksum = NA_character_,
                      downloaded_at = Sys.time(), quiet = FALSE) {
  if (!isTRUE(cache$enabled)) {
    return(list(
      path = path,
      downloaded_at = as.POSIXct(downloaded_at, origin = "1970-01-01", tz = "UTC")
    ))
  }
  downloaded_at <- cache_time(downloaded_at)
  cache_first_use_notice(cache$dir, quiet = quiet)

  if (is.na(checksum) || !nzchar(checksum)) checksum <- checksum_file(path)
  size_bytes <- file.size(path)

  cache_ensure_dirs(cache$dir)
  blob <- cache_blob_path(cache$dir, checksum)

  # Content-addressed: identical bytes at two dataset versions share one blob.
  if (!file.exists(blob)) {
    stored <- isTRUE(suppressWarnings(file.rename(path, blob))) ||
      cache_write_atomic(blob, function(tmp) file.copy(path, tmp))
    # Another session may have stored the same bytes in the meantime.
    if (!stored && !file.exists(blob)) {
      cli::cli_warn(c(
        "Could not store {.path {file.path(key$directory_label, key$label)}}
         in the cache.",
        i = "The downloaded copy is still used for this result."
      ))
      return(list(path = path, downloaded_at = downloaded_at))
    }
    # These are the bytes just checked against the published checksum (or
    # hashed above), so their first use from the cache needs no new hash. A
    # blob that was already there has not been checked by this session.
    if (stored) cache_blob_mark_verified(blob, checksum)
  }

  cache_index_upsert(cache, tibble::tibble(
    server = key$server, doi = key$doi, version = key$version,
    directory_label = key$directory_label, label = key$label,
    checksum = checksum, size_bytes = size_bytes, fetched_at = downloaded_at
  ))

  list(path = blob, downloaded_at = downloaded_at)
}

#' Which planned files are already cached?
#'
#' Used by the download guardrail so the size estimate reflects what will
#' actually be transferred. One lookup for the whole plan.
#'
#' @param cache Resolved cache list.
#' @param keys A data frame (or a list of scalars) with the key columns
#'   `server`, `doi`, `version`, `directory_label` and `label`, and optionally
#'   `checksum`: the checksum from the resolved dataset listing, when known.
#'   An entry with a known checksum counts as cached only when the cached
#'   copy has that checksum.
#'
#' @return Logical vector, one element per row of `keys`.
#' @keywords internal
#' @noRd
cache_has <- function(cache, keys) {
  keys <- tibble::as_tibble(keys)
  out <- rep(FALSE, nrow(keys))
  if (!isTRUE(cache$enabled) || nrow(keys) == 0L) {
    return(out)
  }
  idx <- cache_index_get(cache)
  key_of <- function(x) {
    cache_key_string(x$server, x$doi, x$directory_label, x$label, x$version)
  }
  wanted <- key_of(keys)
  # Only entries for planned files, and only those whose blob is present.
  idx <- idx[key_of(idx) %in% wanted, , drop = FALSE]
  idx <- idx[file.exists(cache_blob_path(cache$dir, idx$checksum)), , drop = FALSE]
  if (nrow(idx) == 0L) {
    return(out)
  }

  have <- key_of(idx)
  expected <- keys$checksum %||% rep(NA_character_, nrow(keys))
  known <- !is.na(expected) & nzchar(expected)
  ifelse(known,
    paste(wanted, tolower(expected)) %in% paste(have, tolower(idx$checksum)),
    wanted %in% have
  )
}

#' Which index entries have a given key?
#'
#' @param idx The cache index.
#' @param key Named list (or one-row data frame) with `server`, `doi`,
#'   `version`, `directory_label` and `label`.
#' @return Logical vector over the rows of `idx`.
#' @keywords internal
#' @noRd
cache_key_match <- function(idx, key) {
  idx$server == key$server & idx$doi == key$doi &
    idx$version == key$version &
    idx$directory_label == key$directory_label & idx$label == key$label
}

# --- the index of one query (PERF-04) ---------------------------------------

#' An in-memory view of the cache index for one query
#'
#' A query used to read `index.json` for every file it looked up and rewrite
#' it for every file it stored. A fetch context instead carries one handle:
#' the index is read once, on first use, lookups are answered from memory,
#' and the query's changes are collected and written once, by
#' `cache_index_flush()`, when the query ends (`cache_manage_ctx()`, run on
#' exit, so an interrupted query still records what it downloaded).
#'
#' The flush re-reads the file and applies the query's changes to it, rather
#' than writing the query's snapshot back, so entries another R session added
#' in the meantime are kept.
#'
#' Without a handle (a bare `cache_resolve()` list), `cache_get()` and
#' `cache_put()` read and write the file directly.
#'
#' @param dir Cache directory.
#' @return An environment: `idx` (the index, `NULL` until first use),
#'   `upserts` (entries this query stored), `dropped` (checksums whose entries
#'   it removed) and `dirty`.
#' @keywords internal
#' @noRd
cache_index_handle <- function(dir) {
  handle <- new.env(parent = emptyenv())
  handle$dir <- dir
  handle$idx <- NULL
  handle$upserts <- cache_index_empty()
  handle$dropped <- character(0)
  handle$dirty <- FALSE
  handle
}

#' The current index of a cache, from its handle when it has one
#' @keywords internal
#' @noRd
cache_index_get <- function(cache) {
  handle <- cache$index
  if (is.null(handle)) {
    return(cache_index_read(cache$dir))
  }
  if (is.null(handle$idx)) handle$idx <- cache_index_read(handle$dir)
  handle$idx
}

#' Add an index entry, replacing any entry with the same key
#' @keywords internal
#' @noRd
cache_index_upsert <- function(cache, entry) {
  handle <- cache$index
  if (is.null(handle)) {
    idx <- cache_index_read(cache$dir)
    idx <- dplyr::bind_rows(idx[!cache_key_match(idx, entry), , drop = FALSE], entry)
    cache_index_write(cache$dir, idx)
    return(invisible(idx))
  }
  idx <- cache_index_get(cache)
  handle$idx <- dplyr::bind_rows(idx[!cache_key_match(idx, entry), , drop = FALSE], entry)
  up <- handle$upserts
  handle$upserts <- dplyr::bind_rows(up[!cache_key_match(up, entry), , drop = FALSE], entry)
  handle$dirty <- TRUE
  invisible(handle$idx)
}

#' Remove the index entries for a blob
#' @keywords internal
#' @noRd
cache_index_drop <- function(cache, checksum) {
  handle <- cache$index
  if (is.null(handle)) {
    idx <- cache_index_read(cache$dir)
    idx <- idx[!(idx$checksum %in% checksum), , drop = FALSE]
    cache_index_write(cache$dir, idx)
    return(invisible(idx))
  }
  idx <- cache_index_get(cache)
  handle$idx <- idx[!(idx$checksum %in% checksum), , drop = FALSE]
  # A later upsert of the same blob is applied after the drop at flush time,
  # so an earlier one must not survive it.
  up <- handle$upserts
  handle$upserts <- up[!(up$checksum %in% checksum), , drop = FALSE]
  handle$dropped <- union(handle$dropped, checksum)
  handle$dirty <- TRUE
  invisible(handle$idx)
}

#' Write a query's index changes to `index.json` (see `cache_index_handle()`)
#' @keywords internal
#' @noRd
cache_index_flush <- function(cache) {
  handle <- cache$index
  if (is.null(handle) || !isTRUE(handle$dirty)) {
    return(invisible(FALSE))
  }
  idx <- cache_index_read(handle$dir)
  idx <- idx[!(idx$checksum %in% handle$dropped), , drop = FALSE]
  up <- handle$upserts
  if (nrow(up) > 0L) {
    replaced <- cache_key_string(
      idx$server, idx$doi, idx$directory_label, idx$label, idx$version
    ) %in% cache_key_string(up$server, up$doi, up$directory_label, up$label, up$version)
    idx <- dplyr::bind_rows(idx[!replaced, , drop = FALSE], up)
  }
  cache_index_write(handle$dir, idx)
  handle$idx <- idx
  handle$upserts <- cache_index_empty()
  handle$dropped <- character(0)
  handle$dirty <- FALSE
  invisible(TRUE)
}

# --- persisted discovery snapshots -----------------------------------------

#' @keywords internal
#' @noRd
cache_discovery_path <- function(dir) file.path(dir, "discovery.json")

#' Persist a concrete discovery result for intentional offline use
#' @keywords internal
#' @noRd
cache_discovery_write <- function(dir, discovery, collection) {
  if (is.null(dir)) return(invisible(FALSE))
  cache_ensure_dirs(dir)
  path <- cache_discovery_path(dir)
  ok <- cache_write_json(path, list(
    saved_at = jsonlite::unbox(format_cache_time(Sys.time())),
    server = jsonlite::unbox(discovery$server),
    collection = jsonlite::unbox(collection),
    discovery = discovery_to_json(discovery)
  ))
  if (!ok) {
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
  snapshot <- cache_read_json(path, cache_discovery_schema, warn = FALSE)
  if (is.null(snapshot)) {
    cli::cli_abort(c(
      "The discovery snapshot at {.path {path}} is unreadable.",
      i = "Run the query online once with caching enabled to replace it."
    ), call = call)
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

# --- automatic management (CRAN-11) ----------------------------------------

#' Say once per session where downloads are cached
#'
#' @param dir Cache directory.
#' @param quiet Suppress the notice (it is then shown on a later write).
#' @return Invisibly, whether the notice was shown.
#' @keywords internal
#' @noRd
cache_first_use_notice <- function(dir, quiet = FALSE) {
  if (isTRUE(quiet) || isTRUE(.gtropic_session$cache_notice_shown)) {
    return(invisible(FALSE))
  }
  .gtropic_session$cache_notice_shown <- TRUE
  cli::cli_inform(c(
    i = "Caching downloads in {.path {dir}} (see {.code ?gtropic_cache_enable}
         to change or disable)."
  ))
  invisible(TRUE)
}

#' Remove index entries and the blobs no remaining entry uses
#'
#' A blob may be referenced by more than one index entry (the same bytes at two
#' dataset versions), so only blobs that no surviving entry points at are
#' deleted.
#'
#' @param dir Cache directory.
#' @param idx The cache index.
#' @param drop Logical vector over `idx`: entries to remove.
#' @return The removed entries.
#' @keywords internal
#' @noRd
cache_remove_entries <- function(dir, idx, drop) {
  removed <- idx[drop, , drop = FALSE]
  kept <- idx[!drop, , drop = FALSE]
  orphans <- setdiff(unique(removed$checksum), unique(kept$checksum))
  unlink(cache_blob_path(dir, orphans), recursive = FALSE, force = FALSE)
  cache_index_write(dir, kept)
  removed
}

#' One string per cache key, for matching index entries
#' @keywords internal
#' @noRd
cache_key_string <- function(server, doi, directory_label, label,
                             version = NULL) {
  parts <- list(server, doi, directory_label, label)
  if (!is.null(version)) parts <- c(parts, list(version))
  do.call(paste, c(parts, sep = "\r"))
}

#' Is dataset version `a` older than version `b`?
#'
#' Dataverse versions are `major.minor`. Values that do not parse are never
#' treated as older.
#' @keywords internal
#' @noRd
version_older <- function(a, b) {
  vapply(seq_along(a), function(i) {
    va <- numeric_version(a[i], strict = FALSE)
    vb <- numeric_version(b[i], strict = FALSE)
    isTRUE(!is.na(va) && !is.na(vb) && va < vb)
  }, logical(1))
}

#' Manage the cache automatically after a retrieval
#'
#' CRAN asks that cached files be kept small and actively managed, including
#' removing outdated material. After a retrieval that wrote to the cache:
#'
#' 1. entries downloaded more than `gtropic.cache_max_age_days` ago are
#'    removed;
#' 2. entries for a file that `latest` holds at a newer dataset version are
#'    removed (they were superseded by a `":latest"` retrieval);
#' 3. the least recently downloaded entries are removed until the indexed size
#'    is at most `gtropic.cache_max_gb`.
#'
#' Entries in `keep` (the files the retrieval used) are never removed. `Inf`
#' disables rules 1 and 3.
#'
#' @param dir Cache directory.
#' @param keep Key strings (`cache_key_string()` with version) of protected
#'   entries.
#' @param latest A data frame of the files retrieved at `":latest"`, with
#'   `server`, `doi`, `directory_label`, `label` and `version`.
#' @param quiet Suppress the summary message.
#' @param now Current time.
#' @param idx The cache index, if already read.
#'
#' @return The removed entries, invisibly.
#' @keywords internal
#' @noRd
cache_manage <- function(dir, keep = character(0), latest = NULL,
                         quiet = FALSE, now = Sys.time(),
                         idx = cache_index_read(dir)) {
  if (nrow(idx) == 0L) {
    return(invisible(idx))
  }
  protected <- cache_key_string(
    idx$server, idx$doi, idx$directory_label, idx$label, idx$version
  ) %in% keep
  drop <- rep(FALSE, nrow(idx))

  max_age_days <- opt("cache_max_age_days")
  if (is.finite(max_age_days)) {
    drop <- drop | (!is.na(idx$fetched_at) &
      idx$fetched_at < now - max_age_days * 86400)
  }

  if (!is.null(latest) && nrow(latest) > 0L) {
    newest <- latest$version[match(
      cache_key_string(idx$server, idx$doi, idx$directory_label, idx$label),
      cache_key_string(
        latest$server, latest$doi, latest$directory_label, latest$label
      )
    )]
    drop <- drop | (!is.na(newest) & version_older(idx$version, newest))
  }
  drop <- drop & !protected

  max_gb <- opt("cache_max_gb")
  if (is.finite(max_gb)) {
    # Least recently downloaded first out; protected entries always stay and
    # count towards the budget first.
    survivors <- which(!drop)
    ord <- survivors[order(
      !protected[survivors], -as.numeric(idx$fetched_at[survivors])
    )]
    size <- idx$size_bytes[ord]
    running <- cumsum(ifelse(is.na(size), 0, size))
    drop[ord[running > max_gb * 1024^3 & !protected[ord]]] <- TRUE
  }

  if (!any(drop)) {
    return(invisible(idx[0, , drop = FALSE]))
  }
  removed <- cache_remove_entries(dir, idx, drop)
  inform_unless_quiet(
    c(i = "Removed {nrow(removed)} old or outdated cached file{?s}
           ({format_bytes(sum(removed$size_bytes, na.rm = TRUE))}) to keep the
           cache within its limits (see {.code ?gtropic_options})."),
    quiet = quiet
  )
  invisible(removed)
}

#' Finish a fetch context's use of the cache
#'
#' Run on exit by every retrieval. Writes the query's changes to the cache
#' index (`cache_index_flush()`), then runs `cache_manage()` when the
#' retrieval wrote to the cache, protecting every file it used.
#'
#' @param ctx A fetch context.
#' @return The removed entries, invisibly, or `NULL` when nothing ran.
#' @keywords internal
#' @noRd
cache_manage_ctx <- function(ctx) {
  cache_index_flush(ctx$cache)
  if (!isTRUE(ctx$cache$enabled) || !isTRUE(ctx$cache_state$written)) {
    return(invisible(NULL))
  }
  mf <- ctx_manifest(ctx)
  files <- mf$files
  keep <- cache_key_string(
    mf$server, files$dataset_doi, files$directory_label, files$label,
    files$dataset_version
  )
  at_latest <- files[files$dataset_doi %in% ctx$latest_dois, , drop = FALSE]
  latest <- tibble::tibble(
    server = rep(mf$server, nrow(at_latest)),
    doi = at_latest$dataset_doi,
    directory_label = at_latest$directory_label,
    label = at_latest$label,
    version = at_latest$dataset_version
  )
  cache_manage(ctx$cache$dir, keep = keep, latest = latest,
    quiet = ctx$notice_quiet, idx = cache_index_get(ctx$cache)
  )
}

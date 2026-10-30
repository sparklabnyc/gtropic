# Session-scoped scratch space. Used for the discovered DOI/year index so that
# collection discovery (a multi-request operation) happens at most once per
# session. Deliberately an environment rather than options(), because it holds
# derived state that users should never set by hand.
.gtropic_session <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  persisted_cache_enabled <- cache_config_read()
  defaults <- list(
    gtropic.server = "demo.dataverse.org",
    gtropic.doi_historical = "doi:10.70122/FK2/SWEYST",
    gtropic.doi_current = "doi:10.70122/FK2/DDSYYD",
    gtropic.collection = "gtropic",
    # The config file is read without creating it. A user-supplied option takes
    # precedence over this persisted setting.
    gtropic.cache_enabled = persisted_cache_enabled %||% TRUE,
    # Resolving this path does not create it; directories are made only when a
    # retrieval writes its first cached file or caching is explicitly enabled.
    gtropic.cache_dir = tools::R_user_dir("gtropic", "cache"),
    gtropic.confirm_threshold_gb = 1,
    gtropic.timeout = 600,
    gtropic.max_tries = 3
  )

  # Only fill in options the user has not already set (e.g. in .Rprofile).
  unset <- !(names(defaults) %in% names(options()))
  if (any(unset)) options(defaults[unset])

  invisible()
}

.onUnload <- function(libpath) {
  rm(list = ls(envir = .gtropic_session), envir = .gtropic_session)
  invisible()
}

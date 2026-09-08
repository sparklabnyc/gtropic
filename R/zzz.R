# Session-scoped scratch space. Used for the discovered DOI/year index so that
# collection discovery (a multi-request operation) happens at most once per
# session. Deliberately an environment rather than options(), because it holds
# derived state that users should never set by hand.
.gtropic_session <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  defaults <- list(
    gtropic.server = "demo.dataverse.org",
    gtropic.doi_historical = "doi:10.70122/FK2/SWEYST",
    gtropic.doi_current = "doi:10.70122/FK2/DDSYYD",
    gtropic.collection = "gtropic",
    gtropic.cache_enabled = FALSE,
    # Resolved lazily rather than at load time so that a user who has never
    # enabled the cache never triggers directory creation.
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

# Session-scoped scratch space. Used for the discovered DOI/year index so that
# collection discovery (a multi-request operation) happens at most once per
# session. Deliberately an environment rather than options(), because it holds
# derived state that users should never set by hand.
.gtropic_session <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Defaults come from the options registry (gtropic_default_options()).
  # Resolving the cache path does not create it; directories are made only
  # when a retrieval writes its first cached file or caching is explicitly
  # enabled.
  defaults <- lapply(gtropic_default_options(), `[[`, "default")
  names(defaults) <- paste0("gtropic.", names(defaults))
  # The config file is read without creating it. A user-supplied option takes
  # precedence over this persisted setting.
  persisted_cache_enabled <- cache_config_read()
  if (!is.null(persisted_cache_enabled)) {
    defaults$gtropic.cache_enabled <- persisted_cache_enabled
  }

  # Only fill in options the user has not already set (e.g. in .Rprofile).
  unset <- !(names(defaults) %in% names(options()))
  if (any(unset)) options(defaults[unset])

  invisible()
}

.onUnload <- function(libpath) {
  rm(list = ls(envir = .gtropic_session), envir = .gtropic_session)
  invisible()
}

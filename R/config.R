#' Options used by gtropic
#'
#' @description
#' `gtropic` is configured entirely through `options()`. Setting these is how
#' you point the package at a different Dataverse installation -- for example
#' moving from the demonstration server to a production repository -- without
#' needing a package update.
#'
#' @details
#' | Option | Default | Meaning |
#' | --- | --- | --- |
#' | `gtropic.server` | `"dataverse.harvard.edu"` | Dataverse host (HTTPS only). |
#' | `gtropic.doi_historical` | `"doi:10.7910/DVN/EAVWC3"` | DOI of the historical dataset. |
#' | `gtropic.doi_current` | `"doi:10.7910/DVN/FZBAKO"` | DOI of the current-year dataset. Used only as a fallback if automatic discovery fails. |
#' | `gtropic.collection` | `"gtropic"` | Dataverse collection (alias) searched during discovery. |
#' | `gtropic.cache_enabled` | `TRUE` | Whether downloaded files are kept on disk. |
#' | `gtropic.cache_dir` | `tools::R_user_dir("gtropic", "cache")` | Where cached files live. |
#' | `gtropic.confirm_threshold_gb` | `1` | Download size, in gigabytes, above which you are asked to confirm. |
#' | `gtropic.timeout` | `600` | Per-request timeout in seconds. |
#' | `gtropic.max_tries` | `3` | Maximum attempts per request before giving up. |
#'
#' A "cache" here means a folder of previously downloaded files on your own
#' computer. It is enabled by default and uses a standard per-user location
#' chosen by R. Use [gtropic_cache_disable()] or `cache = FALSE` on a retrieval
#' to bypass it. See [gtropic_cache_enable()] to persist an enabled or disabled
#' choice across R sessions.
#'
#' @return These are options, not a function; nothing is returned. This topic
#'   exists for documentation only.
#'
#' @examples
#' # Point the package at a different installation:
#' \dontrun{
#' options(
#'   gtropic.server = "dataverse.harvard.edu",
#'   gtropic.doi_historical = "doi:10.7910/DVN/EXAMPLE"
#' )
#' }
#'
#' # Inspect the current settings:
#' getOption("gtropic.server")
#'
#' @name gtropic_options
NULL

#' Read a gtropic option
#'
#' @param name Option name, without the `gtropic.` prefix.
#' @param default Value to fall back to if the option is unset.
#'
#' @return The option value.
#' @keywords internal
#' @noRd
opt <- function(name, default = NULL) {
  getOption(paste0("gtropic.", name), default = default)
}

#' Default Dataverse host
#'
#' The single definition used by `.onLoad()` and `opt_server()`.
#' @keywords internal
#' @noRd
DEFAULT_SERVER <- "dataverse.harvard.edu"

#' @keywords internal
#' @noRd
opt_server <- function() {
  validate_server(opt("server", DEFAULT_SERVER), arg = "gtropic.server")
}

#' Validate and normalise a Dataverse host name
#'
#' Any scheme and trailing slash are stripped (requests are always HTTPS);
#' what remains must be a bare host name with an optional port. Paths, queries,
#' user information, spaces and control characters are rejected, because the
#' host is pasted into request URLs.
#'
#' @param x Value to check.
#' @param arg Where the value came from, for the error message.
#' @param call Calling environment for error reporting.
#'
#' @return The normalised host name.
#' @keywords internal
#' @noRd
validate_server <- function(x, arg = "server", call = rlang::caller_env()) {
  ok <- is.character(x) && length(x) == 1L && !is.na(x)
  if (ok) {
    x <- sub("/+$", "", sub("^https?://", "", x, ignore.case = TRUE))
    ok <- nchar(x) <= 253L && grepl(
      "^[A-Za-z0-9]([A-Za-z0-9.-]*[A-Za-z0-9])?(:[0-9]{1,5})?$", x
    )
  }
  if (!ok) {
    cli::cli_abort(
      c("{.code {arg}} must be a Dataverse host name, such as
         {.val {DEFAULT_SERVER}}.",
        x = if (is.character(x) && length(x) == 1L) {
          "Got {.val {x}}."
        } else {
          "Got an object of class {.cls {class(x)}} and length {length(x)}."
        }
      ),
      call = call
    )
  }
  x
}

#' @keywords internal
#' @noRd
opt_timeout <- function() as.numeric(opt("timeout", 600))

#' @keywords internal
#' @noRd
opt_max_tries <- function() as.integer(opt("max_tries", 3L))

#' @keywords internal
#' @noRd
opt_collection <- function() opt("collection", "gtropic")

#' @keywords internal
#' @noRd
opt_confirm_threshold_gb <- function() as.numeric(opt("confirm_threshold_gb", 1))

#' Resolve the version argument into per-dataset versions
#'
#' `version` may be a single string applied to both datasets, or a named vector
#' such as `c(historical = "2.0", current = "1.3")`. Anything else is a user
#' error rather than something to silently normalize.
#'
#' @param version User-supplied `version` argument.
#' @param call Calling environment for error reporting.
#'
#' @return A list with `historical` and `current` version strings.
#' @keywords internal
#' @noRd
resolve_versions <- function(version = ":latest", call = rlang::caller_env()) {
  if (!is.character(version) || length(version) == 0L) {
    cli::cli_abort(
      c("{.arg version} must be a character vector.",
        i = 'Use {.val :latest}, a single version string, or
             {.code c(historical = "2.0", current = "1.3")}.'
      ),
      call = call
    )
  }

  if (length(version) == 1L && is.null(names(version))) {
    return(list(historical = version, current = version))
  }

  nms <- names(version)
  if (is.null(nms) || !all(nms %in% c("historical", "current"))) {
    cli::cli_abort(
      c("{.arg version} of length > 1 must be named.",
        x = "Got name{?s}: {.val {nms %||% 'none'}}.",
        i = 'Expected {.code c(historical = "...", current = "...")}.'
      ),
      call = call
    )
  }

  # `[[` on an absent name errors for atomic vectors, so index positionally.
  pick <- function(nm) {
    i <- match(nm, nms)
    if (is.na(i)) ":latest" else unname(version[[i]])
  }

  list(historical = pick("historical"), current = pick("current"))
}

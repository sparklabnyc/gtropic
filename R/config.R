#' Options used by gtropic
#'
#' @description
#' `gtropic` is configured entirely through `options()`. Setting these is how
#' you point the package at a different Dataverse installation, or change how
#' it caches and confirms downloads, without needing a package update.
#'
#' @details
#' `r options_doc_table()`
#'
#' Each option is checked when it is read, and an invalid value is an error
#' that names the option.
#'
#' A "cache" here means a folder of previously downloaded files on your own
#' computer. It is enabled by default and uses a standard per-user location
#' chosen by R. Use [gtropic_cache_disable()] or `cache = FALSE` on a retrieval
#' to bypass it. See [gtropic_cache_enable()] to persist an enabled or disabled
#' choice across R sessions; a persisted choice replaces the default of
#' `gtropic.cache_enabled` when the package is loaded.
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

#' Registry of gtropic options
#'
#' The single definition of every option: its default, the check applied when
#' it is read, and its row in `?gtropic_options`. `.onLoad()`, `opt()` and the
#' documentation table all come from here. A new option is added here, and
#' nowhere else, once the maintainers have accepted it.
#'
#' @return A named list of option specifications (see `option_spec()`), keyed
#'   by option name without the `gtropic.` prefix.
#' @keywords internal
#' @noRd
gtropic_default_options <- function() {
  list(
    server = option_spec(
      "dataverse.harvard.edu", "server",
      "Dataverse host name (requests always use HTTPS), optionally with a port."
    ),
    doi_historical = option_spec(
      "doi:10.7910/DVN/EAVWC3", "string",
      "DOI of the historical dataset."
    ),
    doi_current = option_spec(
      "doi:10.7910/DVN/FZBAKO", "string",
      "DOI of the current-year dataset. Used only as a fallback if automatic
       discovery fails."
    ),
    collection = option_spec(
      "gtropic", "string",
      "Dataverse collection (alias) searched during discovery."
    ),
    cache_enabled = option_spec(
      TRUE, "flag",
      "Whether downloaded files are kept on disk."
    ),
    cache_dir = option_spec(
      tools::R_user_dir("gtropic", "cache"), "string",
      "Where cached files live.",
      shown = 'tools::R_user_dir("gtropic", "cache")'
    ),
    cache_max_age_days = option_spec(
      180, "positive",
      "Cached files downloaded more than this many days ago are removed
       automatically after a retrieval that adds to the cache. `Inf` keeps
       them."
    ),
    cache_max_gb = option_spec(
      10, "positive",
      "Size cap, in gigabytes, for the cache: after a retrieval that adds to
       it, the least recently downloaded files are removed until it fits.
       `Inf` disables the cap."
    ),
    confirm_threshold_gb = option_spec(
      1, "positive",
      "Download size, in gigabytes, above which you are asked to confirm."
    ),
    timeout = option_spec(
      600, "positive",
      "Per-request timeout in seconds."
    ),
    max_tries = option_spec(
      3L, "count",
      "Maximum attempts per request before giving up."
    ),
    quiet = option_spec(
      FALSE, "flag",
      "Session-wide default for `quiet`: `TRUE` suppresses progress and
       informational messages from every call."
    )
  )
}

#' Describe one option
#'
#' @param default Default value.
#' @param type How a value is checked when read: `"server"` (a host name, see
#'   `validate_server()`), `"string"` (a character scalar), `"flag"` (`TRUE` or
#'   `FALSE`), `"positive"` (a number greater than zero, `Inf` allowed) or
#'   `"count"` (a whole number of at least one).
#' @param doc One sentence for the documentation table.
#' @param shown How the default is shown in the documentation, when it is not
#'   the value itself.
#' @keywords internal
#' @noRd
option_spec <- function(default, type, doc, shown = NULL) {
  list(
    default = default, type = type, doc = gsub("\\s+", " ", doc),
    shown = shown %||% if (is.character(default)) deparse(default) else format(default)
  )
}

#' Default value of an option
#'
#' @param name Option name, without the `gtropic.` prefix.
#' @keywords internal
#' @noRd
opt_default <- function(name) {
  gtropic_default_options()[[name]]$default
}

#' Read a gtropic option
#'
#' Unset options fall back to their registry default; set ones are checked
#' against the registry.
#'
#' @param name Option name, without the `gtropic.` prefix.
#' @param call Calling environment for error reporting.
#'
#' @return The option value.
#' @keywords internal
#' @noRd
opt <- function(name, call = rlang::caller_env()) {
  spec <- gtropic_default_options()[[name]]
  if (is.null(spec)) {
    cli::cli_abort("Unknown option {.code gtropic.{name}}.", .internal = TRUE)
  }
  value <- getOption(paste0("gtropic.", name))
  if (is.null(value)) {
    return(spec$default)
  }
  option_check(value, spec$type, paste0("gtropic.", name), call)
}

#' Check an option value against its type
#' @keywords internal
#' @noRd
option_check <- function(x, type, arg, call = rlang::caller_env()) {
  if (identical(type, "server")) {
    return(validate_server(x, arg = arg, call = call))
  }
  scalar <- length(x) == 1L && !is.na(x)
  ok <- switch(type,
    string = is.character(x) && scalar,
    flag = is.logical(x) && scalar,
    positive = is.numeric(x) && scalar && x > 0,
    count = is.numeric(x) && scalar && is.finite(x) && x >= 1 &&
      x == round(x)
  )
  if (!isTRUE(ok)) {
    expected <- switch(type,
      string = "a single string",
      flag = "TRUE or FALSE",
      positive = "a single number greater than zero",
      count = "a single whole number of at least 1"
    )
    cli::cli_abort(
      c(
        "{.code {arg}} must be {expected}.",
        x = if (length(x) == 1L) {
          "Got {.val {x}}."
        } else {
          "Got an object of class {.cls {class(x)}} and length {length(x)}."
        }
      ),
      call = call
    )
  }
  switch(type,
    count = as.integer(x),
    positive = as.numeric(x),
    x
  )
}

#' The options table in `?gtropic_options`
#'
#' Called from inline R code in the roxygen block, so the table is generated
#' from the registry.
#' @return A Markdown table, as a single string.
#' @keywords internal
#' @noRd
options_doc_table <- function() {
  specs <- gtropic_default_options()
  rows <- vapply(names(specs), function(nm) {
    s <- specs[[nm]]
    sprintf("| `gtropic.%s` | `%s` | %s |", nm, s$shown, s$doc)
  }, character(1))
  paste(c("| Option | Default | Meaning |", "| --- | --- | --- |", rows),
    collapse = "\n"
  )
}

#' @keywords internal
#' @noRd
opt_server <- function() opt("server")

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
    example <- opt_default("server")
    cli::cli_abort(
      c("{.code {arg}} must be a Dataverse host name, such as
         {.val {example}}.",
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
opt_timeout <- function() opt("timeout")

#' @keywords internal
#' @noRd
opt_max_tries <- function() opt("max_tries")

#' @keywords internal
#' @noRd
opt_collection <- function() opt("collection")

#' @keywords internal
#' @noRd
opt_confirm_threshold_gb <- function() opt("confirm_threshold_gb")

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

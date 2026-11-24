# Strings from a server or from a manifest are data. `cli` treats every
# template it is given as glue and evaluates `{...}` as R code, so such
# strings must never reach template position (Plan 05 SEC-01).

injected <- function(flag) sprintf("{file.create('%s')}", flag)

test_that("printing a manifest does not evaluate code in its strings", {
  m <- gtropic_manifest(gtropic_example())
  flag <- withr::local_tempfile()
  m$datasets$dataset_doi[1] <- injected(flag)
  m$datasets$dataset_version[1] <- injected(flag)

  expect_no_error(cli::cli_fmt(print(m)))
  expect_false(file.exists(flag))
})

test_that("printing or summarising a result does not evaluate its strings", {
  x <- gtropic_example()
  flag <- withr::local_tempfile()
  mf <- attr(x, "manifest")
  mf$datasets$dataset_doi[1] <- injected(flag)
  mf$datasets$dataset_version[1] <- injected(flag)
  mf$server <- injected(flag)
  attr(x, "manifest") <- mf

  expect_no_error(cli::cli_fmt(print(x)))
  expect_no_error(cli::cli_fmt(print(summary(x))))
  expect_false(file.exists(flag))
})

test_that("braces in a DOI or version are printed literally", {
  m <- gtropic_manifest(gtropic_example())
  m$datasets$dataset_doi[1] <- "doi:10.0000/{A}"
  m$datasets$dataset_version[1] <- "1.{0}"
  x <- gtropic_example()
  mf <- attr(x, "manifest")
  mf$datasets <- m$datasets
  attr(x, "manifest") <- mf

  out <- paste(cli::cli_fmt(print(m)), collapse = "\n")
  expect_match(out, "doi:10.0000/{A} (version 1.{0})", fixed = TRUE)
  out <- paste(cli::cli_fmt(print(x)), collapse = "\n")
  expect_match(out, "doi:10.0000/{A} (version 1.{0})", fixed = TRUE)
  out <- paste(cli::cli_fmt(print(summary(x))), collapse = "\n")
  expect_match(out, "doi:10.0000/{A} v1.{0}", fixed = TRUE)
})

test_that("unmatched filter values and suggestions are not evaluated", {
  flag <- withr::local_tempfile()
  # A published name that is a close match to the user's value appears in the
  # error's "did you mean" suggestion.
  candidate <- injected(flag)
  err <- expect_error(
    match_values(paste0(candidate, "x"), candidate, arg = "ADM2_NAME"),
    class = "rlang_error"
  )
  expect_false(file.exists(flag))
  expect_match(conditionMessage(err), candidate, fixed = TRUE)

  err <- expect_error(
    match_values("{A}", "Example Coast", arg = "ADM2_NAME"),
    class = "rlang_error"
  )
  expect_match(conditionMessage(err), "\"{A}\" - no close match found", fixed = TRUE)
})

# --- regression guard --------------------------------------------------------
#
# Scans the package's own functions (so it also runs under R CMD check, where
# the R/ sources are not available) for calls to interpolating `cli` functions
# whose template is anything other than a string literal. `c()`, `if` and `{`
# are allowed as long as everything they return is a literal, and so is text
# passed through the package's `cli_escape()`.

cli_template_sinks <- c(
  "cli_abort", "cli_warn", "cli_inform", "cli_text", "cli_bullets",
  "cli_alert", "cli_alert_info", "cli_alert_success", "cli_alert_warning",
  "cli_alert_danger", "cli_h1", "cli_h2", "cli_h3", "cli_li", "cli_ul",
  "cli_ol", "cli_dl", "cli_blockquote", "cli_rule", "format_inline",
  "format_message", "format_warning", "format_error",
  # The package's own wrappers around cli::cli_inform(), cli::cli_abort() and
  # cli::cli_warn().
  "inform_unless_quiet", "gtropic_abort", "gtropic_warn"
)

is_literal_template <- function(e) {
  if (is.character(e) || is.null(e)) {
    return(TRUE)
  }
  if (!is.call(e)) {
    return(FALSE)
  }
  head <- e[[1L]]
  if (identical(head, as.name("cli_escape"))) {
    return(TRUE)
  }
  if (identical(head, as.name("c")) || identical(head, as.name("("))) {
    parts <- as.list(e)[-1L]
    return(all(vapply(parts, is_literal_template, logical(1))))
  }
  if (identical(head, as.name("if"))) {
    return(is_literal_template(e[[3L]]) &&
      (length(e) < 4L || is_literal_template(e[[4L]])))
  }
  if (identical(head, as.name("{"))) {
    return(length(e) == 1L || is_literal_template(e[[length(e)]]))
  }
  FALSE
}

call_name <- function(e) {
  fn <- e[[1L]]
  if (is.symbol(fn)) {
    return(as.character(fn))
  }
  if (is.call(fn) && identical(fn[[1L]], as.name("::"))) {
    return(as.character(fn[[3L]]))
  }
  ""
}

template_arg <- function(e) {
  args <- as.list(e)[-1L]
  if (length(args) == 0L) {
    return(NULL)
  }
  nms <- names(args) %||% rep("", length(args))
  i <- which(nms %in% c("", "message", "text"))[1L]
  if (is.na(i)) NULL else args[[i]]
}

find_unsafe_cli_calls <- function(e, where) {
  found <- character(0)
  if (is.function(e)) {
    return(c(
      unlist(lapply(as.list(formals(e)), find_unsafe_cli_calls, where = where)),
      find_unsafe_cli_calls(body(e), where)
    ))
  }
  if (!is.call(e)) {
    return(found)
  }
  if (call_name(e) %in% cli_template_sinks) {
    tmpl <- template_arg(e)
    # `...` is a pass-through (inform_unless_quiet() itself); its callers are
    # checked at their own call sites.
    if (!is.null(tmpl) && !identical(tmpl, quote(...)) &&
      !is_literal_template(tmpl)) {
      found <- c(found, paste0(where, ": ", deparse(e, nlines = 1L)))
    }
  }
  for (part in as.list(e)) {
    if (!missing(part)) {
      found <- c(found, find_unsafe_cli_calls(part, where))
    }
  }
  found
}

test_that("the guard flags templates built from data", {
  bad <- list(
    function(x) cli::cli_warn(x),
    function(x) cli::cli_bullets(c("*" = "ok", stats::setNames(paste0("a", x), "*"))),
    function(x) cli::cli_abort(c("ok", sprintf("%s", x))),
    function(x) inform_unless_quiet(glue::glue("{x}"))
  )
  for (f in bad) {
    expect_length(find_unsafe_cli_calls(f, "f"), 1L)
  }
  good <- function(x) {
    cli::cli_abort(c("Value {.val {x}}.", i = if (x) "yes" else "no"))
    cli::cli_warn(c("Unmatched:", cli_escape(x)))
    cli::cli_bullets_raw(x)
  }
  expect_length(find_unsafe_cli_calls(good, "good"), 0L)
})

test_that("no cli template in the package is built from data", {
  ns <- asNamespace("gtropic")
  unsafe <- character(0)
  for (nm in ls(ns, all.names = TRUE)) {
    obj <- get(nm, envir = ns)
    if (is.function(obj)) {
      unsafe <- c(unsafe, find_unsafe_cli_calls(obj, nm))
    }
  }
  expect_identical(unsafe, character(0))
})

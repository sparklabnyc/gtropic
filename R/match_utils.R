#' Normalise a string for case-insensitive comparison
#'
#' Human-readable fields are matched case-insensitively and whitespace-trimmed,
#' but the *canonical* spelling from the dataset is what we carry forward, so
#' this is only ever used to build a lookup key.
#'
#' @param x Character vector.
#' @return Character vector, lower-cased with collapsed whitespace.
#' @keywords internal
#' @noRd
norm_key <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  tolower(x)
}

#' Suggest near-matches for an unmatched value
#'
#' Uses [utils::adist()] so no new dependency is introduced. The distance
#' threshold scales with string length: a typo in a 4-character country code is
#' worth flagging at distance 1, while a long place name tolerates more. Without
#' the scaling, short strings attract nonsense suggestions.
#'
#' @param x A single unmatched value.
#' @param candidates Character vector of the valid vocabulary.
#' @param n Maximum suggestions to return.
#'
#' @return Character vector of up to `n` suggested canonical values.
#' @keywords internal
#' @noRd
suggest_matches <- function(x, candidates, n = 3L) {
  if (length(candidates) == 0L) {
    return(character(0))
  }

  d <- utils::adist(norm_key(x), norm_key(candidates), ignore.case = TRUE)[1, ]

  # Allow roughly one edit per four characters, with short values still getting
  # a little room for transpositions. The cap stops very long names from
  # matching essentially anything.
  threshold <- max(1L, min(4L, ceiling(nchar(x) / 4)))
  keep <- which(d <= threshold)
  if (length(keep) == 0L) {
    return(character(0))
  }

  keep <- keep[order(d[keep])]
  unique(as.character(candidates)[utils::head(keep, n)])
}

#' Match user-supplied filter values against a vocabulary
#'
#' Any unmatched value is a hard error, including a partial miss. Silently
#' answering a narrower question than the one asked is the worst failure mode in
#' a research pipeline: the user gets a plausible-looking result that is wrong.
#'
#' @param values Character vector supplied by the user.
#' @param candidates Character vector of valid values from the dataset.
#' @param arg Name of the argument being validated, for the error message.
#' @param exact `TRUE` for machine keys (case-sensitive, exact), `FALSE` for
#'   human-readable fields (case-insensitive, whitespace-trimmed).
#' @param helper Name of the discovery helper to point the user at.
#' @param what Noun describing what failed to match.
#' @param call Calling environment for error reporting.
#'
#' @return The canonical spellings of the matched values.
#' @keywords internal
#' @noRd
match_values <- function(values,
                         candidates,
                         arg,
                         exact = FALSE,
                         helper = "gtropic_adm2()",
                         what = "ADM2 unit",
                         call = rlang::caller_env()) {
  candidates <- unique(as.character(candidates[!is.na(candidates)]))

  if (exact) {
    hit <- values %in% candidates
    canonical <- values
  } else {
    lookup <- stats::setNames(candidates, norm_key(candidates))
    # Duplicated keys (two spellings differing only by case) collapse to the
    # first occurrence; the dataset should not contain these, but if it does we
    # prefer determinism to an error here.
    lookup <- lookup[!duplicated(names(lookup))]
    idx <- match(norm_key(values), names(lookup))
    hit <- !is.na(idx)
    canonical <- ifelse(hit, unname(lookup[idx]), values)
  }

  if (all(hit)) {
    return(unique(canonical))
  }

  missed <- values[!hit]
  bullets <- vapply(missed, function(m) {
    sugg <- suggest_matches(m, candidates)
    if (length(sugg) == 0L) {
      glue::glue('"{m}" - no close match found')
    } else {
      glue::glue('"{m}" - did you mean {paste0(\'"\', sugg, \'"\', collapse = ", ")}?')
    }
  }, character(1))
  names(bullets) <- rep("x", length(bullets))

  cli::cli_abort(
    c(
      "{length(missed)} value{?s} in {.arg {arg}} did not match any {what}.",
      bullets,
      i = "See {.code {helper}} for the full list of valid values."
    ),
    call = call
  )
}

#' Abort because two filters intersect to nothing
#'
#' Returning an empty result would look like a legitimate finding. Naming the
#' arguments involved and restating the AND rule is the only useful response.
#'
#' @param args Character vector of argument names that were supplied.
#' @param call Calling environment.
#'
#' @return Never returns.
#' @keywords internal
#' @noRd
abort_empty_intersection <- function(args, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "No ADM2 units satisfy all of the geographic filters supplied.",
      x = "Filters in play: {.arg {args}}.",
      i = "Values are combined with OR {.emph within} an argument and AND
           {.emph across} arguments, so these conditions must hold together.",
      i = "Use {.code gtropic_adm2()} to explore valid combinations."
    ),
    call = call
  )
}

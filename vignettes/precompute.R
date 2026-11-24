# Precompute the vignettes and the web-only articles.
#
# The vignettes query a live Dataverse repository, which R CMD check must never
# do: checks run on machines with no reliable network, and CRAN policy forbids
# it outright. So we use the .Rmd.orig pattern.
#
#   vignettes/*.Rmd.orig            <- the source you edit; chunks really run
#   vignettes/*.Rmd                 <- generated here, with outputs baked in
#   vignettes/articles/*.Rmd.orig   <- the same, for pkgdown-only articles
#
# Running this script knits each .orig file with the network available and
# writes a plain .Rmd containing the results. R CMD build and pkgdown then
# treat those as ordinary documents with nothing left to evaluate.
#
# Run from the package root, with a working network connection, whenever the
# sources change or the published data is revised:
#
#   source("vignettes/precompute.R")
#
# Then inspect the generated .Rmd files and figures and commit them with the
# sources.
#
# Downloads go to a throwaway cache in tempdir(), never to your own gtropic
# cache. To reuse downloads between runs, point GTROPIC_PRECOMPUTE_CACHE at a
# directory you are happy to give to this script.

library(gtropic)

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("knitr is required to precompute the vignettes.")
}

# Figures are drawn with ragg and recompressed losslessly by optipng (see the
# setup chunk of each .Rmd.orig). Without optipng, knitr only warns, and
# unoptimised figures would be committed, so refuse to start instead.
if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("ragg is required to precompute the vignettes: install.packages(\"ragg\").")
}
if (!nzchar(Sys.which("optipng"))) {
  stop(
    "optipng is required to precompute the vignettes: it recompresses the ",
    "figures losslessly. Install it (for example `brew install optipng` or ",
    "`apt install optipng`) and make sure it is on the PATH."
  )
}

# Ensure knitted output is clean for version control while retaining UTF-8 text
# and the terminating newline expected of text files.
normalize_rmd_whitespace <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("[ \t]+$", "", lines)

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(if (length(lines)) lines else "", con, useBytes = TRUE)
}

# Sources to knit, relative to vignettes/. Each is knitted from its own
# directory, so figures land next to it (figs/ or articles/figs/).
documents <- c(
  "getting-started",
  "filtering",
  "linkage",
  "plotting",
  "versions-and-caching",
  file.path("articles", sub("\\.Rmd\\.orig$", "", list.files(
    file.path("vignettes", "articles"), pattern = "\\.Rmd\\.orig$"
  )))
)

knit_document <- function(doc) {
  input <- paste0(basename(doc), ".Rmd.orig")
  output <- paste0(basename(doc), ".Rmd")
  old <- setwd(dirname(doc))
  on.exit(setwd(old), add = TRUE)

  if (!file.exists(input)) {
    stop("Cannot find ", file.path("vignettes", doc), ".Rmd.orig.")
  }
  message("Knitting ", doc, ".Rmd.orig -> ", doc, ".Rmd")
  # knit() keeps going past a failing chunk by default and writes the error
  # into the output. Stop instead; chunks that are meant to fail say
  # error = TRUE themselves.
  knitr::opts_chunk$set(error = FALSE)
  started <- Sys.time()
  knitr::knit(input = input, output = output, quiet = TRUE)
  normalize_rmd_whitespace(output)
  as.numeric(difftime(Sys.time(), started, units = "secs"))
}

precompute_vignettes <- function() {
  cache_dir <- Sys.getenv(
    "GTROPIC_PRECOMPUTE_CACHE",
    file.path(tempdir(), "gtropic-precompute-cache")
  )
  old_options <- options(
    gtropic.cache_dir = cache_dir,
    gtropic.cache_enabled = TRUE,
    arrow.use_threads = FALSE,
    width = 80,
    cli.width = 80,
    pillar.width = 80
  )
  on.exit(options(old_options), add = TRUE)

  # The same two-thread limits as the tests (CRAN policy).
  old_env <- Sys.getenv("OMP_THREAD_LIMIT", unset = NA)
  Sys.setenv(OMP_THREAD_LIMIT = "2")
  on.exit(
    if (is.na(old_env)) Sys.unsetenv("OMP_THREAD_LIMIT") else Sys.setenv(OMP_THREAD_LIMIT = old_env),
    add = TRUE
  )
  old_cpu <- arrow::cpu_count()
  old_io <- arrow::io_thread_count()
  arrow::set_cpu_count(1L)
  arrow::set_io_thread_count(2L)
  on.exit(
    {
      arrow::set_cpu_count(old_cpu)
      arrow::set_io_thread_count(old_io)
    },
    add = TRUE
  )

  # The first file cached in a session prints a notice naming the cache
  # directory. Fetch the small codebook first, so the notice appears here and
  # no temporary path is baked into a vignette.
  invisible(gtropic_codebook())

  old <- setwd("vignettes")
  on.exit(setwd(old), add = TRUE)
  seconds <- vapply(documents, knit_document, numeric(1))
  message("\nKnitting time (s):")
  message(paste0("  ", format(names(seconds)), "  ", round(seconds), collapse = "\n"))
}

precompute_vignettes()

message(
  "\nDone. Check the generated .Rmd files for:\n",
  "  * error/warning chunks that produced the intended message\n",
  "  * output that is too long and should be truncated at the source\n",
  "  * any absolute paths or machine-specific detail that leaked into output\n"
)

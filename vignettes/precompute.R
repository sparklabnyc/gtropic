# Precompute the vignettes.
#
# The vignettes query a live Dataverse repository, which R CMD check must never
# do: checks run on machines with no reliable network, and CRAN policy forbids
# it outright. So we use the .Rmd.orig pattern.
#
#   vignettes/*.Rmd.orig   <- the source you edit; chunks really run
#   vignettes/*.Rmd        <- generated here, with outputs baked in as text
#
# Running this script knits each .orig file with the network available and
# writes a plain .Rmd containing the results. R CMD build then treats those as
# ordinary vignettes with nothing left to evaluate.
#
# Run from the package root, with a working network connection, whenever the
# vignette sources change or the published data is revised:
#
#   source("vignettes/precompute.R")
#
# Then inspect the generated .Rmd files and commit both .orig and .Rmd.

# Load gtropic to manage the cache during precomputation
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

vignettes <- c(
  "getting-started",
  "filtering",
  "linkage",
  "plotting",
  "versions-and-caching"
)

# Knit from within vignettes/ so that any figures land in the right place with
# relative paths intact.
precompute_vignettes <- function() {
  old_cache_enabled <- options(gtropic.cache_enabled = TRUE)
  on.exit(options(old_cache_enabled), add = TRUE)
  gtropic_cache_clear()

  old <- setwd("vignettes")
  on.exit(setwd(old), add = TRUE)

  old_display_options <- options(c(
    "width",
    "cli.width",
    "pillar.width"
  ))
  options(width = 80, cli.width = 80, pillar.width = 80)
  on.exit(options(old_display_options), add = TRUE)

  for (v in vignettes) {
    input <- paste0(v, ".Rmd.orig")
    output <- paste0(v, ".Rmd")

    if (!file.exists(input)) {
      warning("Skipping ", input, ": not found.")
      next
    }

    message("Knitting ", input, " -> ", output)
    knitr::knit(input = input, output = output)
    normalize_rmd_whitespace(output)
  }
}

precompute_vignettes()

message(
  "\nDone. Check the generated .Rmd files for:\n",
  "  * error/warning chunks that produced the intended message\n",
  "  * output that is too long and should be truncated at the source\n",
  "  * any absolute paths or machine-specific detail that leaked into output\n"
)

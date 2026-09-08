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

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("knitr is required to precompute the vignettes.")
}

vignettes <- c(
  "getting-started",
  "filtering",
  "linkage",
  "versions-and-caching"
)

# Knit from within vignettes/ so that any figures land in the right place with
# relative paths intact.
old <- setwd("vignettes")
on.exit(setwd(old), add = TRUE)

for (v in vignettes) {
  input <- paste0("vignettes/", v, ".Rmd.orig")
  output <- paste0("vignettes/", v, ".Rmd")

  if (!file.exists(input)) {
    warning("Skipping ", input, ": not found.")
    next
  }

  message("Knitting ", input, " -> ", output)
  knitr::knit(input = input, output = output)
}

message(
  "\nDone. Check the generated .Rmd files for:\n",
  "  * error/warning chunks that produced the intended message\n",
  "  * output that is too long and should be truncated at the source\n",
  "  * any absolute paths or machine-specific detail that leaked into output\n"
)

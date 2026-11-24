# Check that every shipped figure is pixel-identical to an unoptimised render.
#
# vignettes/precompute.R and devtools::build_readme() draw figures with ragg
# and recompress them with optipng, which must be lossless: these are data
# maps, and fill colour encodes the measurement (Plan 04 PERF-12). This script
# knits copies of the same sources again with a no-op `optipng` first on the
# PATH, so the figures stay unoptimised, and compares each one with the
# committed figure of the same name. Every pair must have the same dimensions
# and zero differing pixels (ImageMagick's "AE" metric).
#
# Run from the package root, right after precompute.R and build_readme(), with
# GTROPIC_PRECOMPUTE_CACHE set to the cache those runs used:
#
#   GTROPIC_PRECOMPUTE_CACHE=/path/to/cache Rscript data-raw/check-figure-quality.R
#
# The plot captions record when the data were downloaded, and a cached file
# keeps its original download time, so re-rendering from the same cache draws
# identical captions and downloads nothing.

library(gtropic)

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("magick is required: install.packages(\"magick\").")
}
cache_dir <- Sys.getenv("GTROPIC_PRECOMPUTE_CACHE")
if (!nzchar(cache_dir) || !dir.exists(cache_dir)) {
  stop("Set GTROPIC_PRECOMPUTE_CACHE to the cache used by precompute.R.")
}

work <- tempfile("figure-check-")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)

# A stand-in optipng that leaves its input alone.
bin <- file.path(work, "bin")
dir.create(bin)
writeLines(c("#!/bin/sh", "exit 0"), file.path(bin, "optipng"))
Sys.chmod(file.path(bin, "optipng"), "0755")
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(bin, old_path, sep = .Platform$path.sep))
on.exit(Sys.setenv(PATH = old_path), add = TRUE)

old_options <- options(
  gtropic.cache_dir = cache_dir,
  gtropic.cache_enabled = TRUE,
  gtropic.quiet = TRUE,
  width = 80
)
on.exit(options(old_options), add = TRUE)

# Knit `source` (relative to the package root) inside a copy of its directory
# and return the directory the figures were written to.
render_unoptimised <- function(source, extra = character()) {
  dest <- file.path(work, dirname(source))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(c(source, extra), dest, overwrite = TRUE)
  old <- setwd(dest)
  on.exit(setwd(old), add = TRUE)
  knitr::opts_chunk$set(error = FALSE)
  knitr::knit(basename(source), output = tempfile(fileext = ".md"), quiet = TRUE)
  dest
}

compare_dir <- function(rendered, committed, pattern = "\\.png$") {
  files <- list.files(committed, pattern = pattern)
  rows <- lapply(files, function(f) {
    new <- file.path(rendered, f)
    if (!file.exists(new)) {
      return(data.frame(file = file.path(committed, f), status = "not re-rendered"))
    }
    a <- magick::image_read(file.path(committed, f))
    b <- magick::image_read(new)
    ia <- magick::image_info(a)
    ib <- magick::image_info(b)
    if (ia$width != ib$width || ia$height != ib$height) {
      return(data.frame(file = file.path(committed, f), status = "size differs"))
    }
    ae <- attr(magick::image_compare(a, b, metric = "AE"), "distortion")
    data.frame(
      file = file.path(committed, f),
      status = if (ae == 0) "identical" else paste(ae, "pixels differ"),
      optimised_bytes = file.size(file.path(committed, f)),
      unoptimised_bytes = file.size(new)
    )
  })
  do.call(rbind, lapply(rows, function(r) {
    r[setdiff(c("file", "status", "optimised_bytes", "unoptimised_bytes"), names(r))] <- NA
    r
  }))
}

vignettes <- list.files("vignettes", pattern = "\\.Rmd\\.orig$", full.names = TRUE)
vignettes <- vignettes[!startsWith(basename(vignettes), "_")]
articles <- list.files("vignettes/articles", pattern = "\\.Rmd\\.orig$", full.names = TRUE)

for (source in vignettes) {
  message("Re-rendering ", source)
  render_unoptimised(source, extra = "vignettes/_next.Rmd.orig")
}
for (source in articles) {
  message("Re-rendering ", source)
  render_unoptimised(source)
}
message("Re-rendering README.Rmd")
render_unoptimised("README.Rmd")

result <- rbind(
  compare_dir(file.path(work, "vignettes", "figs"), "vignettes/figs"),
  if (dir.exists("vignettes/articles/figs")) {
    compare_dir(file.path(work, "vignettes", "articles", "figs"), "vignettes/articles/figs")
  },
  compare_dir(file.path(work, "man", "figures"), "man/figures", "^README-.*\\.png$")
)
print(result, row.names = FALSE)

bad <- result$status != "identical"
if (any(bad)) {
  stop(sum(bad), " figure(s) are not identical to an unoptimised render.")
}
message(
  "All ", nrow(result), " figures are pixel-identical to an unoptimised render (",
  format(sum(result$unoptimised_bytes), big.mark = ","), " -> ",
  format(sum(result$optimised_bytes), big.mark = ","), " bytes)."
)

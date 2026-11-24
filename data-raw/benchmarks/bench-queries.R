# Benchmark three live queries, cold and warm cache (Plan 04 PERF-00).
#
# Run from the package root:
#
#   Rscript data-raw/benchmarks/bench-queries.R
#
# Each (query, cache state) case runs in a fresh R process under /usr/bin/time
# so the maximum resident set size includes Arrow's allocations, which R's own
# memory profiler does not see. The cache lives in a temporary directory per
# query: the cold run starts from an empty cache, the warm run reuses it. The
# user's real cache is never touched.
#
# Output: one row per case with elapsed seconds, R heap allocation
# (`bench::mark()` `mem_alloc`), maximum RSS, bytes downloaded and the number of
# Dataverse API requests (one per dataset listed, version looked up or file
# downloaded; retries are not counted). Paste the table into the
# Benchmarks section of PROGRESS.md. Do not commit output.
#
# These queries hit the live Harvard Dataverse server and download tens of MB.
# Keep the set to these three.

queries <- list(
  katrina = list(
    label = "Katrina 2005, wind",
    args = list(STORM_NAME = "Katrina-2005", tables = c("wind", "storm_metadata"))
  ),
  philippines = list(
    label = "Philippines 2015 + precip",
    args = list(
      WHO_ENTITY = "Philippines", date_range = 2015,
      tables = c("wind", "precip", "storm_metadata")
    )
  ),
  who_region = list(
    label = "WHO region, 5 years",
    args = list(
      WHO_REGION = "Americas", date_range = c("2015-01-01", "2019-12-31"),
      tables = c("wind", "adm2", "storm_metadata")
    )
  )
)

run_case <- function(name, cache_dir) {
  pkgload::load_all(".", quiet = TRUE)
  options(gtropic.cache_dir = cache_dir, gtropic.cache_enabled = TRUE)
  stats <- new.env()
  stats$calls <- 0L
  stats$bytes <- 0
  ns <- asNamespace("gtropic")
  # `per` counts one request per element of the first argument, for the
  # functions that request several datasets at once.
  counted <- function(f, download = FALSE, per = FALSE) {
    force(f)
    function(...) {
      stats$calls <- stats$calls + if (per) length(..1) else 1L
      out <- f(...)
      # dv_get_file() returns the path of the downloaded file.
      if (download) stats$bytes <- stats$bytes + file.size(out)
      out
    }
  }
  q <- queries[[name]]
  run <- function() {
    testthat::with_mocked_bindings(
      do.call(gtropic::gtropic_data, c(q$args, list(quiet = TRUE, confirm = FALSE))),
      dv_list_datasets = counted(ns$dv_list_datasets),
      dv_list_files_each = counted(ns$dv_list_files_each, per = TRUE),
      dv_version_labels = counted(ns$dv_version_labels, per = TRUE),
      dv_get_file = counted(ns$dv_get_file, download = TRUE),
      .package = "gtropic"
    )
  }
  b <- bench::mark(run(), iterations = 1, check = FALSE, memory = TRUE)
  cat(sprintf(
    "RESULT\t%.1f\t%s\t%.0f\t%d\n",
    as.numeric(b$median), format(b$mem_alloc), stats$bytes, stats$calls
  ))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2L) {
  run_case(args[[1]], args[[2]])
  quit(save = "no")
}

time_flag <- if (Sys.info()[["sysname"]] == "Darwin") "-l" else "-v"
script <- "data-raw/benchmarks/bench-queries.R"
rows <- list()
for (name in names(queries)) {
  cache_dir <- tempfile(paste0("gtropic-bench-", name, "-"))
  for (state in c("cold", "warm")) {
    out <- system2("/usr/bin/time", c(time_flag, "Rscript", script, name, cache_dir),
      stdout = TRUE, stderr = TRUE
    )
    result <- grep("^RESULT\t", out, value = TRUE)
    if (length(result) != 1L) {
      writeLines(out)
      stop("Benchmark case ", name, " (", state, ") failed.")
    }
    result <- strsplit(sub("^RESULT\t", "", result), "\t")[[1]]
    rss_line <- grep("maximum resident set size|Maximum resident set size", out, value = TRUE)
    rss <- as.numeric(gsub("[^0-9]", "", rss_line))
    # macOS reports bytes, GNU time reports kilobytes.
    rss_mb <- if (time_flag == "-l") rss / 1024^2 else rss / 1024
    rows[[length(rows) + 1L]] <- data.frame(
      query = queries[[name]]$label, cache = state,
      elapsed_s = as.numeric(result[[1]]), mem_alloc = result[[2]],
      max_rss_mb = round(rss_mb), bytes_downloaded = as.numeric(result[[3]]),
      api_calls = as.integer(result[[4]])
    )
  }
  unlink(cache_dir, recursive = TRUE)
}
print(do.call(rbind, rows), row.names = FALSE)

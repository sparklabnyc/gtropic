# Record the Dataverse responses that tests/testthat/test-recorded-dataverse.R
# replays with httptest2.
#
# Run from the package root, with network access:
#
#   Rscript data-raw/record-http-mocks.R
#
# The package's own discovery and download code runs against a recording mock:
# each request it makes to the test host "dv.test" is sent to Harvard
# Dataverse instead (without an API key), the response is trimmed, and saved
# in tests/testthat/dv/ under the request's "dv.test" URL. Recording through
# the package keeps the recorded URLs identical to the ones the tests make.
#
# Trimming keeps the fixtures small (Plan 04 PERF-15: under 200 KB in all) and
# free of anything that is not needed:
#   - file listings keep the files named in `keep_files`;
#   - version metadata keeps `version_fields`;
#   - downloads are stored as the final response from the storage host, with
#     only a Content-Type header (no redirect, cookies or signed URLs).
# The short host name and mock directory keep every path in the built tarball
# under 100 bytes.

pkgload::load_all(".", quiet = TRUE)

live <- "dataverse.harvard.edu"
host <- "dv.test"
dir <- file.path("tests", "testthat", "dv")
keep_files <- list(
  "doi:10.7910/DVN/EAVWC3" = c(
    "metadata/codebook.json",
    "02_wind/storm_metadata/1980.parquet",
    "02_wind/storm_metadata/2025.parquet",
    "02_wind/excluded_storms/2007.parquet"
  ),
  "doi:10.7910/DVN/FZBAKO" = c(
    "02_wind/storm_metadata/2026.parquet",
    "02_wind/excluded_storms/2026.parquet"
  )
)
version_fields <- c(
  "versionNumber", "versionMinorNumber", "versionState", "datasetPersistentId"
)

unlink(dir, recursive = TRUE)
options(httptest2.mock.paths = dir, gtropic.server = host)
Sys.setenv(DATAVERSE_KEY = "")

json_response <- function(url, body) {
  httr2::response(200,
    url = url,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body,
      auto_unbox = TRUE, null = "null", digits = NA
    ))
  )
}

record <- function(req) {
  live_url <- sub(paste0("^https://", host, "/"), paste0("https://", live, "/"), req$url)
  # `mock = NULL`: this request goes to the live server, not back to `record`.
  live_resp <- httr2::req_perform(
    httr2::req_user_agent(httr2::request(live_url), gtropic_user_agent()),
    mock = NULL
  )
  path <- httr2::url_parse(req$url)$path
  query <- httr2::url_parse(req$url)$query
  out <- if (grepl("/access/datafile/", path, fixed = TRUE)) {
    httr2::response(200,
      url = req$url,
      headers = list(`Content-Type` = "application/octet-stream"),
      body = httr2::resp_body_raw(live_resp)
    )
  } else {
    body <- httr2::resp_body_json(live_resp, simplifyVector = FALSE)
    if (endsWith(path, "/files")) {
      keep <- keep_files[[query$persistentId]]
      body$data <- Filter(function(f) {
        paste(f$directoryLabel, f$label, sep = "/") %in% keep
      }, body$data)
    } else if (grepl("/versions/[^/]+$", path)) {
      body$data <- body$data[intersect(version_fields, names(body$data))]
    }
    json_response(req$url, body)
  }
  httptest2::save_response(out, httptest2::build_mock_url(req))
  out
}

httr2::with_mocked_responses(record, {
  discovery <- discover_datasets(refresh = TRUE, quiet = TRUE)
  ctx <- fetch_context(discovery, list(enabled = FALSE, dir = NULL), quiet = TRUE)
  fetch_file("historical", "02_wind/excluded_storms", "2007.parquet", ctx)
  fetch_file("current", "02_wind/excluded_storms", "2026.parquet", ctx)
})

files <- list.files(dir, recursive = TRUE)
print(data.frame(
  file = files,
  bytes = file.size(file.path(dir, files)),
  tarball_path = nchar(file.path("gtropic", dir, files))
))

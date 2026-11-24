# Transport helpers that need no network (Plan 05 SEC-05, SEC-06).

test_that("is_hex_checksum() accepts the four supported digest lengths only", {
  expect_true(all(is_hex_checksum(c(
    strrep("a", 32L), strrep("B", 40L), strrep("0", 64L), strrep("f", 128L)
  ))))
  expect_false(any(is_hex_checksum(c(
    NA, "", strrep("a", 31L), strrep("a", 33L), strrep("g", 32L),
    paste0("../", strrep("a", 29L)), paste0(strrep("a", 31L), "/")
  ))))
})

test_that("listings drop checksums that are not hexadecimal digests", {
  records <- list(
    list(
      label = "2001.parquet", directoryLabel = "yearly",
      dataFile = list(id = 1L, md5 = paste0("../", strrep("a", 29L)), filesize = 10)
    ),
    list(
      label = "2002.parquet", directoryLabel = "yearly",
      dataFile = list(
        id = 2L, filesize = 10,
        checksum = list(type = "MD5", value = strrep("a", 32L))
      )
    )
  )
  listing <- dv_listing_from_records(records)
  expect_identical(listing$checksum, c(NA_character_, strrep("a", 32L)))
  expect_identical(listing$file_id, 1:2)
})

test_that("validate_server() keeps bare host names and strips schemes", {
  expect_identical(validate_server("dataverse.harvard.edu"), "dataverse.harvard.edu")
  expect_identical(validate_server("https://dataverse.harvard.edu/"), "dataverse.harvard.edu")
  expect_identical(validate_server("HTTP://dataverse.example.org//"), "dataverse.example.org")
  expect_identical(validate_server("localhost:8080"), "localhost:8080")
  expect_identical(validate_server("example.invalid"), "example.invalid")
})

test_that("validate_server() rejects anything that is not a host name", {
  bad <- list(
    "evil.example/path?", "user:pw@host", "a b", "host\n", "", NA_character_,
    c("a.example", "b.example"), 123, strrep("a", 254L), "-bad.example",
    "bad.example-", "host:123456", "host/../x"
  )
  for (x in bad) {
    expect_error(validate_server(x, arg = "gtropic.server"), "gtropic.server")
  }
})

test_that("an invalid gtropic.server option is rejected when it is read", {
  withr::local_options(gtropic.server = "evil.example/steal?x=")
  expect_error(opt_server(), "gtropic.server")
})

test_that("the default server is defined once", {
  withr::local_options(gtropic.server = NULL)
  expect_identical(opt_server(), opt_default("server"))
  .onLoad(NULL, "gtropic")
  expect_identical(getOption("gtropic.server"), opt_default("server"))
})

test_that("DATAVERSE_KEY is sent only to the configured server", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(
    gtropic.server = "dataverse.harvard.edu", rlib_warning_verbosity = "verbose"
  )

  expect_identical(dv_key("dataverse.harvard.edu"), "secret-token")
  expect_identical(dv_key("DataVerse.Harvard.edu"), "secret-token")
  w <- expect_warning(key <- dv_key("evil.example"), "Not sending")
  expect_identical(key, "")
  expect_no_match(conditionMessage(w), "secret-token", fixed = TRUE)

  withr::local_envvar(DATAVERSE_KEY = "")
  expect_no_warning(expect_identical(dv_key("evil.example"), ""))
})

# The API token a mocked request carries, or "".
sent_key <- function(req) {
  httr2::req_get_headers(req, redacted = "reveal")[["X-Dataverse-key"]] %||% ""
}

test_that("the transport receives no key for a host other than the configured one", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  keys <- character(0)
  httr2::local_mocked_responses(function(req) {
    keys <<- c(keys, sent_key(req))
    httr2::response(200, body = charToRaw("x"))
  })

  suppressWarnings(dv_get_file(1L, server = "evil.example", quiet = TRUE))
  dv_get_file(1L, server = "dataverse.harvard.edu", quiet = TRUE)
  expect_identical(keys, c("", "secret-token"))
})

test_that("replaying a manifest from another host does not send the token", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  keys <- character(0)
  httr2::local_mocked_responses(function(req) {
    keys <<- c(keys, sent_key(req))
    httr2::response(200, body = charToRaw("not the published bytes"))
  })
  m <- gtropic_manifest(gtropic_example())
  m$server <- "evil.example"

  expect_error(suppressWarnings(
    gtropic_data(manifest = m, cache = FALSE, quiet = TRUE)
  ))
  expect_gt(length(keys), 0L)
  expect_true(all(keys == ""))
})

test_that("the token is not sent to the storage host a download redirects to", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- list(
      url = req$url, key = sent_key(req)
    )
    if (grepl("^https://dataverse.harvard.edu/", req$url)) {
      httr2::response(303, headers = list(
        Location = "https://storage.example/bucket/file?signature=abc"
      ))
    } else {
      httr2::response(200, body = charToRaw("file bytes"))
    }
  })

  path <- dv_get_file(7L, quiet = TRUE)
  withr::defer(unlink(path))

  expect_identical(read_bytes(path), charToRaw("file bytes"))
  expect_identical(
    vapply(seen, `[[`, character(1), "url"),
    c(
      "https://dataverse.harvard.edu/api/access/datafile/7?format=original",
      "https://storage.example/bucket/file?signature=abc"
    )
  )
  expect_identical(vapply(seen, `[[`, character(1), "key"), c("secret-token", ""))
})

test_that("a download redirect cannot downgrade to plain HTTP", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(302, headers = list(Location = "http://storage.example/f"))
  })
  dest <- withr::local_tempfile()
  expect_error(
    dv_get_file(7L, server = "example.invalid", quiet = TRUE, dest = dest),
    "Refusing a redirect"
  )
  expect_false(file.exists(dest))
})

test_that("transport errors name the server that was contacted", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(404,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"status":"ERROR","message":"Dataset not found"}')
    )
  })
  err <- expect_error(
    dv_list_files("doi:10.0000/X", server = "other.example", quiet = TRUE),
    "other.example",
    class = "gtropic_error_network"
  )
  # The Dataverse error message is passed on.
  expect_match(conditionMessage(err), "Dataset not found")
})

test_that("requests are built for the four native API endpoints", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    expect_match(req$options$useragent, "^gtropic/")
    if (grepl("datafile", req$url)) {
      return(httr2::response(200, body = charToRaw("x")))
    }
    data <- if (grepl("contents$", req$url)) {
      '[{"type":"dataset","persistentUrl":"https://doi.org/10.0000/A"},
        {"type":"dataverse","alias":"sub"}]'
    } else if (grepl("files", req$url)) {
      '[]'
    } else {
      '{"versionNumber":3,"versionMinorNumber":1}'
    }
    httr2::response(200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(paste0('{"status":"OK","data":', data, '}'))
    )
  })

  expect_identical(
    dv_list_datasets("gtropic", server = "dv.example", quiet = TRUE)$doi,
    "doi:10.0000/A"
  )
  expect_identical(
    nrow(dv_list_files("doi:10.0000/A", "1.0", server = "dv.example", quiet = TRUE)),
    0L
  )
  expect_identical(
    dv_version_label("doi:10.0000/A", ":latest", server = "dv.example", quiet = TRUE),
    "3.1"
  )
  path <- dv_get_file(12L, server = "dv.example", quiet = TRUE)
  unlink(path)

  expect_identical(urls, c(
    "https://dv.example/api/dataverses/gtropic/contents",
    "https://dv.example/api/datasets/:persistentId/versions/1.0/files?persistentId=doi%3A10.0000%2FA",
    "https://dv.example/api/datasets/:persistentId/versions/:latest?persistentId=doi%3A10.0000%2FA&excludeFiles=true",
    "https://dv.example/api/access/datafile/12?format=original"
  ))
})

test_that("path segments from options cannot leave their segment", {
  expect_identical(dv_path_segment(":latest"), ":latest")
  expect_identical(dv_path_segment("1.0"), "1.0")
  expect_identical(dv_path_segment("../x?y"), "..%2Fx%3Fy")
  expect_error(dv_file_id(1.5), "whole positive number")
  expect_error(dv_file_id("1/../2"), "whole positive number")
  expect_identical(dv_file_id(14430328), "14430328")
})

# --- retries (Plan 04 PERF-09) -----------------------------------------------

test_that("only transient HTTP statuses are retried", {
  transient <- function(status) dv_is_transient(httr2::response(status))
  for (status in c(408L, 429L, 500L, 502L, 503L, 504L)) {
    expect_true(transient(status), label = status)
  }
  for (status in c(400L, 401L, 403L, 404L, 410L, 422L)) {
    expect_false(transient(status), label = status)
  }
})

test_that("retry waits grow and are capped", {
  expect_identical(vapply(1:6, dv_backoff, numeric(1)), c(1, 2, 4, 8, 8, 8))
  wait <- function(value) {
    dv_retry_after(httr2::response(503, headers = list(`Retry-After` = value)))
  }
  expect_identical(wait("5"), 5)
  expect_identical(wait("3600"), 60)
  expect_identical(dv_retry_after(httr2::response(503)), NA_real_)
})

test_that("requests carry the retry policy", {
  withr::local_options(gtropic.max_tries = 4L)
  req <- dv_request("example.invalid", "info", "version")
  expect_identical(req$policies$retry_max_tries, 4L)
  expect_true(req$policies$retry_on_failure)
})

test_that("a transient failure is retried and a permanent one fails at once", {
  web <- local_dataverse_app(dataverse_test_app())
  withr::local_options(gtropic.max_tries = 3L)

  expect_message(
    out <- dv_list_datasets("flaky", server = "dv.invalid"),
    "Retrying listing of collection flaky (1/2 retries used)",
    fixed = TRUE
  )
  expect_identical(nrow(out), 0L)
  expect_identical(app_hits(web, "flaky"), 2L)

  err <- expect_error(
    dv_list_datasets("missing", server = "dv.invalid"),
    class = "gtropic_error_network"
  )
  expect_match(conditionMessage(err), "Collection not found")
  expect_identical(app_hits(web, "missing"), 1L)
})

test_that("a refused connection is retried, then reported once", {
  skip_on_cran()
  local_mocked_bindings(
    dv_api_url = function(server) "http://127.0.0.1:9/api",
    dv_backoff = function(i) 0.05
  )
  withr::local_options(gtropic.max_tries = 3L)
  notices <- 0L
  err <- expect_error(
    withCallingHandlers(
      dv_list_datasets("gtropic", server = "dv.invalid"),
      message = function(m) {
        if (grepl("Retrying", conditionMessage(m))) notices <<- notices + 1L
        invokeRestart("muffleMessage")
      }
    ),
    class = "gtropic_error_network"
  )
  expect_identical(notices, 2L)
  expect_match(conditionMessage(err), "dv.invalid")
})

test_that("retry notices respect quiet", {
  web <- local_dataverse_app(dataverse_test_app())
  withr::local_options(gtropic.max_tries = 2L)
  expect_no_message(dv_list_datasets("flaky", server = "dv.invalid", quiet = TRUE))
})

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

test_that("the transport receives no key for a host other than the configured one", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  keys <- character(0)
  local_mocked_bindings(
    get_file = function(file, server, format, key, ...) {
      keys <<- c(keys, key)
      charToRaw("x")
    },
    .package = "dataverse"
  )

  suppressWarnings(dv_get_file(1L, server = "evil.example", quiet = TRUE))
  dv_get_file(1L, server = "dataverse.harvard.edu", quiet = TRUE)
  expect_identical(keys, c("", "secret-token"))
})

test_that("replaying a manifest from another host does not send the token", {
  withr::local_envvar(DATAVERSE_KEY = "secret-token")
  withr::local_options(gtropic.server = "dataverse.harvard.edu")
  keys <- character(0)
  local_mocked_bindings(
    get_file = function(file, server, format, key, ...) {
      keys <<- c(keys, key)
      charToRaw("not the published bytes")
    },
    .package = "dataverse"
  )
  m <- gtropic_manifest(gtropic_example())
  m$server <- "evil.example"

  expect_error(suppressWarnings(
    gtropic_data(manifest = m, cache = FALSE, quiet = TRUE)
  ))
  expect_gt(length(keys), 0L)
  expect_true(all(keys == ""))
})

test_that("transport errors name the server that was contacted", {
  local_mocked_bindings(
    dataset_files = function(...) stop("HTTP 404"),
    .package = "dataverse"
  )
  expect_error(
    dv_list_files("doi:10.0000/X", server = "other.example", quiet = TRUE),
    "other.example"
  )
})

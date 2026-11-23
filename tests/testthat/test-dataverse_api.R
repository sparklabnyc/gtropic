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

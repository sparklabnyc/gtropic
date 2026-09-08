candidates <- c("United States", "Mexico", "Philippines", "Costa Rica")

test_that("human-readable fields match case-insensitively and return canonical
           spelling", {
  expect_equal(
    match_values("united states", candidates, arg = "WHO_ENTITY"),
    "United States"
  )
  expect_equal(
    match_values("  MEXICO  ", candidates, arg = "WHO_ENTITY"),
    "Mexico"
  )
})

test_that("machine keys match exactly and are case-sensitive", {
  ids <- c("USA.1.1", "MEX.1.1")
  expect_equal(
    match_values("USA.1.1", ids, arg = "ADM2_ID", exact = TRUE),
    "USA.1.1"
  )
  expect_error(
    match_values("usa.1.1", ids, arg = "ADM2_ID", exact = TRUE),
    "did not match"
  )
})

test_that("a partial miss is still an error", {
  expect_error(
    match_values(c("Mexico", "Atlantis"), candidates, arg = "WHO_ENTITY"),
    "1 value in `WHO_ENTITY` did not match"
  )
})

test_that("suggestions are offered for near misses", {
  expect_error(
    match_values("Untied States", candidates, arg = "WHO_ENTITY"),
    "United States"
  )
  expect_error(
    match_values("Mexcio", candidates, arg = "WHO_ENTITY"),
    "Mexico"
  )
})

test_that("nonsense values get no suggestions", {
  expect_equal(suggest_matches("zzzzzzzzzzzz", candidates), character(0))
})

test_that("the suggestion threshold scales with string length", {
  # A short code tolerates one edit only.
  expect_equal(suggest_matches("USB", c("USA", "MEX")), "USA")
  expect_equal(suggest_matches("QQQ", c("USA", "MEX")), character(0))
})

test_that("the error message points at the discovery helper", {
  expect_error(
    match_values("Atlantis", candidates,
      arg = "WHO_ENTITY",
      helper = "gtropic_adm2()"
    ),
    "gtropic_adm2"
  )
})

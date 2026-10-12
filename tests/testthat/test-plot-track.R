testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("sf")

track_key <- function(unit = "knots", colour_name = "colour") {
  key <- data.frame(
    category = c("Tropical storm", "Hurricane"),
    threshold = c(63, Inf),
    unit = unit,
    colour = c("#4DAF4A", "#E41A1C")
  )
  names(key)[names(key) == "colour"] <- colour_name
  key
}

track_rows <- function() {
  tibble::tibble(
    STORM_ID = c(rep("A", 5), rep("B", 2), "C"),
    STORM_NAME = c(rep("Alpha", 5), rep("", 2), "Charlie"),
    ISO_TIME = as.POSIXct(c(
      "2020-01-01 00:00:00", "2020-01-01 03:00:00", "2020-01-01 06:00:00",
      "2020-01-01 09:00:00", "2020-01-01 12:00:00", "2020-01-01 00:00:00",
      "2020-01-01 06:00:00", "2020-01-01 00:00:00"
    ), tz = "UTC"),
    LON = c(0, 1, 2, 3, 4, 10, 11, NA),
    LAT = c(0, 1, 2, 3, 4, 10, 11, 0),
    USA_WIND = c(40, 50, 63, 64, 80, 30, 70, 20),
    USA_STATUS = c(rep("TS", 3), rep("HU", 2), "TD", "TS", "TD")
  )
}

test_that("track map keys require complete, ordered, constant-unit categories", {
  key <- plot_validate_track_map_key(track_key())
  expect_s3_class(key, "tbl_df")
  expect_equal(key$colour, c("#4DAF4A", "#E41A1C"))
  expect_equal(plot_validate_track_map_key(track_key(colour_name = "color"))$colour,
    c("#4DAF4A", "#E41A1C"))
  expect_error(plot_validate_track_map_key(data.frame(category = "A")), "category")
  expect_error(plot_validate_track_map_key(transform(track_key(), unit = c("knots", "mph"))),
    "constant")
  expect_error(plot_validate_track_map_key(transform(track_key(), threshold = c(63, 80))),
    "ending in Inf")
  expect_error(plot_validate_track_map_key(transform(track_key(), threshold = c(63, 63))),
    "strictly increasing")
  conflicting <- cbind(track_key(), color = c("red", "blue"))
  expect_error(plot_validate_track_map_key(conflicting), "conflicting")
})

test_that("wind conversion and categorization use inclusive ceilings", {
  expect_equal(plot_convert_track_wind(10, "m/s"), 5.14444444444444)
  expect_equal(plot_convert_track_wind(10, "mph"), 11.50779448)
  expect_equal(plot_convert_track_wind(10, "km/h"), 18.52)
  expect_equal(
    as.character(plot_categorize_track_wind(c(63, 64, NA), track_key())),
    c("Tropical storm", "Hurricane", NA)
  )
  metric_key <- track_key("m/s")
  metric_key$threshold <- c(20, Inf)
  expect_equal(as.character(plot_categorize_track_wind(c(38.8, 39), metric_key)),
    c("Tropical storm", "Hurricane"))
})

test_that("track points sample only requested UTC intervals while paths remain complete", {
  tracks <- plot_split_tracks(track_rows())
  expect_equal(nrow(plot_sample_track_points(tracks, 3)), 8L)
  expect_equal(nrow(plot_sample_track_points(tracks, 6)), 6L)
  expect_error(plot_sample_track_points(tracks, 4), "3, or 6")
  expect_error(plot_sample_track_points(transform(tracks, ISO_TIME = seq_len(nrow(tracks))), 3),
    "POSIXct")

  p <- plot_add_tracks(ggplot2::ggplot(), tracks,
    colour_metric = "USA_WIND", track_map_key = track_key(), track_interval = 6
  )
  expect_length(p$layers, 2)
  expect_s3_class(p$layers[[1]]$geom, "GeomPath")
  expect_s3_class(p$layers[[2]]$geom, "GeomPoint")
  expect_equal(nrow(p$layers[[1]]$data), nrow(tracks))
  expect_equal(nrow(p$layers[[2]]$data), 6L)
  expect_equal(rlang::as_label(p$layers[[2]]$mapping$fill), ".track_category")
  expect_null(p$layers[[2]]$mapping$size)
  expect_equal(p$layers[[2]]$aes_params$size, 2.5)
  expect_s3_class(p$scales$get_scales("fill"), "ScaleDiscrete")
  expect_null(p$scales$get_scales("size"))
})

test_that("track labels use the first valid point and a readable name fallback", {
  tracks <- plot_split_tracks(track_rows())
  labels <- plot_track_label_data(tracks)
  expect_equal(labels$STORM_ID, c("A", "B"))
  expect_equal(labels$.track_label, c("Alpha", "B"))
  expect_equal(labels$ISO_TIME, as.POSIXct(c("2020-01-01", "2020-01-01"), tz = "UTC"))

  p <- plot_add_tracks(ggplot2::ggplot(), tracks,
    colour_metric = "USA_WIND", track_map_key = track_key(), track_labels = TRUE
  )
  expect_length(p$layers, 3)
  expect_s3_class(p$layers[[3]]$geom, "GeomText")
  expect_equal(p$layers[[3]]$data$.track_label, c("Alpha", "B"))
})

test_that("explicit USA_STATUS support remains a discrete colour overlay", {
  tracks <- plot_split_tracks(track_rows())
  p <- plot_add_tracks(ggplot2::ggplot(), tracks,
    colour_metric = "USA_STATUS", palette = "mako", direction = -1,
    track_interval = 6
  )
  expect_s3_class(p$scales$get_scales("colour"), "ScaleDiscrete")
  expect_s3_class(p$scales$get_scales("size"), "ScaleContinuous")
  expect_equal(nrow(p$layers[[1]]$data), nrow(tracks))
  expect_equal(nrow(p$layers[[2]]$data), 6L)
  expect_equal(rlang::as_label(p$layers[[2]]$mapping$colour), "USA_STATUS")
})

test_that("track groups follow storm/time order and original dateline jumps", {
  tracks <- prepare_plot_data(plot_fixture(), "track")
  shuffled <- tracks[c(8, 2, 6, 4, 1, 7, 3, 5), ]
  out <- plot_split_tracks(shuffled)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$STORM_ID, tracks$STORM_ID)
  expect_equal(out$ISO_TIME, tracks$ISO_TIME)
  expect_equal(out$.plot_group, c(1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L))

  shifted <- shuffled
  shifted$original_LON <- shifted$LON
  shifted$LON <- shifted$LON %% 360
  split_shifted <- plot_split_tracks(shifted)
  expect_equal(split_shifted$.plot_group, out$.plot_group)
  expect_equal(split_shifted$original_LON, out$LON)
})

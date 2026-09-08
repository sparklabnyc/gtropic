#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils adist head
## usethis namespace: end
NULL

# Silence R CMD check notes for columns referenced inside Arrow/dplyr pipelines
# by bare name. We use .data$ where practical, but Arrow's dplyr bindings do not
# always tolerate the pronoun, so a handful of bare-name references remain.
utils::globalVariables(c(
  "ADM2_ID", "ADM2_NAME", "ADM2_GROUP", "WHO_ENTITY", "WHO_REGION",
  "STORM_ID", "STORM_NAME", "USA_ATCF_ID", "STORM_DIST_KM",
  "STORM_GENESIS_DATE_UTC", "LOCAL_DATETIME_STORM_CLOSEST",
  "LOCAL_DATE_STORM_CLOSEST", "POP_CENTROID_LAT", "POP_CENTROID_LON",
  "FLOOD_START_DATE", "FLOOD_END_DATE",
  "PRECIP_WINDOW_START", "PRECIP_WINDOW_END",
  "FLOOD_WINDOW_START", "FLOOD_WINDOW_END",
  "PRECIP_IN_FIELD", "FLOOD_IN_FIELD", "WIND_EXPOSURE", "YEAR",
  ".gtropic_date", ".gtropic_start", ".gtropic_end"
))

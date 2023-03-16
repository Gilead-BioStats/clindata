#' @importFrom dplyr filter
#'
#' @export
snapshot_data_points <- function(snapshot_date, data_points = clindata::edc_data_points, print_check_rows = TRUE) {
  data_points_snapshot <- data_points %>%
    dplyr::filter(
      impute_date(visit_dt) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(data_points, data_points_snapshot, "data_points")
  }

  data_points_snapshot
}

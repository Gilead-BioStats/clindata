snapshot_data_change_rate <- function(snapshot_date, data_change_rate = clindata::edc_data_change_rate) {
  data_change_rate_snapshot <- data_change_rate %>%
    dplyr::filter(
      impute_date(visit_dt) <= snapshot_date
    )

  check_rows(data_change_rate, data_change_rate_snapshot, "data_change_rate")

  data_change_rate_snapshot
}

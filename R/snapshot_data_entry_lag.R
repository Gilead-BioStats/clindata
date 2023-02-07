#' @importFrom dplyr filter
#'
#' @export
snapshot_data_entry_lag <- function(snapshot_date, data_entry_lag = clindata::edc_data_entry_lag, print_check_rows = TRUE) {
  data_entry_lag_snapshot <- data_entry_lag %>%
    dplyr::filter(
      impute_date(visit_dt) <= snapshot_date
    )

  if (print_check_rows)
  check_rows(data_entry_lag, data_entry_lag_snapshot, "data_entry_lag")

  data_entry_lag_snapshot
}

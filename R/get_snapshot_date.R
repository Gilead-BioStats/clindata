get_snapshot_date <- function(snapshot_date = NULL, interval = lubridate::month, n_intervals = 1) {
  if (is.null(snapshot_date)) {
    snapshot_date <- clindata::rawplus_visdt$visit_dt %>%
      ymd %>%
      max(na.rm = TRUE)
  } else {
    month(snapshot_date) <- month(snapshot_date) - n_intervals
  }

  cli::cli_alert_success('New snapshot date: {snapshot_date}')
  snapshot_date
}

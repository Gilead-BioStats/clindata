#' Returns an earlier date
#'
#' @param snapshot_date `Date` Initial snapshot date
#' @param interval `function` Interval size as implemented in {lubridate}, one of `year`, `month`, or `day`
#' @param n_intervals `numeric` Number of intervals
#'
#' @return `Date`
get_snapshot_date <- function(
  snapshot_date = NULL,
  interval = lubridate::month,
  n_intervals = 1
) {
  if (is.null(snapshot_date)) {
    snapshot_date <- clindata::rawplus_visdt$visit_dt %>%
      impute_date() %>%
      max(na.rm = TRUE)
  } else {
    interval(snapshot_date) <- interval(snapshot_date) - n_intervals
  }

  cat(paste0("\n", paste(rep("-", 64), collapse = ""), "\n"))
  cli::cli_alert_success("New snapshot date: {snapshot_date}")
  cat(paste0(paste(rep("-", 64), collapse = ""), "\n\n"))

  snapshot_date
}

#' @importFrom dplyr filter
#'
#' @export
snapshot_lb <- function(snapshot_date, dm, lb = clindata::rawplus_lb) {
  if (is.character(lb$lb_dt))
      lb$lb_dt <- substring(lb$lb_dt, 1, 10)

  lb_snapshot <- lb %>%
    dplyr::filter(
      subjid %in% dm$subjid,
      impute_date(lb_dt) <= snapshot_date
    )

  check_rows(lb, lb_snapshot, "lb")

  lb_snapshot
}

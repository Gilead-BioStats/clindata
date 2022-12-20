snapshot_lb <- function(snapshot_date, dm, lb = clindata::rawplus_lb) {
  lb_snapshot <- lb %>%
    dplyr::filter(
      subjid %in% dm$subjid,
      impute_date(substring(lb_dt, 1, 10)) <= snapshot_date
    )

  check_rows(lb, lb_snapshot, "lb")

  lb_snapshot
}

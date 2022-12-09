snapshot_enroll <- function(snapshot_date, enroll = clindata::rawplus_enroll) {
  enroll_snapshot <- enroll %>%
    dplyr::filter(
      impute_date(enroll_dt) <= snapshot_date
    )

  check_rows(enroll, enroll_snapshot, 'enroll')

  enroll_snapshot
}

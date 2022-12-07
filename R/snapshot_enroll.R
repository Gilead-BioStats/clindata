snapshot_enroll <- function(snapshot_date, dm, enroll = clindata::rawplus_enroll) {
  enroll_snapshot <- enroll %>%
    dplyr::filter(
      subjid %in% dm$subjid
    )

  check_rows(enroll, enroll_snapshot, 'enroll')

  enroll_snapshot
}

snapshot_ie <- function(snapshot_date, dm, ie = clindata::rawplus_ie) {
  ie_snapshot <- ie %>%
    filter(
      subjid %in% dm$subjid
    )

  check_rows(ie, ie_snapshot, 'ie')

  ie_snapshot
}

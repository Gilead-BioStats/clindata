snapshot_protdev <- function(snapshot_date, dm, protdev = clindata::rawplus_protdev) {
  protdev_snapshot <- protdev %>%
    mutate(
      dv_dt = ymd(dv_dt)
    ) %>%
    filter(
      dv_dt <= snapshot_date
    )

  check_rows(protdev, protdev_snapshot, 'protdev')

  protdev_snapshot
}

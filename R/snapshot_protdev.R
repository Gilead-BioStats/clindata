snapshot_protdev <- function(snapshot_date, dm, protdev = clindata::rawplus_protdev) {
  protdev_snapshot <- protdev %>%
    dplyr::filter(
      impute_date(dv_dt) <= snapshot_date
    )

  check_rows(protdev, protdev_snapshot, 'protdev')

  protdev_snapshot
}

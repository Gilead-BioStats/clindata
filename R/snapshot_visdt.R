snapshot_visdt <- function(snapshot_date, visdt = clindata::rawplus_visdt) {
  visdt_snapshot <- visdt %>%
    dplyr::filter(
      lubridate::ymd(visit_dt) <= snapshot_date
    )

  check_rows(visdt, visdt_snapshot, 'visdt')

  visdt_snapshot
}

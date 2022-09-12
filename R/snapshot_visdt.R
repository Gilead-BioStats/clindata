snapshot_visdt <- function(snapshot_date, visdt = clindata::rawplus_visdt) {
  visdt_snapshot <- visdt %>%
    mutate(
      visit_dt = ymd(visit_dt)
    ) %>%
    filter(
      visit_dt <= snapshot_date
    )

  check_rows(visdt, visdt_snapshot, 'visdt')

  visdt_snapshot
}

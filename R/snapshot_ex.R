snapshot_ex <- function(snapshot_date, ex = clindata::rawplus_ex) {
  ex_snapshot <- ex %>%
    mutate(
      exst_dt = ymd(exst_dt),
      exen_dt = ymd(exen_dt)
    ) %>%
    filter(
      exst_dt <= snapshot_date
    ) %>%
    mutate(
      exen_dt = if_else(
        exen_dt > snapshot_date,
        snapshot_date,
        exen_dt
      )
    )

  check_rows(ex, ex_snapshot, 'ex')

  ex_snapshot
}

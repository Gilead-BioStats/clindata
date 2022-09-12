snapshot_dm <- function(snapshot_date, visdt, ex, dm = clindata::rawplus_dm) {
  rfpen_dt <- visdt %>%
    group_by(subjid) %>%
    summarize(
      rfpen_dt = max(visit_dt)
    )

  rfxen_dt <- ex %>%
    group_by(subjid) %>%
    summarize(
      rfxen_dt = max(exen_dt)
    )

  dm_snapshot <- dm %>%
    filter(
      rfpst_dt <= snapshot_date
    ) %>%
    select(
      -rfpen_dt,
      -rfxen_dt
    ) %>%
    left_join(
      rfpen_dt,
      'subjid'
    ) %>%
    left_join(
      rfxen_dt,
      'subjid'
    )

  check_rows(dm, dm_snapshot, 'dm')

  dm_snapshot
}

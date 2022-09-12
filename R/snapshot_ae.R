snapshot_ae <- function(snapshot_date, dm, ae = clindata::rawplus_ae) {
  ae_snapshot <- ae %>%
    filter(
      aest_dt <= snapshot_date
    ) %>%
    left_join(
      dm %>% select(subjid, rfpen_dt, rfxen_dt),
      'subjid'
    ) %>%
    mutate(
      aeen_dt = ymd(aeen_dt),
      aeen_dt = if_else(
        aeen_dt > snapshot_date,
        snapshot_date,
        aeen_dt
      )
    ) %>%
    select(
      -rfpen_dt,
      -rfxen_dt
    )

  check_rows(ae, ae_snapshot, 'ae')

  ae_snapshot
}

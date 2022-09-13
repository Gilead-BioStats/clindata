snapshot_sdrgcomp <- function(snapshot_date, dm, sdrgcomp = clindata::rawplus_sdrgcomp) {
  sdrgcomp_snapshot <- sdrgcomp %>%
    inner_join(
      dm %>% select(subjid, rfxen_dt, rfxen_dt0),
      'subjid'
    ) %>%
    mutate(
      sdrgyn = if_else(
        rfxen_dt < rfxen_dt0,
        '',
        sdrgyn
      ),
      sdrgreas = if_else(
        rfxen_dt < rfxen_dt0,
        '',
        sdrgreas
      )
    ) %>%
    select(
      -rfxen_dt,
      -rfxen_dt0
    )

  check_rows(sdrgcomp, sdrgcomp_snapshot, 'sdrgcomp')

  sdrgcomp_snapshot
}

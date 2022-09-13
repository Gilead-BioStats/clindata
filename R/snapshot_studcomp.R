snapshot_studcomp <- function(snapshot_date, dm, studcomp = clindata::rawplus_studcomp) {
  studcomp_snapshot <- studcomp %>%
    inner_join(
      dm %>% select(subjid, rfpen_dt, rfpen_dt0),
      'subjid'
    ) %>%
    mutate(
      compyn = if_else(
        rfpen_dt < rfpen_dt0,
        '',
        compyn
      ),
      compreas = if_else(
        rfpen_dt < rfpen_dt0,
        '',
        compreas
      )
    ) %>%
    select(
      -rfpen_dt,
      -rfpen_dt0
    )

  check_rows(studcomp, studcomp_snapshot, 'studcomp')

  studcomp_snapshot
}

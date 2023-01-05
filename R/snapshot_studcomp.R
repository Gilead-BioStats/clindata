snapshot_studcomp <- function(snapshot_date, dm, studcomp = clindata::rawplus_studcomp) {
  studcomp_snapshot <- studcomp %>%
    dplyr::inner_join(
      dm %>% dplyr::select(subjid, rfpen_dt, rfpen_dt0),
      "subjid"
    ) %>%
    dplyr::mutate(
      compyn = dplyr::if_else(
        rfpen_dt < rfpen_dt0,
        "",
        compyn
      ),
      compreas = dplyr::if_else(
        rfpen_dt < rfpen_dt0,
        "",
        compreas
      )
    ) %>%
    dplyr::select(
      -rfpen_dt,
      -rfpen_dt0
    )

  check_rows(studcomp, studcomp_snapshot, "studcomp")

  studcomp_snapshot
}

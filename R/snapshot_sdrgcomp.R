snapshot_sdrgcomp <- function(snapshot_date, dm, sdrgcomp = clindata::rawplus_sdrgcomp) {
  sdrgcomp_snapshot <- sdrgcomp %>%
    dplyr::inner_join(
      dm %>% select(subjid, rfxen_dt, rfxen_dt0),
      "subjid"
    ) %>%
    dplyr::mutate(
      sdrgyn = dplyr::if_else(
        rfxen_dt < rfxen_dt0,
        "",
        sdrgyn
      ),
      sdrgreas = dplyr::if_else(
        rfxen_dt < rfxen_dt0,
        "",
        sdrgreas
      )
    ) %>%
    dplyr::select(
      -rfxen_dt,
      -rfxen_dt0
    )

  check_rows(sdrgcomp, sdrgcomp_snapshot, "sdrgcomp")

  sdrgcomp_snapshot
}

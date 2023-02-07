#' @importFrom dplyr filter if_else left_join select
#'
#' @export
snapshot_ae <- function(snapshot_date, dm, ae = clindata::rawplus_ae) {
  ae_snapshot <- ae %>%
    dplyr::left_join(
      dm %>% dplyr::select(subjid, rfpen_dt, rfxen_dt),
      "subjid"
    ) %>%
    dplyr::mutate(
      aeen_dt = dplyr::if_else(
        impute_date(aeen_dt) > snapshot_date,
        as.character(snapshot_date),
        as.character(aeen_dt)
      )
    ) %>%
    dplyr::filter(
      impute_date(aest_dt) <= snapshot_date
    ) %>%
    dplyr::select(
      -rfpen_dt,
      -rfxen_dt
    )

  check_rows(ae, ae_snapshot, "ae")

  ae_snapshot
}

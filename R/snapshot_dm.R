snapshot_dm <- function(snapshot_date, visdt = NULL, ex = NULL, dm = clindata::rawplus_dm) {
  if (!is.null(visdt) & !is.null(ex)) {
    rfpen_dt <- visdt %>%
        dplyr::group_by(subjid) %>%
        dplyr::summarize(
            rfpen_dt = max(visit_dt)
        )

    rfxen_dt <- ex %>%
        dplyr::group_by(subjid) %>%
        dplyr::summarize(
            rfxen_dt = max(exen_dt)
        )

    dm_snapshot <- dm %>%
        dplyr::filter(
            impute_date(rfpst_dt) <= snapshot_date
        ) %>%
        dplyr::rename(
            rfpen_dt0 = rfpen_dt,
            rfxen_dt0 = rfxen_dt
        ) %>%
        dplyr::left_join(
            rfpen_dt,
            "subjid"
        ) %>%
        dplyr::left_join(
            rfxen_dt,
            "subjid"
        )
  } else {
    dm_snapshot <- dm %>%
        dplyr::filter(
            impute_date(rfpst_dt) <= snapshot_date
        ) %>%
        dplyr::rename(
            rfpen_dt0 = rfpen_dt,
            rfxen_dt0 = rfxen_dt
        ) %>%
        dplyr::mutate(
            rfpen_dt = snapshot_date,
            rfxen_dt = snapshot_date
        )
  }

  check_rows(dm, dm_snapshot, "dm")

  dm_snapshot
}

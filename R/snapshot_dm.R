#' @import dplyr
#'
#' @export
snapshot_dm <- function(snapshot_date, visdt = NULL, ex = NULL, dm = clindata::rawplus_dm, print_check_rows = TRUE) {
  if (!is.null(visdt) & !is.null(ex)) {
    lastparticipantdate <- visdt %>%
      dplyr::group_by(subjid) %>%
      dplyr::summarize(
        lastparticipantdate = max(impute_date(visit_dt))
      )

    lastdosedate <- ex %>%
      dplyr::group_by(subjid) %>%
      dplyr::summarize(
        lastdosedate = max(impute_date(exen_dt))
      )

    dm_snapshot <- dm %>%
      dplyr::filter(
        impute_date(firstparticipantdate) <= snapshot_date
      ) %>%
      dplyr::rename(
        lastparticipantdate0 = lastparticipantdate,
        lastdosedate0 = lastdosedate
      ) %>%
      dplyr::left_join(
        lastparticipantdate,
        "subjid"
      ) %>%
      dplyr::left_join(
        lastdosedate,
        "subjid"
      )
  } else {
    dm_snapshot <- dm %>%
      dplyr::filter(
        impute_date(firstparticipantdate) <= snapshot_date
      ) %>%
      dplyr::rename(
        lastparticipantdate0 = lastparticipantdate,
        lastdosedate0 = lastdosedate
      ) %>%
      dplyr::mutate(
        lastparticipantdate = snapshot_date,
        lastdosedate = snapshot_date
      )
  }

  if (print_check_rows) {
    check_rows(dm, dm_snapshot, "dm")
  }

  dm_snapshot
}

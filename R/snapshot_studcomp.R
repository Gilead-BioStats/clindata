#' @importFrom dplyr if_else inner_join mutate select
#'
#' @export
snapshot_studcomp <- function(snapshot_date, dm, studcomp = clindata::rawplus_studcomp, print_check_rows = TRUE) {
  studcomp_snapshot <- studcomp %>%
    dplyr::inner_join(
      dm %>% dplyr::select(subjid, lastparticipantdate, lastparticipantdate0),
      "subjid"
    ) %>%
    dplyr::mutate(
      compyn = dplyr::if_else(
        lastparticipantdate < lastparticipantdate0,
        "",
        compyn
      ),
      compreas = dplyr::if_else(
        lastparticipantdate < lastparticipantdate0,
        "",
        compreas
      )
    ) %>%
    dplyr::select(
      -lastparticipantdate,
      -lastparticipantdate0
    )

  if (print_check_rows) {
    check_rows(studcomp, studcomp_snapshot, "studcomp")
  }

  studcomp_snapshot
}

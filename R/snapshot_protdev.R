#' @import dplyr
#' @importFrom readr parse_number
#'
#' @export
snapshot_protdev <- function(snapshot_date, dm, protdev = clindata::ctms_protdev, print_check_rows = TRUE) {
  if ("visit_nsv" %in% names(protdev)) {
    protdev_snapshot <- protdev %>%
      left_join(
        dm %>% select(subjid, firstparticipantdate),
        "subjid"
      ) %>%
      mutate(
        week = case_when(
          visit_nsv == "Screening" ~ -5,
          visit_nsv == "Baseline" ~ 0,
          grepl("week", visit_nsv, TRUE) ~ readr::parse_number(visit_nsv)
        ),
        dv_dt = if_else(
          dv_dt == "",
          impute_date(firstparticipantdate) + 7 * week,
          impute_date(dv_dt)
        )
      ) %>%
      select(-firstparticipantdate, -week)
  } else {
    protdev_snapshot <- protdev
  }

  protdev_snapshot1 <- protdev_snapshot %>%
    dplyr::filter(
      impute_date(dv_dt) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(protdev, protdev_snapshot, "protdev")
  }

  protdev_snapshot
}

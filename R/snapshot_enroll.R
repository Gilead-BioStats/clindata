#' @importFrom dplyr filter
#'
#' @export
snapshot_enroll <- function(snapshot_date, enroll = clindata::rawplus_enroll, print_check_rows = TRUE) {
  enroll_snapshot <- enroll %>%
    dplyr::filter(
      impute_date(enroll_dt) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(enroll, enroll_snapshot, "enroll")
  }

  enroll_snapshot
}

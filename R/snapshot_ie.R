#' @importFrom dplyr filter
#'
#' @export
snapshot_ie <- function(snapshot_date, dm, ie = clindata::rawplus_ie, print_check_rows = TRUE) {
  ie_snapshot <- ie %>%
    dplyr::filter(
      subjid %in% dm$subjid
    )

  if (print_check_rows) {
    check_rows(ie, ie_snapshot, "ie")
  }

  ie_snapshot
}

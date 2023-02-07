#' @importFrom dplyr filter
#'
#' @export
snapshot_visdt <- function(snapshot_date, visdt = clindata::rawplus_visdt, print_check_rows = TRUE) {
  visdt_snapshot <- visdt %>%
    dplyr::filter(
      impute_date(visit_dt) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(visdt, visdt_snapshot, "visdt")
  }

  visdt_snapshot
}

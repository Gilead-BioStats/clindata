#' @importFrom dplyr filter
#'
#' @export
snapshot_data_pages <- function(snapshot_date, data_pages = clindata::edc_data_pages, print_check_rows = TRUE) {

  data_pages_snapshot <- data_pages %>%
    dplyr::filter(
      impute_date(visit_date) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(data_pages, data_pages_snapshot, "data_pages")
  }

  data_pages_snapshot
}

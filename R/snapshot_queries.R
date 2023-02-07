#' @importFrom dplyr filter
#'
#' @export
snapshot_queries <- function(snapshot_date, queries = clindata::edc_queries, print_check_rows = TRUE) {
  queries_snapshot <- queries %>%
    dplyr::filter(
      impute_date(qryopendate) <= snapshot_date
    )

  if (print_check_rows) {
    check_rows(queries, queries_snapshot, "queries")
  }

  queries_snapshot
}

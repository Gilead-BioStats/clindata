snapshot_queries <- function(snapshot_date, queries = clindata::edc_queries) {
  queries_snapshot <- queries %>%
    dplyr::filter(
      impute_date(qryopendate) <= snapshot_date
    )

  check_rows(queries, queries_snapshot, "queries")

  queries_snapshot
}

#' @importFrom dplyr filter if_else mutate
#'
#' @export
snapshot_ex <- function(snapshot_date, ex = clindata::rawplus_ex) {
  ex_snapshot <- ex %>%
    dplyr::filter(
      impute_date(exst_dt) <= snapshot_date
    ) %>%
    dplyr::mutate(
      exen_dt = dplyr::if_else(
        impute_date(exen_dt) > snapshot_date,
        as.character(snapshot_date),
        exen_dt
      )
    )

  check_rows(ex, ex_snapshot, "ex")

  ex_snapshot
}

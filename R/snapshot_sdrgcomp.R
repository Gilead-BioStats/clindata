#' @importFrom dplyr if_else inner_join mutate select
#'
#' @export
snapshot_sdrgcomp <- function(snapshot_date, dm, sdrgcomp = clindata::rawplus_sdrgcomp, print_check_rows = TRUE) {
  sdrgcomp_snapshot <- sdrgcomp %>%
    dplyr::inner_join(
      dm %>% select(subjid, lastdosedate, lastdosedate0),
      "subjid"
    ) %>%
    dplyr::mutate(
      sdrgyn = dplyr::if_else(
        lastdosedate < lastdosedate0,
        "",
        sdrgyn
      ),
      sdrgreas = dplyr::if_else(
        lastdosedate < lastdosedate0,
        "",
        sdrgreas
      )
    ) %>%
    dplyr::select(
      -lastdosedate,
      -lastdosedate0
    )

  if (print_check_rows) {
    check_rows(sdrgcomp, sdrgcomp_snapshot, "sdrgcomp")
  }

  sdrgcomp_snapshot
}

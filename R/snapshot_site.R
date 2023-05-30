#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_site <- function(
  snapshot_date,
  dm = clindata::rawplus_dm,
  site = clindata::ctms_site,
  print_check_rows = TRUE) {
  n_enrolled <- dm %>%
    filter(
      .data$firstparticipantdate <= snapshot_date
    ) %>%
    group_by(siteid) %>%
    summarize(
      enrolled_participants = n()
    ) %>%
    ungroup()

  site_snapshot <- site %>%
    inner_join(
      n_enrolled,
      c("site_num" = "siteid")
    )

  if (print_check_rows) {
    check_rows(site, site_snapshot, "site")
  }

  site_snapshot
}

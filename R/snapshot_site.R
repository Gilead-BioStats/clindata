#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_site <- function(
  snapshot_date,
  dm = clindata::rawplus_dm,
  site = clindata::ctms_site
) {
  n_enrolled <- dm %>%
    filter(
      .data$rfpst_dt <= snapshot_date
    ) %>%
    group_by(siteid) %>%
    summarize(
      enrolled_participants = n()
    ) %>%
    ungroup()

  site_snapshot <- site %>%
    inner_join(
      n_enrolled,
      c("SITE_NUM" = "siteid")
    )

  check_rows(site, site_snapshot, "site")

  site_snapshot
}

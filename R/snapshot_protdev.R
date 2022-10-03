snapshot_protdev <- function(snapshot_date, dm, protdev = clindata::rawplus_protdev) {
  protdev_snapshot <- protdev %>%
    left_join(
      dm %>% select(subjid, rfpst_dt),
      'subjid'
    ) %>%
    mutate(
      week = case_when(
        visit_nsv == 'Screening' ~ -5,
        visit_nsv == 'Baseline' ~ 0,
        grepl('week', visit_nsv, TRUE) ~ readr::parse_number(visit_nsv)
      ),
      dv_dt = if_else(
        dv_dt == '',
        impute_date(rfpst_dt) + 7*week,
        lubridate::ymd(dv_dt)
      ) %>% as.character
    ) %>%
    dplyr::filter(
      impute_date(dv_dt) <= snapshot_date
    ) %>%
    select(-rfpst_dt, -week)

  check_rows(protdev, protdev_snapshot, 'protdev')

  protdev_snapshot
}

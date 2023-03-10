#' Process [ lb ] data
#'
#' Reduce the size of [ lb ] data.
#'
#' @param lb `data.frame` [ lb ] data
#' @param dm `data.frame` [ dm ] data
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr case_when filter if_else inner_join mutate select
#' @importFrom magrittr %>%

rawplus_2_process_3_lb <- function(lb, dm) {
  cli::cli_alert_warning(
    "[ treatmentemergent ] and [ alertsimplified ] temporarily derived for [ lb ]."
  )

  lb_processed <- lb %>%
    dplyr::filter(
      battrnam %in% c(
        "HEMATOLOGY&DIFFERENTIAL PANEL",
        "CHEMISTRY PANEL"
      ),
      !is.na(siresn)
    ) %>%
    dplyr::select(
      subjid, # participant
      visnam, visnum, lb_dt, # timing
      battrnam, lbtstnam, # measure
      siresn, sinrlo, sinrhi, toxgrg_nsv, alrtfl # result
    ) %>%
    dplyr::inner_join(
      dm %>% dplyr::select(subjid, firstdosedate, lastdosedate),
      "subjid"
    ) %>%
    dplyr::mutate(
      treatmentemergent = dplyr::case_when(
        lb_dt == "" | firstdosedate == "" | lastdosedate == "" ~ "",
        as.Date(firstdosedate) <= as.Date(lb_dt) & as.Date(lb_dt) <= (as.Date(lastdosedate) + 30) ~ "Y",
        TRUE ~ ""
      ),
      alertsimplified = dplyr::case_when(
        alrtfl %in% c("H", "HN", "HT", "HP") ~ "H",
        alrtfl %in% c("L", "LN", "LT", "LP") ~ "L",
        TRUE ~ alrtfl
      )
    ) %>%
    dplyr::select(-firstdosedate, -lastdosedate)

  n_rows_removed <- nrow(lb) - nrow(lb_processed)

  if (n_rows_removed) {
    cli::cli_alert_warning(
      "{n_rows_removed} rows removed from [ lb ]."
    )
  }

  lb_processed
}

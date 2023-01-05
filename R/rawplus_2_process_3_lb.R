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
    "[ lb_te ], [ alrtfl_s ], and [ lb_abn ] temporarily derived for [ lb ]."
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
      battrnam, lbtstnam, lbtstcd, # measure
      siresn, sinrlo, sinrhi, toxgr, alrtfl # result
    ) %>%
    dplyr::inner_join(
      dm %>% dplyr::select(subjid, rfxst_dt, rfxen_dt),
      "subjid"
    ) %>%
    dplyr::mutate(
      lb_te = dplyr::case_when(
        lb_dt == "" | rfxst_dt == "" | rfxen_dt == "" ~ "",
        as.Date(rfxst_dt) <= as.Date(lb_dt) & as.Date(lb_dt) <= (as.Date(rfxen_dt) + 30) ~ "Y",
        TRUE ~ ""
      ),
      alrtfl_s = dplyr::case_when(
        alrtfl %in% c("H", "HN", "HT", "HP") ~ "H",
        alrtfl %in% c("L", "LN", "LT", "LP") ~ "L",
        TRUE ~ alrtfl
      ),
      lb_abn = dplyr::if_else(
        toxgr %in% c("1", "2", "3", "4") & alrtfl_s != "N",
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(-rfxst_dt, -rfxen_dt)

  n_rows_removed <- nrow(lb) - nrow(lb_processed)

  if (n_rows_removed) {
    cli::cli_alert_warning(
      "{n_rows_removed} rows removed from [ lb ]."
    )
  }

  lb_processed
}

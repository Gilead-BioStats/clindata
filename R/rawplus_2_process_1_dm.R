#' Process [ dm ] domain
#'
#' @param dm `data.frame` [ dm ] data
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr filter
#' @importFrom magrittr %>%

rawplus_2_process_1_dm <- function(dm) {
    dm_processed <- dm %>%
        dplyr::filter(
            .data$subjid != ""
        )

    n_rows_filtered <- nrow(dm) - nrow(dm_processed)

    if (n_rows_filtered)
        cli::cli_alert_warning(
            '[ {n_rows_filtered} ] rows with a missing subject ID removed from [ dm ].'
        )

    dm_processed
}

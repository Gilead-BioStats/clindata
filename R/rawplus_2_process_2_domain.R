#' Process domain data
#'
#' Remove rows from domain data where the associated subject ID does not exist in [ dm ].
#'
#' @param data `data.frame` domain data
#' @param dm `data.frame` [ dm ] data
#' @param domain `character` domain name
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr filter
#' @importFrom magrittr %>%

rawplus_2_process_2_domain <- function(data, dm, domain) {
    data_processed <- data %>%
        dplyr::filter(
            .data$subjid %in% dm$subjid
        )

    n_rows_removed <- nrow(data) - nrow(data_processed)

    if (n_rows_removed)
        cli::cli_alert_warning(
            '{n_rows_removed} rows removed from [ {domain} ] for subject IDs not present in [ dm ].'
        )

    data_processed
}

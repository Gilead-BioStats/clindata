#' Export EDC data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr iwalk
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data

edc_3_export <- function(
    datasets_processed
) {
    datasets_processed %>%
        purrr::iwalk(function(data, domain) {
            edc_domain <- paste0('edc_', domain)

            assign(edc_domain, tibble::as_tibble(data))

            do.call(
                'use_data',
                list(
                    as.name(edc_domain),
                    overwrite = TRUE
                )
            )

            cli::cli_alert_success(
                '[ {stringr::str_pad(edc_domain, 16, "both")} ] exported.'
            )
        })
}

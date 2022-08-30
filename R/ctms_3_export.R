#' Export CTMS data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr iwalk
#' @importFrom usethis use_data

ctms_3_export <- function(
    datasets_processed
) {
    datasets_processed %>%
        purrr::iwalk(function(data, domain) {
            ctms_domain <- paste0('ctms_', domain)

            assign(ctms_domain, data)

            do.call(
                'use_data',
                list(
                    as.name(ctms_domain),
                    overwrite = TRUE
                )
            )

            cli::cli_alert_success(
                '[ {stringr::str_pad(ctms_domain, 16, "both")} ] exported.'
            )
        })
}

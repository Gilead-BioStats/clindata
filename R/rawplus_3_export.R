#' Export [ rawplus ] data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr iwalk
#' @importFrom usethis use_data

rawplus_3_export <- function(
    datasets_processed
) {
    datasets_processed %>%
        purrr::iwalk(function(data, domain) {
            rawplus_domain <- paste0('rawplus_', domain)

            assign(rawplus_domain, data)

            do.call(
                'use_data',
                list(
                    as.name(rawplus_domain),
                    overwrite = TRUE
                )
            )

            cli::cli_alert_success(
                '[ {stringr::str_pad(rawplus_domain, 16, "both")} ] exported.'
            )
        })
}

#' Export CTMS data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr iwalk
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data

ctms_3_export <- function(
  datasets_processed,
  data_domain = "ctms"
) {
  datasets_processed %>%
    purrr::iwalk(function(data, domain) {
      ctms_domain <- paste0(data_domain, "_", domain)

      assign(ctms_domain, tibble::as_tibble(data))

      do.call(
        "use_data",
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

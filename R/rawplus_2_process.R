#' Process [ rawplus ] data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom magrittr %>%
#' @importFrom purrr imap

rawplus_2_process <- function(
  datasets
) {
  # Remove invalid records from [ dm ].
  dm <- rawplus_2_process_1_dm(datasets$dm)

  # Subset each domain on subject IDs found in [ dm ].
  datasets_processed <- datasets %>%
    purrr::imap(function(data, domain) {
      rawplus_2_process_2_domain(data, dm, domain)
    })

  # Further process problem domains.
  datasets_processed$lb <- rawplus_2_process_3_lb(datasets_processed$lb, dm)

  datasets_processed
}

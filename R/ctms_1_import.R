#' Import CTMS data
#'
#' @param data_path `character` path to data files
#'
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr imap
#' @importFrom rlang set_names
#' @importFrom stringr str_pad word

ctms_1_import <- function(
    data_path = file.path('data-raw', 'ctms')
) {
    stopifnot(
        '[ data_path ] does not exist.' = file.exists(data_path)
    )

    # Retrieve list of data files.
    data_files <- data_path %>%
        list.files(
            '\\.csv$',
            full.names = TRUE
        )

    # Parse domain name.
    domains <- data_files %>%
        purrr::map_chr(
            ~stringr::word(.x, -2, sep = '/|\\.')
        )

    # Ingest data files.
    datasets <- as.list(data_files) %>%
        purrr::imap(function(data_file, index) {
            domain <- domains[ index ]

            data <- read.csv(data_file)

            cli::cli_alert_success(
                '[ {stringr::str_pad(domain, 8, "both")} ] imported with {nrow(data)} rows and {ncol(data)} columns.'
            )

            data
        }) %>%
        rlang::set_names(domains)

    # # Parse numeric siteid from siteid
    # datasets$site <- datasets$site %>%
    #   mutate(
    #     siteid = as.character(readr::parse_number(siteid))
    #   )

    datasets
}

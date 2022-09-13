#' Import [ rawplus ] data
#'
#' @param data_path `character` path to data files
#'
#' @importFrom arrow read_parquet
#' @importFrom cli cli_alert_success
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr imap
#' @importFrom rlang set_names
#' @importFrom stringr str_pad word

rawplus_1_import <- function(
    data_path = file.path('data-raw', 'rawplus')
) {
    stopifnot(
        '[ data_path ] does not exist.' = file.exists(data_path)
    )

    # Retrieve list of data files.
    data_files <- data_path %>%
        list.files(
            '\\.parquet$',
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

            data <- arrow::read_parquet(data_file,)

            cli::cli_alert_success(
                '[ {stringr::str_pad(domain, 8, "both")} ] imported with {nrow(data)} rows and {ncol(data)} columns.'
            )

            data
        }) %>%
        rlang::set_names(domains)

    datasets
}

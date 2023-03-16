#' @importFrom cli cli_alert_danger
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr str_split_1
check_columns <- function(path) {
    files <- list.files(
        path,
        '\\.csv$',
        full.names = TRUE,
        recursive = TRUE
    )

    gismo_schema <- gsm::rbm_data_spec %>%
        filter(
            System == 'Gismo'
        )

    walk(files, function(file) {
        domain <- basename(file) %>%
            sub('\\.csv$', '', .)

        schema <- gismo_schema %>%
            filter(
                Table == domain
            )
        
        columns <- readLines(file, 1) %>%
            str_split_1(',')
        
        if (!all(sort(schema$Column) == sort(columns))) {
            cli::cli_alert_danger(glue::glue(
                'Column mismatch detected in {file}.'
            ))
        }
    })
}

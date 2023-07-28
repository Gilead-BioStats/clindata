#' Check Columns
#'
#' @description
#' Checks that columns map to the data specification in `{gsm}`.
#'
#' @param path `directory path` Path to where `.csv` files are stored.
#'
#' @importFrom cli cli_alert_danger
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr str_split_1
#'
#' @keywords internal
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


#' Check Mapping
#'
#' @description
#' Checks that columns map to the data specification in `{gsm}`.
#'
#' @importFrom cli cli_alert_danger
#'
#' @param domain `character` Data domain.
#' @param data `data.frame` Data.
#'
#' @keywords internal
check_mapping <- function(domain, data) {
    mapping <- gsm::input_data_schema %>%
        filter(
            .data$`{clindata} Name` == !!domain
        )

    missing_columns <- setdiff(mapping$Column, names(data))

    if (length(missing_columns)) {
        cli::cli_alert_danger(
            '{length(missing_columns)} column{ifelse(length(missing_columns) == 1, "", "s")} not found in [ {domain} ]: {missing_columns}.'
        )
    }
}

#' Check Rows
#'
#' @param df1 `data.frame` Data domain.
#' @param df2 `data.frame` Data domain.
#' @param domain `character` Data domain.
#'
#' @keywords internal
check_rows <- function(df1, df2, domain = NULL) {
    n_rows <- nrow(df1) - nrow(df2)

    if (is.null(domain)) {
        cli::cli_alert_success(
            "{n_rows} rows removed."
        )
    } else {
        cli::cli_alert_success(
            "{n_rows} rows removed from [ {domain} ]."
        )
    }
}

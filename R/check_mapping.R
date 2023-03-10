check_mapping <- function(domain, data) {
    mapping <- gsm::input_data_schema %>%
        filter(
            .data$Name == !!domain
        )

    missing_columns <- setdiff(mapping$Column, names(data))

    if (length(missing_columns)) {
        cli::cli_alert_danger(
            '{length(missing_columns)} columns not found in [ {domain} ]: {missing_columns}.'
        )
    }
}

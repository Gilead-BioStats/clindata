#' Document EDC data
#'
#' @param datasets_processed `list` named list of processed data frames
#' @param domain_metadata_path `character` path to domain metadata file
#' @param documentation_path `character` path to documentation file
#'
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom yaml read_yaml

edc_4_document <- function(
    datasets_processed,
    domain_metadata_path = system.file('data-standards', 'edc.yaml', package = 'clindata'),
    documentation_path = paste0(
        system.file('R', package = 'clindata'),
        '/edc.R'
    )
) {
    stopifnot(
        '[ domain_metadata_path ] does not exist.' = file.exists(domain_metadata_path)
    )

    domain_metadata <- yaml::read_yaml(domain_metadata_path)

    # Parse domain name.
    domains <- names(datasets_processed)

    purrr::map_chr(domains, function(domain) {
        edc_domain <- paste0('edc_', domain)

        # Retrieve data.
        data <- datasets_processed[[ domain ]]

        # Description of data domain
        description <- domain_metadata[[ domain ]] # description of data domain

        # Dimensions of data
        dimensions <- dim(data)

        documentation <- c(
            glue::glue('@title {description$title}'),
            glue::glue('@description {description$description} ({domain})'),
            glue::glue('@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns'),
            glue::glue('@source ./data-raw/edc/{domain}.csv')
        )

        paste0(
            "#' ",
            paste(
                documentation,
                collapse = "\n#' "
            ),
            '\n"',
            edc_domain,
            '"'
        )
    }) %>%
    paste(collapse = '\n\n') %>%
    writeLines(documentation_path)

    cli::cli_alert_success(
        'EDC documentation successfully saved to [ {documentation_path} ].'
    )
}

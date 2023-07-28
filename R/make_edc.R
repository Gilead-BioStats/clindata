#' Import EDC data
#'
#' @param data_path `character` path to data files
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr map_chr imap
#' @importFrom rlang set_names
#' @importFrom stringr str_pad word
#'
#' @keywords internal

edc_1_import <- function(
        data_path = file.path("data-raw", "edc")
) {
    stopifnot(
        "[ data_path ] does not exist." = file.exists(data_path)
    )

    # Retrieve list of data files.
    data_files <- data_path %>%
        list.files(
            "\\.Rds$",
            full.names = TRUE
        )

    # Parse domain name.
    domains <- data_files %>%
        purrr::map_chr(
            ~ stringr::word(.x, -2, sep = "/|\\.")
        )

    # Ingest data files.
    datasets <- as.list(data_files) %>%
        purrr::imap(function(data_file, index) {
            domain <- domains[index]

            data <- readRDS(data_file)

            cli::cli_alert_success(
                '[ {stringr::str_pad(domain, 8, "both")} ] imported with {nrow(data)} rows and {ncol(data)} columns.'
            )

            data
        }) %>%
        rlang::set_names(domains)

    datasets
}


#' Process EDC data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom purrr imap
#'
#' @keywords internal

edc_2_process <- function(
        datasets
) {
    datasets_processed <- datasets

    datasets_processed
}


#' Export EDC data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr iwalk
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data
#'
#' @keywords internal

edc_3_export <- function(
        datasets_processed
) {

    datasets_processed %>%
        purrr::iwalk(function(data, domain) {
            edc_domain <- paste0("edc_", domain)

            check_mapping(edc_domain, data)

            assign(edc_domain, tibble::as_tibble(data))

            do.call(
                "use_data",
                list(
                    as.name(edc_domain),
                    overwrite = TRUE
                )
            )

            cli::cli_alert_success(
                '[ {stringr::str_pad(edc_domain, 16, "both")} ] exported.'
            )
        })
}


#' Document EDC data
#'
#' @param datasets_processed `list` named list of processed data frames
#' @param domain_metadata_path `character` path to domain metadata file
#' @param documentation_path `character` path to documentation file
#'
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom yaml read_yaml
#'
#' @keywords internal

edc_4_document <- function(
        datasets_processed,
        domain_metadata_path = system.file("data-standards", "edc.yaml", package = "clindata"),
        documentation_path = paste0(
            system.file("R", package = "clindata"),
            "/edc.R"
        )
) {
    stopifnot(
        "[ domain_metadata_path ] does not exist." = file.exists(domain_metadata_path)
    )

    domain_metadata <- yaml::read_yaml(domain_metadata_path)

    # Parse domain name.
    domains <- names(datasets_processed)

    purrr::map_chr(domains, function(domain) {
        edc_domain <- paste0("edc_", domain)

        # Retrieve data.
        data <- datasets_processed[[domain]]

        # Description of data domain
        description <- domain_metadata[[domain]] # description of data domain

        # Dimensions of data
        dimensions <- dim(data)

        documentation <- c(
            glue::glue("@title {description$title}"),
            glue::glue("@description {description$description} ({domain})"),
            glue::glue("@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns"),
            glue::glue("@source ./data-raw/edc/{domain}.csv")
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
        paste(collapse = "\n\n") %>%
        writeLines(documentation_path)

    cli::cli_alert_success(
        "EDC documentation successfully saved to [ {documentation_path} ]."
    )
}

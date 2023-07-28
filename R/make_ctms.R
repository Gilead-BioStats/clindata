#' Import CTMS data
#'
#' @param data_path `character` path to data files
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr map_chr imap
#' @importFrom rlang set_names
#' @importFrom stringr str_pad word
#'
#' @keywords internal
ctms_1_import <- function(
        data_path = file.path("data-raw", "ctms")
) {
    stopifnot(
        "[ data_path ] does not exist." = file.exists(data_path)
    )

    # Retrieve list of data files.
    data_files <- data_path %>%
        list.files(
            "\\.csv$",
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

            data <- read.csv(data_file)

            cli::cli_alert_success(
                '[ {stringr::str_pad(domain, 8, "both")} ] imported with {nrow(data)} rows and {ncol(data)} columns.'
            )

            data
        }) %>%
        rlang::set_names(domains)

    datasets
}

#' Process CTMS data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom purrr imap pluck
#' @importFrom utils read.csv
#'
#' @keywords internal

ctms_2_process <- function(datasets) {

    datasets_processed <- datasets

    # Use download function from rbmPipe to access default rbmLibrary mappings
    mapping <- 'default/mapping_column.csv' %>%
        rbmPipe::download_file_from_github(
            'rbmLibrary',
            tempdir()
        ) %>%
        purrr::pluck('destination') %>%
        utils::read.csv()

    # Define the mapping for gsm_domain_key to clindata_key
    keyMapping <- c("dfPD" = "protdev",
                    "dfSITE" = "site",
                    "dfSTUDY" = "study")

    # Create a new clindata_key column based on gsm_domain_key
    mapping$clindata_key <- keyMapping[mapping$gsm_domain_key]

    # Enforce data types from mapping_column.csv where relevant
    for (dfName in names(datasets_processed)) {
        df <- datasets_processed[[dfName]]

        for (col in colnames(df)) {
            expectedFormat <- mapping[mapping$clindata_key == dfName & mapping$default == col, "format"][1]
            colFormat <- tolower(class(df[[col]]))

            if (!is.na(expectedFormat) && any(!colFormat %in% expectedFormat)) {
                convertedValue <- NULL

                if (expectedFormat[1] %in% c("integer", "numeric", "character")) {
                    convertedValue <- do.call(as, c(list(df[[col]]), expectedFormat[1]))
                } else if (expectedFormat[1] == "date") {
                    convertedValue <- as.Date(df[[col]], format = "%Y-%m-%d")
                }

                df[[col]] <- convertedValue
            }
        }

        datasets_processed[[dfName]] <- df
    }

    datasets_processed
}

#' Export CTMS data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr iwalk
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data
#'
#' @keywords internal

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

#' Document CTMS data
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

ctms_4_document <- function(
        datasets_processed,
        domain_metadata_path = system.file("data-standards", "ctms.yaml", package = "clindata"),
        documentation_path = paste0(
            system.file("R", package = "clindata"),
            "/ctms.R"
        ),
        data_domain = "ctms"
) {
    stopifnot(
        "[ domain_metadata_path ] does not exist." = file.exists(domain_metadata_path)
    )

    domain_metadata <- yaml::read_yaml(domain_metadata_path)

    # Parse domain name.
    domains <- names(datasets_processed)

    purrr::map_chr(domains, function(domain) {
        ctms_domain <- paste0(data_domain, "_", domain)

        # Retrieve data.
        data <- datasets_processed[[domain]]

        # Description of data domain
        description <- domain_metadata[[domain]] # description of data domain

        # Dimensions of data
        dimensions <- dim(data)

        documentation <- c(
            glue::glue("@title {description}"),
            glue::glue("@description {description} ({domain}) data CTMS data"),
            glue::glue("@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns"),
            glue::glue("@source ./data-raw/{data_domain}/{domain}.csv")
        )

        paste0(
            "#' ",
            paste(
                documentation,
                collapse = "\n#' "
            ),
            '\n"',
            ctms_domain,
            '"'
        )
    }) %>%
        paste(collapse = "\n\n") %>%
        writeLines(documentation_path)

    cli::cli_alert_success(
        "CTMS documentation successfully saved to [ {documentation_path} ]."
    )
}


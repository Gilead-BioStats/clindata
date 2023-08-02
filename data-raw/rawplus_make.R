#' Import [ rawplus ] data
#'
#' @param data_path `character` path to data files
#'
#' @importFrom arrow read_parquet
#' @importFrom cli cli_alert_success
#' @importFrom purrr map_chr imap
#' @importFrom rlang set_names
#' @importFrom stringr str_pad word
#'
#' @keywords internal

rawplus_1_import <- function(
        data_path = file.path("data-raw", "rawplus")
) {
    stopifnot(
        "[ data_path ] does not exist." = file.exists(data_path)
    )

    # Retrieve list of data files.
    data_files <- data_path %>%
        list.files(
            "\\.parquet$",
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

            data <- arrow::read_parquet(data_file)

            cli::cli_alert_success(
                '[ {stringr::str_pad(domain, 8, "both")} ] imported with {nrow(data)} rows and {ncol(data)} columns.'
            )

            data
        }) %>%
        rlang::set_names(domains)

    datasets
}

#' Process [ dm ] domain
#'
#' @param dm `data.frame` [ dm ] data
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr filter
#'
#' @keywords internal

rawplus_2_process_1_dm <- function(dm) {
    dm_processed <- dm %>%
        dplyr::filter(
            .data$subjid != ""
        )

    n_rows_filtered <- nrow(dm) - nrow(dm_processed)

    if (n_rows_filtered) {
        cli::cli_alert_warning(
            "{n_rows_filtered} rows with a missing subject ID removed from [ dm ]."
        )
    }

    dm_processed
}

#' Process domain data
#'
#' Remove rows from domain data where the associated subject ID does not exist in [ dm ].
#'
#' @param data `data.frame` domain data
#' @param dm `data.frame` [ dm ] data
#' @param domain `character` domain name
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr filter
#'
#' @keywords internal

rawplus_2_process_2_domain <- function(data, dm, domain) {
    data_processed <- data %>%
        dplyr::filter(
            .data$subjid %in% dm$subjid
        )

    n_rows_removed <- nrow(data) - nrow(data_processed)

    if (n_rows_removed) {
        cli::cli_alert_warning(
            "{n_rows_removed} rows removed from [ {domain} ] for subject IDs not present in [ dm ]."
        )
    }

    data_processed
}


#' Process [ lb ] data
#'
#' Reduce the size of [ lb ] data.
#'
#' @param lb `data.frame` [ lb ] data
#' @param dm `data.frame` [ dm ] data
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr case_when filter if_else inner_join mutate select
#'
#' @keywords internal

rawplus_2_process_3_lb <- function(lb, dm) {
    cli::cli_alert_warning(
        "[ treatmentemergent ] and [ alertsimplified ] temporarily derived for [ lb ]."
    )

    lb_processed <- lb %>%
        dplyr::filter(
            battrnam %in% c(
                "HEMATOLOGY&DIFFERENTIAL PANEL",
                "CHEMISTRY PANEL"
            ),
            !is.na(siresn)
        ) %>%
        dplyr::select(
            subjid, # participant
            visnam, visnum, lb_dt, # timing
            battrnam, lbtstnam, # measure
            siresn, sinrlo, sinrhi, toxgrg_nsv, alrtfl # result
        ) %>%
        dplyr::inner_join(
            dm %>% dplyr::select(subjid, firstdosedate, lastdosedate),
            "subjid"
        ) %>%
        dplyr::mutate(
            treatmentemergent = dplyr::case_when(
                lb_dt == "" | firstdosedate == "" | lastdosedate == "" ~ "",
                as.Date(firstdosedate) <= as.Date(lb_dt) & as.Date(lb_dt) <= (as.Date(lastdosedate) + 30) ~ "Y",
                TRUE ~ ""
            ),
            alertsimplified = dplyr::case_when(
                alrtfl %in% c("H", "HN", "HT", "HP") ~ "H",
                alrtfl %in% c("L", "LN", "LT", "LP") ~ "L",
                TRUE ~ alrtfl
            )
        ) %>%
        dplyr::select(-firstdosedate, -lastdosedate)

    n_rows_removed <- nrow(lb) - nrow(lb_processed)

    if (n_rows_removed) {
        cli::cli_alert_warning(
            "{n_rows_removed} rows removed from [ lb ]."
        )
    }

    lb_processed
}

#' Process [ rawplus ] data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom purrr imap pluck
#' @importFrom utils read.csv
#'
#' @keywords internal

rawplus_2_process <- function(datasets) {

    # Remove invalid records from [ dm ].
    dm <- rawplus_2_process_1_dm(datasets$dm)

    # Subset each domain on subject IDs found in [ dm ].
    datasets_processed <- datasets %>%
        purrr::imap(function(data, domain) {
            rawplus_2_process_2_domain(data, dm, domain)
        })

    # Further process problem domains.
    datasets_processed$lb <- rawplus_2_process_3_lb(datasets_processed$lb, dm)

    # Use download function from rbmPipe to access default rbmLibrary mappings
    mapping <- 'default/mapping_column.csv' %>%
        rbmPipe::download_file_from_github(
            'rbmLibrary',
            tempdir()
        ) %>%
        purrr::pluck('destination') %>%
        utils::read.csv()

    # Define the mapping for gsm_domain_key to clindata_key
    keyMapping <- c("dfAE" = "ae",
                    "dfSUBJ" = "dm",
                    "dfLB" = "lb",
                    "dfSDRGCOMP" = "sdrgcomp",
                    "dfSTUDCOMP" = "studcomp",
                    "dfCONSENT" = "consent",
                    "dfENROLL" = "enroll",
                    "dfIE" = "ie")

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

#' Export [ rawplus ] data
#'
#' @param datasets_processed `list` named list of processed data frames
#'
#' @importFrom cli cli_alert_success
#' @importFrom purrr iwalk
#' @importFrom tibble as_tibble
#' @importFrom usethis use_data
#'
#' @keywords internal

rawplus_3_export <- function(
        datasets_processed
) {
    datasets_processed %>%
        purrr::iwalk(function(data, domain) {
            rawplus_domain <- paste0("rawplus_", domain)

            check_mapping(rawplus_domain, data)

            assign(rawplus_domain, tibble::as_tibble(data))

            do.call(
                "use_data",
                list(
                    as.name(rawplus_domain),
                    overwrite = TRUE
                )
            )

            cli::cli_alert_success(
                '[ {stringr::str_pad(rawplus_domain, 16, "both")} ] exported.'
            )
        })
}


#' Document [ rawplus ] data
#'
#' @param datasets_processed `list` named list of processed data frames
#' @param domain_metadata_path `character` path to domain metadata file
#' @param column_metadata_path `character` path to column metadata files
#' @param documentation_path `character` path to documentation file
#'
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom stringr word
#' @importFrom yaml read_yaml
#'
#' @keywords internal

rawplus_4_document <- function(
        datasets_processed,
        domain_metadata_path = system.file("data-standards", "rawplus.yaml", package = "clindata"),
        column_metadata_path = file.path("data-raw", "rawplus"),
        documentation_path = paste0(
            system.file("R", package = "clindata"),
            "/rawplus.R"
        )
) {
    stopifnot(
        "[ domain_metadata_path ] does not exist." = file.exists(domain_metadata_path),
        "[ column_metadata_path ] does not exist." = file.exists(column_metadata_path)
    )

    domain_metadata <- yaml::read_yaml(domain_metadata_path)

    # Retrieve list of data files.
    column_metadata_files <- column_metadata_path %>%
        list.files(
            "\\.json$",
            full.names = TRUE
        )

    # Parse domain name.
    domains <- column_metadata_files %>%
        purrr::map_chr(
            ~ stringr::word(.x, -3, sep = "/|_|\\.")
        )

    # Parse domain name.
    column_metadatasets <- column_metadata_files %>%
        purrr::map(~ jsonlite::read_json(.x)) %>%
        set_names(domains)

    purrr::map_chr(domains, function(domain) {
        rawplus_domain <- paste0("rawplus_", domain)

        # Retrieve data.
        data <- datasets_processed[[domain]]

        # Description of data domain
        description <- domain_metadata[[domain]] # description of data domain

        # Dimensions of data
        dimensions <- dim(data)

        # Column-level metadata
        column_metadata <- column_metadatasets[[domain]] %>%
            .[intersect(names(.), names(data))] # keep only metadata for columns in data

        # Column-level documentation
        column_documentation <- paste0(
            "  \\item{",
            names(data),
            "}{",
            purrr::map_chr(
                names(data),
                function(column) {
                    column_metadatum <- column_metadata[[column]]

                    if (is.null(column_metadatum)) {
                        type <- class(data[[column]])
                        description <- NULL
                    } else {
                        type <- column_metadatum[[1]]
                        description <- column_metadatum[[2]]
                    }

                    text <- paste("[", type, "]")

                    if (!is.null(description)) {
                        text <- paste(text, description)
                    }

                    text
                }
            ),
            "}"
        )

        documentation <- c(
            glue::glue("@title {rawplus_domain}"),
            glue::glue("@description {description} ({domain}) data in [ rawplus ] format."),
            glue::glue("@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns."),
            "\\describe{",
            column_documentation,
            "}",
            glue::glue("@source ./data-raw/rawplus/{domain}.parquet")
        )

        paste0(
            "#' ",
            paste(
                documentation,
                collapse = "\n#' "
            ),
            '\n"',
            rawplus_domain,
            '"'
        )
    }) %>%
        paste(collapse = "\n\n") %>%
        writeLines(documentation_path)

    cli::cli_alert_success(
        "[ rawplus ] documentation successfully saved to [ {documentation_path} ]."
    )
}

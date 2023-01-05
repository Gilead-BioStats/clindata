#' Document [ rawplus ] data
#'
#' @param datasets_processed `list` named list of processed data frames
#' @param domain_metadata_path `character` path to domain metadata file
#' @param column_metadata_path `character` path to column metadata files
#' @param documentation_path `character` path to documentation file
#'
#' @importFrom cli cli_alert_success
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom stringr word
#' @importFrom yaml read_yaml

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

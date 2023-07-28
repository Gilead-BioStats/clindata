#' Process [ rawplus ] data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom magrittr %>%
#' @importFrom purrr imap

rawplus_2_process <- function(
  datasets
) {
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
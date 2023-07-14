#' Process CTMS data
#'
#' @param datasets `list` named list of data frames
#'
#' @importFrom magrittr %>%
#' @importFrom purrr imap

ctms_2_process <- function(
  datasets
) {
  datasets_processed <- datasets

  # Use download function from rbmPipe to access rbmLibrary mappings
  download_directory_from_github(
      path = "default",
      repo = "rbmLibrary",
      destination = "./inst"
  )

  mapping <- read.csv(system.file("mapping_column.csv", package = "clindata"))

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
          colFormat <- class(df[[col]])

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

library(magrittr)
library(yaml)
library(purrr)
library(stringr)
library(jsonlite)
library(glue)

domains <- system.file('data-standards', 'rawplus.yaml', package = 'clindata') %>%
  yaml::read_yaml()

system.file('data-raw', 'rawplus', package = 'clindata') %>% # path to ./data-raw
  list.files(
    '\\.json$', # retrieve .json files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::map_chr(function(file) {
    domain <- stringr::word(file, -2, sep = '/|\\.') %>% # name of data domain
      stringr::str_replace('_metadata', '')
    print(domain)
    rawplus_domain <- glue::glue('rawplus_{domain}')

    description <- domains[[ domain ]] # description of data domain

    dimensions <- dim(
      do.call(`::`, list('clindata', rawplus_domain))
    )

    column_metadata <- jsonlite::read_json(file) # ingest .json file

    column_documentation <- paste0(
      '  \\item{',
        names(column_metadata),
      '}{',
        purrr::map_chr(
          column_metadata,
          function(column) {
            type <- column[[1]]
            description <- column[[2]]
            text <- paste('[', type, ']')
            if (!is.null(description))
              text = paste(text, description)
            text
          }
        ),
      '}'
    )

    documentation <- c(
      glue::glue('@title {rawplus_domain}'),
      glue::glue('@description {description} ({domain}) data in Raw+ format.'),
      glue::glue('@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns.'),
      '\\describe{',
        column_documentation,
      '}',
      glue::glue('@source {str_replace(file, "_metadata.json", ".parquet")}')
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
  paste(collapse = '\n\n') %>%
  writeLines(
    paste0(system.file('R', package = 'clindata'), '/rawplus.R')
  )

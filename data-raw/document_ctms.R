library(magrittr)
library(yaml)
library(purrr)
library(stringr)
library(jsonlite)
library(glue)

system.file('data-standards', 'ctms.yaml', package = 'clindata') %>%
  yaml::read_yaml() %>%
  purrr::imap(function(description,domain){
    ctms_domain <- glue::glue('ctms_{domain}')

    dimensions <- dim(
      do.call(`::`, list('clindata', ctms_domain))
    )

    documentation <- c(
      glue::glue('@title {description}'),
      glue::glue('@description {description} ({domain}) CTMS data.'),
      glue::glue('@format a data frame with {dimensions[1]} rows and {dimensions[2]} columns.')
    )

    return(
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
    )
  })%>%
  paste(collapse = '\n\n') %>%
  writeLines('./R/ctms.R')

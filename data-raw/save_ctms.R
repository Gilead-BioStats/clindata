library(magrittr)
library(purrr)
library(stringr)
library(usethis)


list.files(
    './data-raw/ctms', 
    '\\.csv$', # retrieve .csv files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::walk(function(file) {
    domain <- paste0(
      'ctms_',
      stringr::word(file, -2, sep = '/|\\.') # name of data domain
    )

    
    data <-read.csv(file)

    assign(domain, data) # define local variable

    do.call(
      'use_data',
      list(
        as.name(domain),
        overwrite = TRUE
      )
    )
  })

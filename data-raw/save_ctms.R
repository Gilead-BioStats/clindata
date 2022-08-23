library(magrittr)
library(purrr)
library(stringr)
library(usethis)

system.file('data-raw', 'ctms', package = 'clindata') %>% # path to ./data-raw
  list.files(
    '\\.csv$', # retrieve .csv files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::walk(function(file) {
    domain <- paste0(
      'ctms_',
      stringr::word(file, -2, sep = '/|\\.') # name of data domain
    )
    print(domain)

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

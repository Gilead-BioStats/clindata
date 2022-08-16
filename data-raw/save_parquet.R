library(magrittr)
library(purrr)
library(stringr)
library(arrow)
library(usethis)

dm <- system.file('data-raw', 'rawplus', 'dm.parquet', package = 'clindata') %>%
  arrow::read_parquet()

system.file('data-raw', 'rawplus', package = 'clindata') %>% # path to ./data-raw
  list.files(
    '\\.parquet$', # retrieve .parquet files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::walk(function(file) {
    domain <- stringr::word(file, -2, sep = '/|\\.') # name of data domain
    data <- arrow::read_parquet(file) # ingest .parquet file
    if (domain == 'protdev')
        data = clean_protdev(data, dm)
    assign(domain, data) # define local variable
    do.call(
      'use_data',
      list(
        as.name(domain),
        overwrite = TRUE
      )
    )
  })

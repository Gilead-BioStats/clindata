library(magrittr)
library(purrr)
library(stringr)
library(arrow)
library(usethis)
library(dplyr)

source('data-raw/rawplus/modify_lb.R')

dm <- arrow::read_parquet(
  system.file('data-raw', 'rawplus', 'dm.parquet', package = 'clindata')
)

system.file('data-raw', 'rawplus', package = 'clindata') %>% # path to ./data-raw
  list.files(
    '\\.parquet$', # retrieve .parquet files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::walk(function(file) {
    domain <- paste0(
      'rawplus_',
      stringr::word(file, -2, sep = '/|\\.') # name of data domain
    )
    print(domain)

    data <- arrow::read_parquet(file) # ingest .parquet file

    if (domain == "rawplus_lb") { # hard code lb differently using lb, dm rawplus data
      data <- modify_lb(data, dm)
    }

    print(dim(data))
    assign(domain, data) # define local variable

    do.call(
      'use_data',
      list(
        as.name(domain),
        overwrite = TRUE
      )
    )
  })

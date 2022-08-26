library(magrittr)
library(purrr)
library(stringr)
library(arrow)
library(usethis)
library(dplyr)

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

    if (domain == "rawplus_lb"){  # hard code lb differently using lb, dm rawplus data
      rawplus_lb <- arrow::read_parquet(file)
      rawplus_dm <- arrow::read_parquet(list.files(system.file('data-raw', 'rawplus', package = 'clindata'),
                                                'dm.parquet$', # retrieve .parquet files in ./data-raw
                                                full.names = TRUE))
      data_te <- rawplus_dm  %>%
        select(subjid, siteid, rfxst_dt, rfxen_dt)  %>%
        filter(subjid!="")
      data_lb <- rawplus_lb %>%
        left_join(data_te, by = "subjid") %>%
        mutate(
          lb_te = ifelse(lb_dt>=as.Date(rfxst_dt)&lb_dt<=as.Date(rfxen_dt)+30, "YES", "")
        )
      print(dim(data_lb))
      assign(domain, data_lb)
    } else {
      data <- arrow::read_parquet(file) # ingest .parquet file
      print(dim(data))
      assign(domain, data) # define local variable
    }

    do.call(
      'use_data',
      list(
        as.name(domain),
        overwrite = TRUE
      )
    )
  })


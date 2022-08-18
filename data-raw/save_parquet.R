library(magrittr)
library(purrr)
library(stringr)
library(arrow)
library(usethis)

#dm <- system.file('data-raw', 'rawplus', 'dm.parquet', package = 'clindata') %>%
#  arrow::read_parquet()

# TODO: replace with actual rawplus export
lb <- clindata::rawplus_lb %>%
  select(
    siteid = SiteID,
    subjid = SubjectID,
    VISNAM = VISIT,
    VISNUM = VISITNUM,
    LB_DT = LBDTN,
    BATTRNAM = LBCAT,
    LBTSTNAM = LBTEST,
    LBTSTCD = LBTESTCD,
    SIRESN = LBSTRESN,
    SINRLO = LBSTNRLO,
    SINRHI = LBSTNRHI,
    LB_TE = LB_TE_FLAG,
    TOXGR = LB_GRADE
  )

usethis::use_data(lb)

system.file('data-raw', 'rawplus', package = 'clindata') %>% # path to ./data-raw
  list.files(
    '\\.parquet$', # retrieve .parquet files in ./data-raw
    full.names = TRUE
  ) %>%
  purrr::walk(function(file) {
    domain <- stringr::word(file, -2, sep = '/|\\.') # name of data domain
    data <- arrow::read_parquet(file) # ingest .parquet file

    # TODO: avoid cleaning protocol deviations
    #
    # Merge dm.subjectid onto protdev
    #if (domain == 'protdev')
    #    data = clean_protdev(data, dm)

    assign(domain, data) # define local variable

    do.call(
      'use_data',
      list(
        as.name(domain),
        overwrite = TRUE
      )
    )
  })

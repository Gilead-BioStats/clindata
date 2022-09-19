devtools::load_all()
datasets <- rawplus_1_import()

# join country data from ctms
# default to 'US' when country is missing
ctms <- ctms_1_import()
datasets$dm <- datasets$dm %>%
  full_join(
    ctms$site %>% select(siteid, country), by = "siteid"
  ) %>%
  mutate(
    country = ifelse(is.na(country), 'US', country),
    timeonstudy = dplyr::coalesce(timeonstudy, 0),
    timeontreatment = dplyr::coalesce(timeontreatment, 0)
  )
datasets_processed <- rawplus_2_process(datasets)
rawplus_3_export(datasets_processed)
rawplus_4_document(datasets_processed)

# temporary fix for [ ie ]
load('data-raw/rawplus/rawplus_ie.rda')
rawplus_ie <- tibble::as_tibble(rawplus_ie)
names(rawplus_ie) <- c('subjid', 'iecat', 'ieorres', 'tiver')
usethis::use_data(rawplus_ie, overwrite = TRUE)

# temporary fix for [ consent ]
load('data-raw/rawplus/rawplus_consent.rda')
names(rawplus_consent) <- c('subjid', 'consdt', 'conscat', 'consyn')
usethis::use_data(rawplus_consent, overwrite = TRUE)

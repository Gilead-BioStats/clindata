devtools::load_all()
datasets <- rawplus_1_import()
datasets$dm$timeonstudy <- dplyr::coalesce(datasets$dm$timeonstudy, 0)
datasets$dm$timeontreatment <- dplyr::coalesce(datasets$dm$timeontreatment, 0)
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

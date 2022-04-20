here::i_am("data-raw/rawplus/makeRawPlus.R")
library(here)
library(yaml)

# Make subj
rawplus_subj_s <- CreateSUBJ(
    dfDm = clindata::raw_dm,
    dfIXRSrand = clindata::raw_iwrsrand,
    dfEx = clindata::raw_ex,
    dfVisit = clindata::raw_visdt,
    dfStud = clindata::raw_studcomp,
    dfSdrg = clindata::raw_sdrgcomp
)

rawplus_subj <- rawplus_subj_s %>%
    filter( RandFlag=="Y") %>%
    filter( !is.na(TimeOnTreatment))

usethis::use_data(rawplus_subj_s, overwrite=TRUE)
usethis::use_data(rawplus_subj, overwrite=TRUE)

# Domain Mappings
ids<- rawplus_subj %>% pull(SubjectID)
rawplus_ae <- map_rawplus_ae(clindata::raw_ae, ids)
rawplus_pd <- map_rawplus_pd(clindata::raw_protdev, ids)
rawplus_ie <- map_rawplus_ie(clindata::raw_ie_all, ids)
rawplus_consent <- map_rawplus_consent(clindata::raw_ic_elig, ids)
rawplus_mapping <- yaml::read_yaml(here::here("inst", "mapping", "rawplus.yaml"))

usethis::use_data(rawplus_ae, overwrite=TRUE)
usethis::use_data(rawplus_pd, overwrite=TRUE)
usethis::use_data(rawplus_ie, overwrite=TRUE)
usethis::use_data(rawplus_consent, overwrite=TRUE)
usethis::use_data(rawplus_mapping, overwrite=TRUE)

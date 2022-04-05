here::i_am("data-raw/rawplus/makeRawPlus.R")
library(here)

# Make RDSL
rawplus_rdsl_s <- CreateRDSL(
    dfDm = clindata::raw_dm,
    dfIXRSrand = clindata::raw_iwrsrand,
    dfEx = clindata::raw_ex,
    dfVisit = clindata::raw_visdt,
    dfStud = clindata::raw_studcomp,
    dfSdrg = clindata::raw_sdrgcomp
)

rawplus_rdsl <- rawplus_rdsl %>% 
    filter( RandFlag=="Y") %>% 
    filter( !is.na(TimeOnTreatment))

usethis::use_data(rawplus_rdsl_s, overwrite=TRUE)
usethis::use_data(rawplus_rdsl, overwrite=TRUE)

# Domain Mappings
rawplus_ae <- map_rawplus_ae(clindata::raw_ae)
rawplus_pd <- map_rawplus_pd(clindata::raw_protdev)
rawplus_ie <- map_rawplus_ie(clindata::raw_ie_all)
rawplus_consent <- map_rawplus_consent(clindata::raw_ic_elig)

usethis::use_data(rawplus_ae, overwrite=TRUE)
usethis::use_data(rawplus_pd, overwrite=TRUE)
usethis::use_data(rawplus_ie, overwrite=TRUE)
usethis::use_data(rawplus_consent, overwrite=TRUE)
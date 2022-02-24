here::i_am("data-raw/rawplus/makeRawPlus.R")
library(here)

rawplus_rdsl_s <- CreateRDSL(dfDm = clindata::raw_dm,
                           dfIXRSrand = clindata::raw_iwrsrand,
                           dfEx = clindata::raw_ex,
                           dfVisit = clindata::raw_visdt,
                           dfStud = clindata::raw_studcomp,
                           dfSdrg = clindata::raw_sdrgcomp)

rawplus_rdsl <- rawplus_rdsl %>% filter( RandFlag=="Y")

usethis::use_data(rawplus_rdsl_s, overwrite=TRUE)
usethis::use_data(rawplus_rdsl, overwrite=TRUE)

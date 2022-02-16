here::i_am("data-raw/rawplus/makeRawPlus.R")
library(here)

rawplus_rdsl <- CreateRDSL(dfDm = clindata::raw_dm,
                           dfIXRSrand = clindata::raw_iwrsrand,
                           dfEx = clindata::raw_ex,
                           dfVisit = clindata::raw_visdt,
                           dfStud = clindata::raw_studcomp,
                           dfSdrg = clindata::raw_sdrgcomp)

usethis::use_data(rawplus_rdsl, overwrite=TRUE)

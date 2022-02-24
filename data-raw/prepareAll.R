library(here)
here::i_am("data-raw/prepareAll.R")

#Raw
source(here("data-raw","raw","_prepareData.R"))
source(here("data-raw", "raw", "prepareIE.R"))
source(here("data-raw","raw","_prepareRoxygen.R"))

#RawPlus
source(here("data-raw","rawplus","makeRawPlus.R"))
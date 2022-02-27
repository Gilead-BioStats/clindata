library(usethis)
library(here)

here::i_am("data-raw/raw/prepareLabsSubset.R.R")

#library(haven)
#covlab <- read_sas("covlab.sas7bdat")


sort(table(covlab$BATTRNAM))


raw_covlab_hema <- covlab[covlab$BATTRNAM == "HEMATOLOGY&DIFFERENTIAL PANEL", ]
table(raw_covlab_hema$LBTEST)
table(raw_covlab_hema$TOXGRG)

usethis::use_data(raw_covlab_hema, overwrite=TRUE)

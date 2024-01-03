# `dev` at commit: 79becf6
# Pre v1.9.0 release
devtools::install_github("Gilead-BioStats/gsm@79becf6")

source("https://raw.githubusercontent.com/Gilead-BioStats/clindata/main/inst/snapshot/snapshot.R")
source("https://raw.githubusercontent.com/Gilead-BioStats/clindata/main/inst/simulation/utils-helpers.R")
source("https://raw.githubusercontent.com/Gilead-BioStats/clindata/be78fd0644bdd32ded9e9cfd2d30f9f963c64fba/inst/utils-check.R")
library(lubridate)
devtools::load_all()


s1_data <- snapshot_all(snapshot_date = "2010-01-01")
s2_data <- snapshot_all(snapshot_date = "2012-01-01")
s3_data <- snapshot_all(snapshot_date = "2014-01-01")
s4_data <- snapshot_all(snapshot_date = "2016-01-01")
s5_data <- snapshot_all(snapshot_date = "2018-01-01")


snap1 <- gsm::Make_Snapshot(lData = s1_data, strAnalysisDate = "2010-01-01")
snap2 <- gsm::Make_Snapshot(lData = s2_data, strAnalysisDate = "2012-01-01", lPrevSnapshot = snap1)
snap3 <- gsm::Make_Snapshot(lData = s3_data, strAnalysisDate = "2014-01-01", lPrevSnapshot = snap2)
snap4 <- gsm::Make_Snapshot(lData = s4_data, strAnalysisDate = "2016-01-01", lPrevSnapshot = snap3)
snap5 <- gsm::Make_Snapshot(lData = s5_data, strAnalysisDate = "2018-01-01", lPrevSnapshot = snap4)

# gsm::Save_Snapshot(snap5, here::here("inst", "data-longitudinal"))

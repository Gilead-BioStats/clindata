library(dplyr)
library(lubridate)
#load_all('.')

#dm <- clindata::rawplus_dm
#sv <- clindata::rawplus_visdt
#ex <- clindata::rawplus_ex
#ae <- clindata::rawplus_ae
#dv <- clindata::rawplus_protdev
#ie <- clindata::rawplus_ie
#dss <- clindata::rawplus_studcomp
#dst <- clindata::rawplus_sdrgcomp
#lb <- clindata::rawplus_lb
#ic <- clindata::rawplus_consent

source('R/get_snapshot_date.R')
source('R/check_rows.R')
source('R/snapshot_visdt.R')
source('R/snapshot_ex.R')
source('R/snapshot_dm.R')
source('R/snapshot_ae.R')

snapshot_date <- get_snapshot_date()

for (i in 1:100) {
  snapshot_date <- get_snapshot_date(snapshot_date)
  visdt <- snapshot_visdt(snapshot_date)
  ex <- snapshot_ex(snapshot_date)
  dm <- snapshot_dm(snapshot_date, visdt, ex)
  ae <- snapshot_ae(snapshot_date, dm)
}

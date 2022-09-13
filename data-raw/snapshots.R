library(dplyr)
library(lubridate)
#load_all('.')

source('R/get_snapshot_date.R')
source('R/check_rows.R')
source('R/snapshot_visdt.R')
source('R/snapshot_ex.R')
source('R/snapshot_dm.R')
source('R/snapshot_consent.R')
source('R/snapshot_ie.R')
source('R/snapshot_studcomp.R')
source('R/snapshot_sdrgcomp.R')
source('R/snapshot_ae.R')
source('R/snapshot_lb.R')
source('R/snapshot_protdev.R')

snapshot_date <- get_snapshot_date()

for (i in 1:3) {
  snapshot_date <- get_snapshot_date(snapshot_date, n_intervals = 12)

  visdt <- snapshot_visdt(snapshot_date)
  ex <- snapshot_ex(snapshot_date)
  dm <- snapshot_dm(snapshot_date, visdt, ex)
  consent <- snapshot_consent(snapshot_date, dm)
  ie <- snapshot_ie(snapshot_date, dm)
  studcomp <- snapshot_studcomp(snapshot_date, dm)
  sdrgcomp <- snapshot_sdrgcomp(snapshot_date, dm)
  ae <- snapshot_ae(snapshot_date, dm)
  lb <- snapshot_lb(snapshot_date, dm)
  protdev <- snapshot_protdev(snapshot_date, dm)
}

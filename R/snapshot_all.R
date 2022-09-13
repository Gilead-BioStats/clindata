snapshot_all <- function(
  n_snapshots = 1,
  n_intervals = 1
) {
  snapshot_date <- get_snapshot_date()

  for (i in 1:n_snapshots) {
    snapshot_date <- get_snapshot_date(snapshot_date, n_intervals = n_intervals)

    visdt <- snapshot_visdt(snapshot_date)
    ex <- snapshot_ex(snapshot_date)
    dm <- snapshot_dm(snapshot_date, visdt, ex)
    consent <- snapshot_consent(snapshot_date, dm)
    ie <- snapshot_ie(snapshot_date, dm)
    studcomp <- snapshot_studcomp(snapshot_date, dm)
    sdrgcomp <- snapshot_sdrgcomp(snapshot_date, dm)
    lb <- snapshot_lb(snapshot_date, dm)
    ae <- snapshot_ae(snapshot_date, dm)
    protdev <- snapshot_protdev(snapshot_date, dm)
  }

  return(
    list(
      dfSUBJ = dm,
      dfCONSENT = consent,
      dfIE = ie,
      dfSTUDCOMP = studcomp,
      dfSDRGCOMP = sdrgcomp,
      dfLB = lb,
      dfAE = ae,
      dfPD = protdev
    )
  )
}

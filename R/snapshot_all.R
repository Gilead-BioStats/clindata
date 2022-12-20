snapshot_all <- function(
  snapshot_date = get_snapshot_date()
) {
  # rawplus
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
  enroll <- snapshot_enroll(snapshot_date)

  # edc
  queries <- snapshot_queries(snapshot_date)
  data_entry_lag <- snapshot_data_entry_lag(snapshot_date)
  data_change_rate <- snapshot_data_change_rate(snapshot_date)


  return(
    list(
      snapshot_date = snapshot_date,

      # rawplus
      dfSUBJ = dm,
      dfCONSENT = consent,
      dfIE = ie,
      dfSTUDCOMP = studcomp,
      dfSDRGCOMP = sdrgcomp,
      dfLB = lb,
      dfAE = ae,
      dfPD = protdev,
      dfENROLL = enroll,

      # edc
      dfQUERY = queries,
      dfDATAENT = data_entry_lag,
      dfDATACHG = data_change_rate
    )
  )
}

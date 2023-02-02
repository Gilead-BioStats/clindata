snapshot_all <- function(
  snapshot_date = get_snapshot_date(),
  data = list(
    # rawplus
    visdt = clindata::rawplus_visdt,
    ex = clindata::rawplus_ex,
    dm = clindata::rawplus_dm,
    consent = clindata::rawplus_consent,
    ie = clindata::rawplus_ie,
    studcomp = clindata::rawplus_studcomp,
    sdrgcomp = clindata::rawplus_sdrgcomp,
    lb = clindata::rawplus_lb,
    ae = clindata::rawplus_ae,
    protdev = clindata::rawplus_protdev,
    enroll = clindata::rawplus_enroll,

    # edc
    queries = clindata::edc_queries,
    data_entry_lag = clindata::edc_data_entry_lag,
    data_change_rate = clindata::edc_data_change_rate
  )
) {
  # rawplus
  visdt <- snapshot_visdt(snapshot_date, data$visdt)
  ex <- snapshot_ex(snapshot_date, data$ex)
  dm <- snapshot_dm(snapshot_date, visdt, ex, data$dm)
  consent <- snapshot_consent(snapshot_date, dm, data$consent)
  ie <- snapshot_ie(snapshot_date, dm, data$ie)
  studcomp <- snapshot_studcomp(snapshot_date, dm, data$studcomp)
  sdrgcomp <- snapshot_sdrgcomp(snapshot_date, dm, data$sdrgcomp)
  lb <- snapshot_lb(snapshot_date, dm, data$lb)
  ae <- snapshot_ae(snapshot_date, dm, data$ae)
  protdev <- snapshot_protdev(snapshot_date, dm, data$protdev)
  enroll <- snapshot_enroll(snapshot_date, data$enroll)

  # edc
  queries <- snapshot_queries(snapshot_date, data$queries)
  data_entry_lag <- snapshot_data_entry_lag(snapshot_date, data$data_entry_lag)
  data_change_rate <- snapshot_data_change_rate(snapshot_date, data$data_change_rate)

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

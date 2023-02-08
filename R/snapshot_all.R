#' Snapshot all data domains
#'
#' Snapshot all data domains at a specific point in time.
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param data `list` Name list of data domains to snapshot
#' @param impute_rf_dt `logical` Imput reference dates with [ visdt ] and [ ex ]?
#'
#' @return `list` Named list of modified data domains
#'
#' @export
# TODO: add consent and IE back in
snapshot_all <- function(
  snapshot_date = get_snapshot_date(),
  data = list(
    # rawplus
    visdt = clindata::rawplus_visdt,
    ex = clindata::rawplus_ex,
    dm = clindata::rawplus_dm,
    # consent = clindata::rawplus_consent,
    # ie = clindata::rawplus_ie,
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
  ),
  impute_rf_dt = TRUE,
  print_check_rows = TRUE,
  rename_gsm = TRUE
) {
  # rawplus
  if (impute_rf_dt) {
    visdt <- snapshot_visdt(snapshot_date, data$visdt, print_check_rows = print_check_rows)
    ex <- snapshot_ex(snapshot_date, data$ex, print_check_rows = print_check_rows)
    dm <- snapshot_dm(snapshot_date, visdt, ex, data$dm, print_check_rows = print_check_rows)
  } else {
    dm <- snapshot_dm(snapshot_date, dm = data$dm, print_check_rows = print_check_rows)
  }

  # consent <- snapshot_consent(snapshot_date, dm, data$consent, print_check_rows = print_check_rows)
  # ie <- snapshot_ie(snapshot_date, dm, data$ie, print_check_rows = print_check_rows)
  studcomp <- snapshot_studcomp(snapshot_date, dm, data$studcomp, print_check_rows = print_check_rows)
  sdrgcomp <- snapshot_sdrgcomp(snapshot_date, dm, data$sdrgcomp, print_check_rows = print_check_rows)
  lb <- snapshot_lb(snapshot_date, dm, data$lb, print_check_rows = print_check_rows)
  ae <- snapshot_ae(snapshot_date, dm, data$ae, print_check_rows = print_check_rows)
  protdev <- snapshot_protdev(snapshot_date, dm, data$protdev, print_check_rows = print_check_rows)
  enroll <- snapshot_enroll(snapshot_date, data$enroll, print_check_rows = print_check_rows)

  # edc
  queries <- snapshot_queries(snapshot_date, data$queries, print_check_rows = print_check_rows)
  data_entry_lag <- snapshot_data_entry_lag(snapshot_date, data$data_entry_lag, print_check_rows = print_check_rows)
  data_change_rate <- snapshot_data_change_rate(snapshot_date, data$data_change_rate, print_check_rows = print_check_rows)

  data <- list(
    snapshot_date = snapshot_date,

    # rawplus
    dm = dm,
    # consent = consent,
    # ie = ie,
    studcomp = studcomp,
    sdrgcomp = sdrgcomp,
    lb = lb,
    ae = ae,
    protdev = protdev,
    enroll = enroll,

    # edc
    queries = queries,
    data_entry_lag = data_entry_lag,
    data_change_rate = data_change_rate
  )

  if (rename_gsm) {
    names(data) <- map_gsm_domains(data)
  }

  return(data)
}

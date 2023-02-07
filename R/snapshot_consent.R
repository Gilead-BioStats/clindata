#' @importFrom dplyr filter
#'
#' @export
snapshot_consent <- function(snapshot_date, dm, consent = clindata::rawplus_consent, print_check_rows = TRUE) {
  consent_snapshot <- consent %>%
    dplyr::filter(
      subjid %in% dm$subjid
    )

  if (print_check_rows)
  check_rows(consent, consent_snapshot, "consent")

  consent_snapshot
}

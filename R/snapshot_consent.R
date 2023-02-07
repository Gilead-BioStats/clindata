#' @importFrom dplyr filter
#'
#' @export
snapshot_consent <- function(snapshot_date, dm, consent = clindata::rawplus_consent) {
  consent_snapshot <- consent %>%
    dplyr::filter(
      subjid %in% dm$subjid
    )

  check_rows(consent, consent_snapshot, "consent")

  consent_snapshot
}

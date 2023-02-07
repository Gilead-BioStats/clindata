#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_study <- function(
  snapshot_date,
  dm = clindata::rawplus_dm,
  study = clindata::ctms_study
) {
  n_enrolled <- dm %>%
    filter(
      .data$rfpst_dt <= snapshot_date
    ) %>%
    group_by(studyid) %>%
    summarize(
      enrolled_sites = length(unique(siteid)),
      enrolled_participants = n()
    ) %>%
    ungroup()

  study_snapshot <- study %>%
    inner_join(
      n_enrolled,
      c("PROTOCOL_NUMBER" = "studyid")
    )

  check_rows(study, study_snapshot, "study")

  study_snapshot
}

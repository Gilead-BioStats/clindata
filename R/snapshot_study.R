#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_study <- function(
  snapshot_date,
  dm = clindata::rawplus_dm,
  study = clindata::ctms_study,
  print_check_rows = TRUE
) {
  n_enrolled <- dm %>%
    filter(
      .data$firstparticipantdate <= snapshot_date
    ) %>%
    group_by(studyid) %>%
    summarize(
      enrolled_sites = length(unique(siteid)),
      enrolled_participants = n()
    ) %>%
    ungroup()

  study_snapshot <- study %>%
    select(-any_of(c('enrolled_sites', 'enrolled_participants'))) %>%
    inner_join(
      n_enrolled,
      c("protocol_number" = "studyid")
    )

  if (print_check_rows) {
    check_rows(study, study_snapshot, "study")
  }

  study_snapshot
}

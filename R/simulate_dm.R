#' @import dplyr
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom tidyr uncount
#'
#' @export
simulate_dm <- function(
  site,
  n_subjects,
  start_date,
  end_date,
  dm = clindata::rawplus_dm
) {
  dm1 <- dm %>%
    select(
      subjid
    )

  if (!is.null(n_subjects)) {
    dm2 <- dm1 %>%
      slice_sample(
        n = n_subjects,
        replace = TRUE
      ) %>%
      group_by(subjid) %>%
      mutate(
        subjid = glue::glue("{.data$subjid}-{row_number()}") %>%
          as.character()
      ) %>%
      ungroup()
  } else {
    dm2 <- dm1
  }

  dm3 <- dm2 %>%
    mutate(
      siteid = sample(site$SITE_NUM, nrow(dm2), TRUE, runif(nrow(site))),
      firstparticipantdate = sample(start_date:end_date, n(), TRUE),
      firstdosedate = .data$firstparticipantdate + sample(1:60, n(), TRUE)
    ) %>%
    left_join(
      site %>%
        select(studyid = PROTOCOL, SITE_NUM, country = COUNTRY),
      c("siteid" = "SITE_NUM")
    ) %>%
    rowwise() %>%
    mutate(
      lastparticipantdate = sample_date(.data$firstdosedate, end_date),
      lastdosedate = sample_date(.data$firstdosedate, .data$lastparticipantdate)
    ) %>%
    ungroup() %>%
    mutate(
      across(ends_with("dt"), lubridate::as_date),
      timeonstudy = as.numeric(.data$lastparticipantdate - .data$firstparticipantdate) + 1,
      timeontreatment = as.numeric(.data$lastdosedate - .data$firstdosedate) + 1
    ) %>%
    select(
      studyid, siteid, country, subjid,
      firstparticipantdate, lastparticipantdate, timeonstudy,
      firstdosedate, lastdosedate, timeontreatment
    )

  message('--> Subjects per site')
  table(dm3$siteid) %>%
      sort() %>%
      print()
  cat('\n')

  return(dm3)
}

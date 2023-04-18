#' TODO: simulate data points (currently just code from simulate_data_pages)
#'
#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_data_points <- function(
  dm,
  data_points = clindata::edc_data_points,
  data_page_rate = runif(1, .1, .4)
) {
  data_points <- dm %>%
    select(subjid, starts_with("rfp"), timeonstudy) %>%
    filter(.data$timeonstudy > 0) %>%
    mutate(
      n_data_points = ceiling(.data$timeonstudy * data_page_rate),
      n = .data$n_data_points
    ) %>%
    tidyr::uncount(
      .data$n_data_points # generate n duplicate rows
    ) %>%
    mutate(
      foldername = "visit",
      formoid = "form",
      data_entry_lag = rnbinom(n(), size = 1, mu = 3),
      min_entereddate = visit_date + data_entry_lag
    ) %>%
    rowwise() %>%
    mutate(
        visit_date = sample_date(.data$firstparticipantdate, .data$lastparticipantdate)
    ) %>%
    ungroup() %>%
    select(
      subjectname = subjid, visit_date, foldername, formoid, data_entry_lag, min_entereddate
    )

  return(data_points)
}

#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_data_pages <- function(
  dm,
  data_pages = clindata::edc_data_pages,
  data_page_rate = runif(1, .1, .4)
) {
  data_pages <- dm %>%
    select(subjid, starts_with("rfp"), timeonstudy) %>%
    filter(.data$timeonstudy > 0) %>%
    mutate(
      n_data_pages = ceiling(.data$timeonstudy * data_page_rate),
      n = .data$n_data_pages
    ) %>%
    tidyr::uncount(
      .data$n_data_pages # generate n duplicate rows
    ) %>%
    mutate(
      foldername = "visit",
      formoid = "form",
      data_entry_lag = rnbinom(n(), size = 1, mu = 3),
      min_entereddate = visitdat_date + data_entry_lag
    ) %>%
    rowwise() %>%
    mutate(
      visitdat_date = sample_date(.data$firstparticipantdate, .data$lastparticipantdate)
    ) %>%
    ungroup() %>%
    select(
      subjectname = subjid, visitdat_date, foldername, formoid, data_entry_lag, min_entereddate
    )

  return(data_pages)
}

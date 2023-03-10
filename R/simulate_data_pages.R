#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_data_pages <- function(
  dm,
  data_entry_lag = clindata::edc_data_entry_lag,
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
      form = "form",
      n_data_points = rpois(n(), 4) + 1,
      data_entry_lag_fl = sample(
        unique(data_entry_lag$data_entry_lag_fl),
        n(),
        replace = TRUE,
        prob = table(data_entry_lag$data_entry_lag_fl) / nrow(data_entry_lag)
      ),
      n_data_points_with_changes = rpois(n(), .3)
    ) %>%
    rowwise() %>%
    mutate(
      visit_dt = sample_date(.data$firstparticipantdate, .data$lastparticipantdate)
    ) %>%
    ungroup() %>%
    select(
      subjid, visit_dt, foldername, form, n_data_points, data_entry_lag_fl, n_data_points_with_changes
    )

  return(data_pages)
}

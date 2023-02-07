#' @import dplyr
#' @importFrom lubridate as_date
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
    filter(timeonstudy > 0) %>%
    mutate(
      n_data_pages = floor(timeonstudy * data_page_rate),
      n = n_data_pages
    ) %>%
    tidyr::uncount(
      n_data_pages # generate n duplicate rows
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
      visit_dt = sample(rfpst_dt:rfpen_dt, 1) %>%
        lubridate::as_date()
    ) %>%
    ungroup() %>%
    select(
      subjid, visit_dt, foldername, form, n_data_points, data_entry_lag_fl, n_data_points_with_changes
    )

  return(data_pages)
}

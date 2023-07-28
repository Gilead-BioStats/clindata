#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_lb <- function(
  dm,
  lb = clindata::rawplus_lb,
  lb_rate = runif(1, .5, 1.1)
) {
  lb1 <- dm %>%
    select(subjid, starts_with("rfx"), timeontreatment) %>%
    filter(timeontreatment > 0) %>%
    mutate(
      n_results = ceiling(timeontreatment * lb_rate),
      n = n_results
    ) %>%
    tidyr::uncount(
      n_results # generate n duplicate rows
    ) %>%
    mutate(
      toxgr = sample(
        unique(lb$toxgr),
        n(),
        replace = TRUE,
        prob = table(lb$toxgr) / nrow(lb)
      ),
      lb_te = "Y"
    ) %>%
    rowwise() %>%
    mutate(
      lb_dt = sample_date(.data$firstdosedate, .data$lastdosedate)
    ) %>%
    ungroup() %>%
    select(
      subjid, lb_dt, toxgr, lb_te
    )

  return(lb1)
}

#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_ae <- function(
  dm,
  ae = clindata::rawplus_ae,
  ae_rate = runif(1, .25, 1)
) {
  ae1 <- dm %>%
    select(subjid, firstdosedate, lastdosedate, timeontreatment) %>%
    filter(.data$timeontreatment > 0) %>%
    slice_sample(
      n = ceiling(ae_rate * nrow(.))
    ) %>%
    rowwise() %>%
    mutate(
      event_rate = rnbinom(1, size = 1, mu = ceiling(.data$timeontreatment / 217)) # event every 217 days (median)
    ) %>%
    ungroup() %>%
    mutate(
      n_events = ceiling(.data$timeontreatment / max(event_rate, 1)),
      n = .data$n_events
    ) %>%
    tidyr::uncount(
      .data$n_events # generate n duplicate rows
    ) %>%
    mutate(
      aeser = sample(
        unique(ae$aeser),
        n(),
        replace = TRUE,
        prob = table(ae$aeser) / nrow(ae)
      ),
      aetoxgr = sample(
        unique(ae$aetoxgr),
        n(),
        replace = TRUE,
        prob = table(ae$aetoxgr) / nrow(ae)
      ),
      ae_te = "Y",
    ) %>%
    rowwise() %>%
    mutate(
      aest_dt = sample_date(.data$firstdosedate, lastdosedate),
      aeen_dt = sample_date(.data$aest_dt, .data$lastdosedate + 30)
    ) %>%
    ungroup() %>%
    select(
      subjid, aest_dt, aeen_dt, aeser, aetoxgr, ae_te
    )

  return(ae1)
}

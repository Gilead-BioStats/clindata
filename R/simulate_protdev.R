#' @import dplyr
#' @importFrom lubridate as_date
#' @importFrom tidyr uncount
#'
#' @export
simulate_protdev <- function(
  dm,
  protdev = clindata::rawplus_protdev,
  pd_rate = runif(1, .25, 1)
) {
  protdev1 <- dm %>%
    select(subjid, starts_with("rfp"), timeonstudy) %>%
    filter(timeonstudy > 0) %>%
    slice_sample(
      n = floor(pd_rate * nrow(.))
    ) %>%
    rowwise() %>%
    mutate(
      event_rate = rnbinom(1, size = 1, mu = ceiling(timeonstudy / 258)) # event every 258 days (median)
    ) %>%
    ungroup() %>%
    mutate(
      n_events = floor(timeonstudy / max(event_rate, 1)),
      n = n_events
    ) %>%
    tidyr::uncount(
      n_events # generate n duplicate rows
    ) %>%
    mutate(
      importnt = sample(
        unique(protdev$importnt),
        n(),
        replace = TRUE,
        prob = table(protdev$importnt) / nrow(protdev)
      ),
      dvdecod = sample(
        unique(protdev$dvdecod),
        n(),
        replace = TRUE,
        prob = table(protdev$dvdecod) / nrow(protdev)
      )
    ) %>%
    rowwise() %>%
    mutate(
      dv_dt = sample(rfpst_dt:rfpen_dt, 1) %>%
        lubridate::as_date()
    ) %>%
    ungroup() %>%
    select(
      subjid, dv_dt, importnt, dvdecod
    )

  return(protdev1)
}

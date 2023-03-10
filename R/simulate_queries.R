#' @import dplyr
#' @importFrom tidyr uncount
#'
#' @export
simulate_queries <- function(
  dm,
  queries = clindata::edc_queries,
  query_rate = runif(1, 0.001, .1)
) {
  queries1 <- dm %>%
    select(subjid, starts_with("rfp"), timeonstudy) %>%
    filter(.data$timeonstudy > 0) %>%
    mutate(
      n_queries = ceiling(.data$timeonstudy * query_rate),
      n = .data$n_queries
    ) %>%
    tidyr::uncount(
      .data$n_queries # generate n duplicate rows
    ) %>%
    mutate(
      foldername = "visit",
      form = "form",
      field = "field",
      qry30fl = sample(
        unique(queries$qry30fl),
        n(),
        replace = TRUE,
        prob = table(queries$qry30fl) / nrow(queries)
      )
    ) %>%
    rowwise() %>%
    mutate(
      qryopendate = sample_date(.data$firstparticipantdate, .data$lastparticipantdate)
    ) %>%
    ungroup() %>%
    select(
      subjid, foldername, form, field, qryopendate, qry30fl
    )

  return(queries1)
}

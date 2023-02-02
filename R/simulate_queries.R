simulate_queries <- function(
    dm,
    queries = clindata::rawplus_queries,
    query_rate = runif(1, 0, .1)
) {
    queries1 <- dm %>%
        select(subjid, starts_with('rfp'), timeonstudy) %>%
        filter(timeonstudy > 0) %>%
        mutate(
            n_queries = floor(timeonstudy * query_rate),
            n = n_queries
        ) %>%
        tidyr::uncount(
            n_queries # generate n duplicate rows
        ) %>%
        mutate(
            qry30fl = sample(
                unique(queries$qry30fl),
                n(),
                replace = TRUE,
                prob = table(queries$qry30fl) / nrow(queries)
            )
        ) %>%
        rowwise() %>%
        mutate(
           qryopendate = sample(rfpst_dt:rfpen_dt, 1) %>% lubridate::as_date()
        ) %>%
        ungroup() %>%
        select(
            subjid, qryopendate, qry30fl
        )

    return(queries1)
}

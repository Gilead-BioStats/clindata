simulate_lb <- function(
    lb = clindata::rawplus_lb,
    dm,
    lb_rate = runif(1, .5, 1.1)
) {
    lb1 <- dm %>%
        select(subjid, starts_with('rfx'), timeontreatment) %>%
        filter(timeontreatment > 0) %>%
        mutate(
            n_results = floor(timeontreatment * lb_rate),
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
            lb_te = 'Y'
        ) %>%
        rowwise() %>%
        mutate(
           lb_dt = sample(rfxst_dt:rfxen_dt, 1) %>% lubridate::as_date()
        ) %>%
        ungroup() %>%
        select(
            subjid, lb_dt, toxgr, lb_te
        )

    return(lb1)
}

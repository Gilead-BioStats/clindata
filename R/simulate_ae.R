#' @import dplyr
#' @importFrom lubridate as_date
#' @importFrom tidyr uncount
#'
#' @export
simulate_ae <- function(
    dm,
    ae = clindata::rawplus_ae,
    ae_rate = runif(1, .25, 1)
) {
    ae1 <- dm %>%
        select(subjid, starts_with('rfx'), timeontreatment) %>%
        filter(timeontreatment > 0) %>%
        slice_sample(
            n = floor(ae_rate*nrow(.))
        ) %>%
        rowwise() %>%
        mutate(
            event_rate = rnbinom(1, size = 1, mu = ceiling(timeontreatment/217)) # event every 217 days (median)
        ) %>%
        ungroup() %>%
        mutate(
            n_events = floor(timeontreatment / max(event_rate, 1)),
            n = n_events
        ) %>%
        tidyr::uncount(
            n_events # generate n duplicate rows
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
            ae_te = 'Y',
        ) %>%
        rowwise() %>%
        mutate(
            aest_dt = sample(rfxst_dt:rfxen_dt, 1) %>%
                lubridate::as_date(),
            aeen_dt = sample(aest_dt:(rfxen_dt + 30), 1) %>%
                lubridate::as_date()
        ) %>%
        ungroup() %>%
        select(
            subjid, aest_dt, aeen_dt, aeser, aetoxgr, ae_te
        )

    return(ae1)
}

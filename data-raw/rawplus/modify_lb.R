modify_lb <- function(lb, dm) {
    lb %>%
        filter(
            subjid != "",
            battrnam %in% c('HEMATOLOGY&DIFFERENTIAL PANEL', 'CHEMISTRY PANEL'),
            !is.na(siresn)
        ) %>%
        select(
            subjid, # participant
            visnam, visnum, lb_dt, # timing
            battrnam, lbtstnam, lbtstcd, # measure
            siresn, sinrlo, sinrhi, toxgr
        ) %>%
        left_join(
            dm %>%
                filter(subjid != "") %>%
                select(subjid, rfxst_dt, rfxen_dt),
            'subjid'
        ) %>%
        mutate(
            lb_te = case_when(
                lb_dt == "" | rfxst_dt == "" | rfxen_dt == "" ~ "",
                as.Date(rfxst_dt) <= as.Date(lb_dt) & as.Date(lb_dt) <= (as.Date(rfxen_dt) + 30) ~ 'Y',
                TRUE ~ ''
            )
        ) %>%
        select(-rfxst_dt, -rfxen_dt)
}

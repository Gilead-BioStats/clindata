simulate_dm <- function(
    dm = clindata::rawplus_dm,
    site,
    n_subjects,
    start_date,
    end_date
) {
    dm1 <- dm %>%
        select(
            studyid,
            siteid,
            subjid,
            country,
            rfpst_dt,
            rfpen_dt,
            timeonstudy,
            rfxst_dt,
            rfxen_dt,
            timeontreatment
        )

    if (!is.null(n_subjects)) {
        dm2 <- dm1 %>%
            slice_sample(
                n = n_subjects,
                replace = TRUE
            ) %>%
            group_by(subjid) %>%
            mutate(
                subjid = glue('{subjid}-{row_number()}')
            ) %>%
            ungroup()
    } else {
        dm2 <- dm1
    }

    dm3 <- dm2 %>%
        select(-ends_with('dt'), -starts_with('timeon')) %>%
        mutate(
            siteid = sample(site$SITE_NUM, nrow(dm2), TRUE, runif(nrow(site))),
            rfpst_dt = sample(start_date:end_date, n(), TRUE),
            rfxst_dt = rfpst_dt + sample(1:60, n(), TRUE)
        ) %>%
        rowwise() %>%
        mutate(
            rfpen_dt = sample(rfxst_dt:end_date, 1),
            rfxen_dt = sample(rfxst_dt:rfpen_dt, 1)
        ) %>%
        ungroup() %>%
        mutate(
            across(ends_with('dt'), lubridate::as_date),
            timeonstudy = as.numeric(rfpen_dt - rfpst_dt) + 1,
            timeontreatment = as.numeric(rfxen_dt - rfxst_dt) + 1
        )

    return(dm3)
}

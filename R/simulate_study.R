simulate_study <- function(
    n_sites = NULL,
    n_subjects = NULL,
    start_date = NULL,
    end_date = NULL,
    duration = NULL
) {
    if (is.null(n_sites)) {
        n_sites <- nrow(clindata::ctms_site)
    }

    if (is.null(n_subjects)) {
        n_subjects <- nrow(clindata::rawplus_dm)
    }

    # TODO: add logic around start date, end date, and duration
    if (is.null(start_date)) {
        start_date <- as.Date(min(clindata::rawplus_dm$rfpst_dt, na.rm = TRUE))
        print(start_date)
    }

    if (is.null(end_date)) {
        end_date <- as.Date(max(clindata::rawplus_dm$rfpen_dt, na.rm = TRUE))
        print(end_date)
    }

    if (is.null(duration)) {
        duration <- as.numeric(
            as.Date(end_date) - as.Date(start_date)
        ) + 1
    }

    studyid <- glue::glue('s-${n_sites}-${n_subjects}-${duration}')

    # sites
    ctms_site <- clindata::ctms_site

    if (!is.null(n_sites)) {
        inv <- ctms_site %>%
            slice_sample(
                n = n_sites,
                replace = TRUE
            ) %>%
            group_by(SITE_NUM) %>%
            mutate(
                SITE_NUM = glue('{SITE_NUM}-{row_number()}')
            ) %>%
            ungroup()
    } else {
        inv <- ctms_site
    }

    # subjects
    rawplus_dm <- clindata::rawplus_dm

    if (!is.null(n_subjects)) {
        dm <- rawplus_dm %>%
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
            ) %>%
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
        dm <- rawplus_dm
    }

    dm1 <- dm %>%
        select(-ends_with('dt'), -starts_with('timeon')) %>%
        mutate(
            siteid = sample(inv$SITE_NUM, nrow(dm), TRUE, runif(nrow(inv))),
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

    # adverse events
    # TODO:
    ae_rate <- runif(1, .25, 1)
    ae <- dm1 %>%
        select(subjid, starts_with('rfx'), timeontreatment) %>%
        filter(timeontreatment > 0) %>%
        slice_sample(
            n = floor(ae_rate*nrow(dm1))
        ) %>%
        rowwise() %>%
        mutate(
            event_rate = rnbinom(1, size = 1, mu = ceiling(timeontreatment/250)) # event every 250 days on average
        ) %>%
        ungroup() %>%
        mutate(
            n_events = floor(timeontreatment / max(event_rate, 1)),
            n = n_events
        ) %>%
        tidyr::uncount(
            n_events # generate n duplicate rows
        )

    # protocol deviations

    # labs

    # disposition

    # edc

    return(ae)
}

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

    # TODO: move each domain into its own function
    studyid <- glue::glue('s-${n_sites}-${n_subjects}-${duration}')

    # sites
    site <- simulate_site(
        clindata::ctms_site,
        n_sites
    )

    # subjects
    dm <- simulate_dm(
        clindata::rawplus_dm,
        site,
        n_subjects,
        start_date,
        end_date
    )

    # adverse events
    ae <- simulate_ae(
        clindata::rawplus_ae,
        dm
    )

    # protocol deviations
    protdev <- simulate_protdev(
        clindata::rawplus_protdev,
        dm
    )

    # labs
    lb <- simulate_lb(
        clindata::rawplus_lb,
        dm
    )

    # disposition - study
    studcomp <- simulate_studcomp(
        clindata::rawplus_studcomp,
        dm
    )

    # disposition - treatment
    sdrgcomp <- simulate_sdrgcomp(
        clindata::rawplus_sdrgcomp,
        dm
    )

    # queries
    queries <- simulate_queries(
        clindata::edc_queries,
        dm
    )

    # data entry lag & data change rate
    data_pages <- simulate_data_pages(
        clindata::edc_data_entry_lag,
        dm
    )

    # disposition - enrollment
    enroll <- simulate_enroll(
        clindata::rawplus_enroll,
        dm
    )
    
    data <- list(
        site = site,
        dm = dm,
        ae = ae,
        protdev = protdev,
        lb = lb,
        studcomp = studcomp,
        sdrgcomp = sdrgcomp,
        queries = queries,
        data_entry_lag = data_pages,
        data_change_rate = data_pages,
        enroll = enroll
    )

    # Display number of rows.
    data %>%
        map_int(~nrow(.x)) %>%
        stack %>%
        select(
            domain = ind,
            n_rows = values
        ) %>%
        print(row.names = FALSE)

    return(data)
}

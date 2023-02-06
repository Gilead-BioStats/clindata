# TODO: use start and end date arguments
simulate_study <- function(
    n_sites = NULL,
    n_subjects = NULL,
    start_date = NULL,
    end_date = NULL,
    duration = NULL,
    rename_gsm = FALSE
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
        print(duration)
    }

    studyid <- glue::glue('study-{n_sites}_sites-{n_subjects}_subjects') %>%
        as.character

    # sites
    site <- simulate_site(
        n_sites,
        studyid
    )

    # subjects
    dm <- simulate_dm(
        site,
        n_subjects,
        start_date,
        end_date
    )

    # rawplus
    ae <- simulate_ae(dm)
    protdev <- simulate_protdev(dm)
    lb <- simulate_lb(dm)
    studcomp <- simulate_studcomp(dm)
    sdrgcomp <- simulate_sdrgcomp(dm)
    enroll <- simulate_enroll(dm)

    # edc
    queries <- simulate_queries(dm)
    data_pages <- simulate_data_pages(dm)
    
    # list of data domains
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

    if (rename_gsm) {
        mapping <- c(
            'site' = 'dfSITE',
            'dm' = 'dfSUBJ',
            'ae' = 'dfAE',
            'protdev' = 'dfPD',
            'lb' = 'dfLB',
            'studcomp' = 'dfSTUDCOMP',
            'sdrgcomp' = 'dfSDRGCOMP',
            'queries' = 'dfQUERY',
            'data_entry_lag' = 'dfDATAENT',
            'data_change_rate' = 'dfDATACHG',
            'enroll' = 'dfENROLL'
        )

        names(data) <- mapping[names(data)]
    }

    return(data)
}

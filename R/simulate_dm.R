#' @import dplyr
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom tidyr uncount
#'
#' @export
simulate_dm <- function(
    site,
    n_subjects,
    start_date,
    end_date,
    dm = clindata::rawplus_dm
) {
    dm1 <- dm %>%
        select(
            subjid
        )

    if (!is.null(n_subjects)) {
        dm2 <- dm1 %>%
            slice_sample(
                n = n_subjects,
                replace = TRUE
            ) %>%
            group_by(subjid) %>%
            mutate(
                subjid = glue::glue('{subjid}-{row_number()}') %>%
                    as.character
            ) %>%
            ungroup()
    } else {
        dm2 <- dm1
    }

    dm3 <- dm2 %>%
        mutate(
            siteid = sample(site$SITE_NUM, nrow(dm2), TRUE, runif(nrow(site))),
            rfpst_dt = sample(start_date:end_date, n(), TRUE),
            rfxst_dt = rfpst_dt + sample(1:60, n(), TRUE)
        ) %>%
        left_join(
            site %>%
                select(studyid = PROTOCOL, SITE_NUM, country = COUNTRY),
            c('siteid' = 'SITE_NUM')
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
        ) %>%
        select(
            studyid, siteid, country, subjid,
            rfpst_dt, rfpen_dt, timeonstudy,
            rfxst_dt, rfxen_dt, timeontreatment
        )
    print(table(dm3$siteid))

    return(dm3)
}

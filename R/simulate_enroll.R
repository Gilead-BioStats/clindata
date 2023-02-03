simulate_enroll <- function(
    dm,
    enroll = clindata::rawplus_enroll,
    sf_rate = runif(1, 0, 1)
) {
    enrolled <- dm %>%
        select(
            studyid,
            siteid,
            subjid,
            country,
            enroll_dt = rfpst_dt
        ) %>%
        mutate(
            enrollyn = 'Y',
            sfreas = ''
        )

    sfreas <- enroll$sfreas %>% .[. != '']
    screen_failures <- enrolled %>%
        slice_sample(
            n = floor(sf_rate*nrow(enrolled))
        ) %>%
        group_by(subjid) %>%
        mutate(
            subjid = paste0('sf', subjid, row_number())
        ) %>%
        ungroup() %>%
        mutate(
            enrollyn = 'N',
            sfreas = sample(
                unique(sfreas),
                n(),
                replace = TRUE,
                prob = table(sfreas) / length(sfreas)
            )
        )

    enroll1 <- enrolled %>%
        bind_rows(screen_failures)

    return(enroll1)
}

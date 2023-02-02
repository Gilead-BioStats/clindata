simulate_enroll <- function(
    enroll = clindata::rawplus_enroll,
    dm,
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
            enrollyn = 'Y'
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
            sfreas = sample(
                unique(sfreas),
                n(),
                replace = TRUE,
                prob = table(sfreas) / length(sfreas)
            )
        )

    enroll <- enrolled %>%
        bind_rows(screen_failures) %>%
        mutate(
            enrollyn = coalesce(enrollyn, 'N'),
            sfreas = coalesce(sfreas, '')
        )

    return(enroll)
}

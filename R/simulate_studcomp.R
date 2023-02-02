simulate_studcomp <- function(
    studcomp = clindata::rawplus_studcomp,
    dm,
    disc_rate = runif(1, 0.001, .25)
) {
    compreas <- studcomp$compreas %>% .[. != '']
    studcomp1 <- dm %>%
        filter(timeonstudy > 0) %>%
        select(subjid) %>%
        slice_sample(
            n = floor(disc_rate*nrow(dm))
        ) %>%
        mutate(
            compyn = 'N',
            compreas = sample(
                unique(compreas),
                n(),
                replace = TRUE,
                prob = table(compreas) / length(compreas)
            )
        )

    return(studcomp1)
}

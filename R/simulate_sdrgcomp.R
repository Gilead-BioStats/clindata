simulate_sdrgcomp <- function(
    dm,
    sdrgcomp = clindata::rawplus_sdrgcomp,
    disc_rate = runif(1, 0.001, .25)
) {
    sdrgreas <- sdrgcomp$sdrgreas %>% .[. != '']
    sdrgcomp1 <- dm %>%
        filter(timeontreatment > 0) %>%
        select(subjid) %>%
        slice_sample(
            n = floor(disc_rate*nrow(dm))
        ) %>%
        mutate(
            sdrgyn = 'N',
            sdrgreas = sample(
                unique(sdrgreas),
                n(),
                replace = TRUE,
                prob = table(sdrgreas) / length(sdrgreas)
            )
        )

    return(sdrgcomp1)
}

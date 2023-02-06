simulate_site <- function(
    n_sites,
    studyid,
    site = clindata::ctms_site
) {
    if (!is.null(n_sites)) {
        site1 <- site %>%
            slice_sample(
                n = n_sites,
                replace = TRUE
            ) %>%
            group_by(SITE_NUM) %>%
            mutate(
                SITE_NUM = glue('{SITE_NUM}-{row_number()}') %>%
                    as.character
            ) %>%
            ungroup()
    } else {
        site1 <- site
    }

    site1$PROTOCOL <- studyid

    return(site1)
}

simulate_site <- function(
    site = clindata::ctms_site,
    n_sites
) {
    if (!is.null(n_sites)) {
        site1 <- site %>%
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
        site1 <- site
    }

    return(site1)
}

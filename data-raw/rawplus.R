library(dplyr)
devtools::load_all()
datasets <- rawplus_1_import()

# join country data from ctms
# default to 'US' when country is missing
ctms <- ctms_1_import()
site <- ctms$site %>%
    select(SITE_NUM, COUNTRY) %>%
    mutate(
        across(everything(), as.character)
    )

datasets$dm <- datasets$dm %>%
  full_join(
    site,
    by = c("siteid" = 'SITE_NUM')
  ) %>%
  mutate(
    country = ifelse(is.na(COUNTRY), 'US', COUNTRY),
    timeonstudy = dplyr::coalesce(timeonstudy, 0),
    timeontreatment = dplyr::coalesce(timeontreatment, 0)
  ) %>%
    select(-COUNTRY)
datasets_processed <- rawplus_2_process(datasets)
rawplus_3_export(datasets_processed)
rawplus_4_document(datasets_processed)

# temporary fix for [ ie ]
load('data-raw/rawplus/rawplus_ie.rda')
rawplus_ie <- tibble::as_tibble(rawplus_ie)
names(rawplus_ie) <- c('subjid', 'iecat', 'ieorres', 'tiver')
usethis::use_data(rawplus_ie, overwrite = TRUE)

# temporary fix for [ consent ]
load('data-raw/rawplus/rawplus_consent.rda')
names(rawplus_consent) <- c('subjid', 'consdt', 'conscat', 'consyn')
usethis::use_data(rawplus_consent, overwrite = TRUE)

# temporary fix for [ enroll ]
rawplus_enroll <- arrow::read_parquet('data-raw/rawplus/dm.parquet') %>%
    full_join(
        site,
        by = c("siteid" = 'SITE_NUM')
    ) %>%
    mutate(
        country = ifelse(is.na(COUNTRY), 'US', COUNTRY)
    ) %>%
    select(-COUNTRY) %>%
    mutate(
        enrollyn = if_else(
            subjid != '',
            'Y',
            'N'
        )
    ) %>%
    group_by(enrollyn) %>%
    arrange(rfpst_dt, as.numeric(siteid), scrnid) %>%
    mutate(
        sfreas = if_else(
            enrollyn == 'Y',
            '',
            sample(
                c(
                    'Inclusion/Exclusion Criteria',
                    'Withdrawal of Consent',
                    'Medical Issue'
                ),
                n(),
                TRUE,
                prob = c(.9, .05, .05)
            )
        ),
        subjid = if_else(
            enrollyn == 'Y',
            subjid,
            paste0('sf', row_number())
        )
    ) %>%
    ungroup %>%
    select(
        studyid, siteid, subjid,
        enroll_dt = rfpst_dt, enrollyn, sfreas,
        country, invid
    )

usethis::use_data(rawplus_enroll, overwrite = TRUE)

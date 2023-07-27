devtools::load_all()
library(dplyr)
library(gsm)
library(rbmPipe)

datasets <- rawplus_1_import()

# join country data from ctms
# default to 'US' when country is missing
ctms <- ctms_1_import()
site <- ctms$site %>%
    select(site_num, country) %>%
    mutate(site_num = as.character(site_num))

datasets$dm <- datasets$dm %>%
  full_join(
    site,
    by = c("siteid" = 'site_num')
  ) %>%
  mutate(
    enrollyn = "Y",
    country = ifelse(is.na(country), 'US', country),
    timeonstudy = dplyr::coalesce(timeonstudy, 0),
    timeontreatment = dplyr::coalesce(timeontreatment, 0)
  )
datasets_processed <- rawplus_2_process(datasets)
rawplus_3_export(datasets_processed)
rawplus_4_document(datasets_processed)

# temporary fix for [ ie ]
load('data-raw/rawplus/rawplus_ie.rda')
rawplus_ie <- tibble::as_tibble(rawplus_ie)
rawplus_ie <- as.data.frame(lapply(rawplus_ie, as.character), stringsAsFactors = FALSE)
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
        by = c("siteid" = 'site_num')
    ) %>%
    mutate(
        country = ifelse(is.na(country), 'US', country)
    ) %>%
    mutate(
        enrollyn = if_else(
            subjid != '',
            'Y',
            'N'
        )
    ) %>%
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
        )
    ) %>%
    select(
        studyid, siteid, subjectid, subjid,
        enroll_dt = firstparticipantdate, enrollyn, sfreas,
        country, invid
    ) %>%
    mutate(enroll_dt = as.Date(enroll_dt, format = "%Y-%m-%d"))

usethis::use_data(rawplus_enroll, overwrite = TRUE)
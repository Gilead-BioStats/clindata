#' Data inputs
#' - clindata::rawplus_visdt
#' - data-raw/edc/form-field.tsv

# load_all('.')
library(dplyr)
library(lubridate)
library(data.table)

set.seed(8675309)

# Visit data.
folder <- clindata::rawplus_visdt %>%
  select(subjid, foldername, visit_dt) %>%
  mutate(
    visit_dt = ymd(visit_dt)
  )

# Form/field metadata.
form_field <- fread('data-raw/edc/form-field.tsv') %>%
  filter(
    !form %in% c('Adverse Event')
  )

# Merge visit data and form/field metadata (Cartesian join) to generate data points.
data_points <- folder %>%
  merge(
    form_field,
    all = TRUE
  ) %>%
  filter(
    !(
      foldername == 'Screening' &
        !form %in% c(
          'Consents',
          'Visit Date',
          'Vital Signs Performed',
          'Prior and Concomitant Medication'
        )
    ),
    !(
      foldername != 'Screening' &
        form == 'Consents'
    ),
    !(
      foldername != 'Day 1' &
        form == 'Enrollment'
    ),
    !(
      foldername == 'Unscheduled' &
        !form %in% c(
          'PK',
          'Study Drug Administration (DRUG1)',
          'Study Drug Administration (DRUG2)',
          'Study Drug Administration (DRUG3)',
          'Study Drug Administration (PK)'
        )
    )
  ) %>%
  arrange(
    subjid, visit_dt, foldername, form
  )

saveRDS(
  data_points,
  'data-raw/edc/data_points.Rds'
)

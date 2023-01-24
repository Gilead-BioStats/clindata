#' Data inputs
#' - clindata::rawplus_visdt
#' - data-raw/edc/form-field.tsv

# load_all('.')
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

set.seed(8675309)

# Visit data with unique visit values within each subject ID.
folder <- clindata::rawplus_visdt %>%
  filter(
    visit_dt != ''
  ) %>%
  select(subjid, foldername, visit_dt, folderseq_nsv) %>%
  arrange(subjid, visit_dt) %>%
  group_by(subjid) %>%
  mutate(
    visit_dt = ymd(visit_dt),
    # Group unscheduled visits with most recent, prior scheduled visit.
    visitnum_tmp = ifelse(
        foldername != 'Unscheduled',
        folderseq_nsv,
        NA
    )
  ) %>%
  fill(visitnum_tmp, .direction = 'down') %>%
  group_by(subjid, visitnum_tmp) %>%
  mutate(
    # Increment visit number by .1 for each unscheduled visit following the most recent, prior
    # scheduled visit.
    visitnum = if_else(
      row_number() == 1,
      visitnum_tmp,
      visitnum_tmp + (row_number()-1)*.1
    ),
    # Concatenate visit name with visit number to define unique visit name.
    foldername = if_else(
      row_number() == 1,
      foldername,
      paste(foldername, visitnum)
    )
  ) %>%
  ungroup() %>%
  select(-folderseq_nsv, -visitnum_tmp)

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
    subjid, visit_dt, foldername, form, field
  )

saveRDS(
  data_points,
  'data-raw/edc/data_points.Rds'
)

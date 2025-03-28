#' Data inputs
#' - clindata::rawplus_visdt
#' - data-raw/edc/form-field.tsv

#load_all('.')
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

set.seed(8675309)

# Visit data with unique visit values within each subject ID.
visit <- clindata::rawplus_visdt %>%
  filter(
    visit_dt != ''
  ) %>%
  select(
      protocolname = studyid,
      subjectname = subject_nsv,
      visit = foldername,
      visit_date = visit_dt,
      folderseq_nsv
  ) %>%
  arrange(protocolname, subjectname, visit_date) %>%
  group_by(protocolname, subjectname) %>%
  mutate(
      visit_date = ymd(visit_date),
    # Group unscheduled visits with most recent, prior scheduled visit.
    visitnum_tmp = ifelse(
        visit != 'Unscheduled',
        folderseq_nsv,
        NA
    )
  ) %>%
  fill(visitnum_tmp, .direction = 'down') %>%
  group_by(protocolname, subjectname, visitnum_tmp) %>%
  mutate(
    # Increment visit number by .1 for each unscheduled visit following the most recent, prior
    # scheduled visit.
    visitnum = if_else(
      row_number() == 1,
      visitnum_tmp,
      visitnum_tmp + (row_number()-1)*.1
    ),
    # Concatenate visit name with visit number to define unique visit name.
    visit = if_else(
      row_number() == 1,
      visit,
      paste(visit, visitnum)
    )
  ) %>%
  ungroup() %>%
  select(-folderseq_nsv, -visitnum_tmp)

# form/field metadata.
form_field <- fread('data-raw/edc/form-field.tsv') %>%
  filter(
    !formoid %in% c('Adverse Event')
  )

# Merge visit data and form/field metadata (Cartesian join) to generate data points.
data_points <- visit %>%
  merge(
    form_field,
    all = TRUE
  ) %>%
  filter(
    !(
      visit == 'Screening' &
        !formoid %in% c(
          'Consents',
          'Visit Date',
          'Vital Signs Performed',
          'Prior and Concomitant Medication'
        )
    ),
    !(
      visit != 'Screening' &
        formoid == 'Consents'
    ),
    !(
      visit != 'Day 1' &
        formoid == 'Enrollment'
    ),
    !(
      visit == 'Unscheduled' &
        !formoid %in% c(
          'PK',
          'Study Drug Administration (DRUG1)',
          'Study Drug Administration (DRUG2)',
          'Study Drug Administration (DRUG3)',
          'Study Drug Administration (PK)'
        )
    )
  ) %>%
  group_by(protocolname, subjectname, visit, visit_date, formoid, fieldoid) %>%
  mutate(
    log_number = row_number()
  ) %>%
  ungroup() %>%
  arrange(
    protocolname, subjectname, visit_date, visitnum, visit, formoid, fieldoid, log_number
  ) %>%
  mutate(
    datapointid = row_number(),
    n_changes = rpois(n(), .3),
    isrequired = 1
  ) %>%
  select(
    protocolname,
    subjectname,
    visit,
    visit_date,
    formoid,
    fieldoid,
    log_number,
    datapointid, n_changes, isrequired
  )

saveRDS(
  data_points,
  'data-raw/edc/data_points.Rds'
)

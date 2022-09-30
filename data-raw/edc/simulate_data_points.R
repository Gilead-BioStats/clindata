load_all('.')
library(dplyr)
library(lubridate)

set.seed(8675309)

# Snapshot date for imputation of query end date.
snapshot_date <- get_snapshot_date()

# Participant-level data with study timeline from which to impute query start date.
subjid <- rawplus_dm %>%
  select(subjid, rfpst_dt, rfpen_dt, timeonstudy) %>%
  mutate(
    rfpst_dt = ymd(rfpst_dt),
    rfpen_dt = ymd(rfpen_dt),
  )

# Visit data.
folder <- rawplus_visdt %>%
  select(subjid, foldername, visit_dt) %>%
  mutate(
    visit_dt = ymd(visit_dt)
  )

# Form/field metadata.
form_field <- data.table::fread('data-raw/edc/form-field.tsv') %>%
  filter(
    !form %in% c('Adverse Event')
  )

# Merge visit data and form/field metadata (Cartesian join) to generate queries.
data_points <- folder %>%
  merge(
    form_field,
    all = TRUE
  ) %>% filter(
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
  )

data_points1 <- data_points %>%
  group_by(subjid, foldername, visit_dt, form) %>%
  tally(name = 'n_data_points') %>%
  mutate(
    data_entry_lag = rnbinom(n(), size = 1, mu = 2)
  )

data.table::fwrite(
  data_points1,
  'data-raw/edc/data_points.csv'
)

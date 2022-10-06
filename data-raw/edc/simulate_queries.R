#' Data inputs
#' - clindata::rawplus_dm
#' - data-raw/edc/data_points.Rds

# load_all('.')
library(dplyr)
library(lubridate)
library(data.table)

set.seed(8675309)

# snapshot date for imputation of query end date.
snapshot_date <- get_snapshot_date()

# participant-level data with study timeline from which to impute query start date.
subjid <- rawplus_dm %>%
  select(subjid, rfpst_dt, rfpen_dt, timeonstudy) %>%
  mutate(
    rfpst_dt = ymd(rfpst_dt),
    rfpen_dt = ymd(rfpen_dt),
  )

# data point-level data
data_points <- readRDS('data-raw/edc/data_points.Rds')

# query-level data
queries <- data_points %>%
  # keep ~2% of records
  filter(
    runif(n()) < .02,
  ) %>%
  arrange(
    subjid, foldername, form, field
  )

# Marking group set from which to sample.
markinggroupname <- c(
  'System Query' = .7,
  'CRA to Site Manual Query' = .2,
  'CDA to Site Manual Query' = .1
)

# Query status set from which to sample.
qrystatus <- c(
  'Closed' = .5,
  'Answered' = .3,
  'Open' = .2
)

# Join queries and participant-level data.
queries1 <- queries %>%
  left_join(
    subjid,
    'subjid'
  ) %>%
  mutate(
    # Sample marking group set.
    markinggroupname = sample(
      names(markinggroupname), n(), TRUE, markinggroupname
    ),
    # Sample query status set.
    qrystatus = sample(
      names(qrystatus), n(), TRUE, qrystatus
    ),
    timeonstudy_since_visit = if_else(
      !is.na(visit_dt),
      (rfpen_dt - visit_dt) + 1,
      timeonstudy
    )
  ) %>%
  # In order to sample `timeonstudy_since_visit` as a single value rather than a vector the
  # imputation needs to run on each row individually.
  rowwise() %>%
  mutate(
    # Impute query open date as any date between visit date and study end date + 30.
    qryopendate = rfpst_dt + sample(timeonstudy_since_visit + 31, 1) - 1
  ) %>%
  ungroup() %>%
  mutate(
    # Set to "Closed" any queries older than a year.
    qrystatus = if_else(
      snapshot_date - qryopendate > 365,
      'Closed',
      qrystatus
    ),
    # Generate random number from negative binomial distribution with mu = 3.
    qryresponsedate = if_else(
      qrystatus == 'Answered',
      qryopendate + rnbinom(n(), size = 1, mu = 3),
      as.Date(NA)
    ),
    # Generate random number from negative binomial distribution with mu = 7.
    qryclosedate = if_else(
      qrystatus == 'Closed',
      qryopendate + rnbinom(n(), size = 1, mu = 7),
      as.Date(NA)
    ),
    # Define query "end date" dependent on query status.
    qryenddate = case_when(
      qrystatus == 'Answered' ~ qryresponsedate,
      qrystatus == 'Closed' ~ qryclosedate,
      qrystatus == 'Open' & qryopendate <= snapshot_date ~ snapshot_date,
      qrystatus == 'Open' & qryopendate > snapshot_date ~ qryopendate
    ),
    # Define query age.
    qryage = as.numeric(qryenddate - qryopendate) + 1,
    # Define query age flag.
    qry30fl = if_else(
      qryage > 30,
      'Y',
      'N'
    ),
    # Define query age category.
    qryagecat = case_when(
      qryage <   7 ~ '<7 days',
      qryage <= 14 ~ '7-14 days',
      qryage <= 21 ~ '14-21 days',
      qryage <= 28 ~ '21-28 days',
      TRUE ~ '>28 days'
    )
  ) %>%
  select(all_of(c(
    'subjid', 'foldername', 'form', 'field',
    'qrystatus', 'markinggroupname', 'qryage', 'qryagecat', 'qry30fl',
    'qryopendate', 'qryresponsedate', 'qryclosedate'
  )))

fwrite(
  queries1,
  'data-raw/edc/queries.csv'
)

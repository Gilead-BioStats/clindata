#' Data inputs
#' - clindata::rawplus_dm
#' - data-raw/edc/data_points.Rds

#load_all('.')
library(dplyr)
library(lubridate)
library(data.table)

set.seed(8675309)

# snapshot date for imputation of query end date.
snapshot_date <- get_snapshot_date()

# participant-level data with study timeline from which to impute query start date.
subjid <- rawplus_dm %>%
  select(subject_nsv, firstparticipantdate, lastparticipantdate, timeonstudy) %>%
  mutate(
    firstparticipantdate = ymd(firstparticipantdate),
    lastparticipantdate = ymd(lastparticipantdate),
  )

# data point-level data
data_points <- readRDS('data-raw/edc/data_points.Rds')

# query-level data
queries <- data_points %>%
  # keep ~2% of all data points
  filter(
    runif(n()) < .02,
  ) %>%
  arrange(
    subjectname, foldername, formoid, fieldoid
  )

# Marking group set from which to sample.
markinggroup <- c(
  'System Query' = .7,
  'CRA to Site Manual Query' = .2,
  'CDA to Site Manual Query' = .1
)

# Query status set from which to sample.
querystatus <- c(
  'Closed' = .5,
  'Answered' = .3,
  'Open' = .2
)

# Join queries and participant-level data.
queries1 <- queries %>%
  left_join(
    subjid,
    c('subjectname' = 'subject_nsv')
  ) %>%
  mutate(
    # Sample marking group set.
    markinggroup = sample(
      names(markinggroup), n(), TRUE, markinggroup
    ),
    # Sample query status set.
    querystatus = sample(
      names(querystatus), n(), TRUE, querystatus
    ),
    timeonstudy_since_visit = if_else(
      !is.na(visitdat_date),
      as.numeric(lastparticipantdate - visitdat_date) + 1,
      timeonstudy
    )
  ) %>%
  # In order to sample `timeonstudy_since_visit` as a single value rather than a vector the
  # imputation needs to run on each row individually.
  rowwise() %>%
  mutate(
    # Impute query open date as any date between visit date and study end date + 30.
    created = firstparticipantdate + sample(timeonstudy_since_visit + 31, 1) - 1
  ) %>%
  ungroup() %>%
  mutate(
    # Set to "Closed" any queries older than a year.
    querystatus = if_else(
      snapshot_date - created > 365,
      'Closed',
      querystatus
    ),
    # Generate random number from negative binomial distribution with mu = 3.
    answered = if_else(
      querystatus == 'Answered',
      created + rnbinom(n(), size = 1, mu = 3),
      as.Date(NA)
    ),
    # Generate random number from negative binomial distribution with mu = 7.
    resolved = if_else(
      querystatus == 'Closed',
      created + rnbinom(n(), size = 1, mu = 7),
      as.Date(NA)
    ),
    # Define query "end date" dependent on query status.
    qryenddate = case_when(
      querystatus == 'Answered' ~ answered,
      querystatus == 'Closed' ~ resolved,
      querystatus == 'Open' & created <= snapshot_date ~ snapshot_date,
      querystatus == 'Open' & created > snapshot_date ~ created
    ),
    # Define query age.
    queryage = as.numeric(qryenddate - created) + 1
  ) %>%
  select(all_of(c(
    'protocolname', 'subjectname', 'foldername', 'formoid', 'fieldoid', 'log_number', 'datapointid',
    'querystatus', 'created', 'answered', 'resolved',
    'queryage', 'markinggroup'
  )))

saveRDS(
  queries1,
  'data-raw/edc/queries.Rds'
)

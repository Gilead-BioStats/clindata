#' Data inputs
#' - data-raw/edc/data_points.Rds

# load_all('.')
library(dplyr)
library(data.table)

set.seed(8675309)

# data point-level data
data_points <- readRDS('data-raw/edc/data_points.Rds')

# Simulate data entry lag and flag data points with data entry lag greater than 10 days.
data_entry_lag <- data_points %>%
  group_by(protocolname, subjectname, foldername, visitdat_date, formoid) %>%
  tally() %>%
  mutate(
    data_entry_lag = rnbinom(n(), size = 1, mu = 3),
    min_entereddate = visitdat_date + data_entry_lag
  ) %>%
  select(
    protocolname, subjectname, foldername, formoid, visitdat_date, min_entereddate, data_entry_lag
  )

saveRDS(
  data_entry_lag,
  'data-raw/edc/data_pages.Rds'
)

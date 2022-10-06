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
  group_by(subjid, foldername, form) %>%
  tally(name = 'n_data_points') %>%
  mutate(
    data_entry_lag = rnbinom(n(), size = 1, mu = 3),
    data_entry_lag_fl = if_else(
      data_entry_lag > 10,
      'Y',
      'N'
    )
  ) %>%
  select(-n_data_points)

fwrite(
  data_entry_lag,
  'data-raw/edc/data_entry_lag.csv'
)

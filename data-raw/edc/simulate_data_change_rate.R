#' Data inputs
#' - data-raw/edc/data_points.Rds
#' - clindata::edc_queries

# load_all('.')
library(dplyr)
library(data.table)

set.seed(8675309)

# data point-level data
data_points <- readRDS('data-raw/edc/data_points.Rds')

# query-level data
queries <- clindata::edc_queries

# Count the number of queries.
n_queries <- queries %>%
  group_by(subjid, foldername, form, field) %>%
  tally(name = 'n_queries')

# Simulate data change rate.
data_change_rate <- data_points %>%
  left_join(
    n_queries,
    c('subjid', 'foldername', 'form', 'field')
  ) %>%
  mutate(
    n_queries = coalesce(n_queries, 0)
  ) %>%
  group_by(n_queries) %>%
  mutate(
    n_data_changes = if_else(
      n_queries > 0,
      rpois(n(), .3),
      0L
    ),
  ) %>%
  ungroup

data_change_rate1 <- data_change_rate %>%
  group_by(subjid, foldername, form) %>%
  summarize(
    n_data_points = n(),
    n_data_points_with_changes = sum(n_data_changes > 0),
    n_data_point_changes = sum(n_data_changes),
    #n_queries = sum(n_queries),
  ) %>%
  ungroup

fwrite(
  data_change_rate1,
  'data-raw/edc/data_change_rate.csv'
)

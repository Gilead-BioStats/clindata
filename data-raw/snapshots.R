library(dplyr)
library(lubridate)
load_all('.')

purrr::walk(list.files('R', full.names = TRUE), source)

snapshot <- snapshot_all(10, 12) # 10 snapshots back x 12 months per snapshot

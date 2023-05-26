# overview ----------------------------------------------------------------
# this is used to create clindata::ctms_site
# for now, we want to make sure that ctms_site$siteid contains all siteids found in clindata::rawplus_dm

library(dplyr)
library(randomNames)
library(jsonlite)

set.seed(1)

status_site <- clindata::rawplus_dm %>%
  group_by(studyid, siteid, invid, country) %>%
  summarize(
    site_active_dt = min(rfpst_dt)
  ) %>%
  mutate(
    protocol_row_id = '1-1G9113',
    site_row_id = paste0('123-1234-', invid),
    pi_last_name = randomNames(n(), which.names = 'last'),
    pi_first_name = randomNames(n(), which.names = 'first'),
    site_status = 'Active',
    is_satellite = sample(c(FALSE, TRUE), n(), TRUE, prob = c(.9, .1)),
    city = paste("city",row_number()),
    state = paste("state",row_number()),
    account = paste(CITY, 'Medical Center')
  ) %>%
  select(
      protocol_row_id,
      site_num = siteid,
      site_row_id,
      protocol = studyid,
      pi_number = invid,
      pi_last_name,
      pi_first_name,
      site_status,
      is_satellite,
      account,
      site_active_dt,
      city,
      state,
      country
  )

jsonlite::write_json(
    status_site,
    paste0(here::here('data-raw', 'ctms'), '/site.json'),
    pretty = 4
)

write.csv(
    status_site,
    paste0(here::here('data-raw', 'ctms'), '/site.csv'),
    row.names = FALSE
)

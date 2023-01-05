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
    SITE_ACTIVE_DT = min(rfpst_dt)
  ) %>%
  mutate(
    PROTOCOL_ROW_ID = '1-1G9113',
    SITE_ROW_ID = paste0('123-1234-', invid),
    PI_LAST_NAME = randomNames(n(), which.names = 'last'),
    PI_FIRST_NAME = randomNames(n(), which.names = 'first'),
    SITE_STATUS = 'Active',
    IS_SATELLITE = sample(c(FALSE, TRUE), n(), TRUE, prob = c(.9, .1)),
    CITY = paste("city",row_number()),
    STATE = paste("state",row_number()),
    ACCOUNT = paste(CITY, 'Medical Center')
  ) %>%
  select(
      PROTOCOL_ROW_ID,
      SITE_NUM = siteid,
      SITE_ROW_ID,
      PROTOCOL = studyid,
      PI_NUMBER = invid,
      PI_LAST_NAME,
      PI_FIRST_NAME,
      SITE_STATUS,
      IS_SATELLITE,
      ACCOUNT,
      SITE_ACTIVE_DT,
      CITY,
      STATE,
      COUNTRY = country
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

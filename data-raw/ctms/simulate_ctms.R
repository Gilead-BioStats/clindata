
# overview ----------------------------------------------------------------
# this is used to create clindata::ctms_site
# for now, we want to make sure that ctms_site$siteid contains all siteids found in clindata::rawplus_dm

library(randomNames)

set.seed(1)

status_site <- clindata::rawplus_dm %>%
  group_by(siteid) %>%
  summarize(
    planned_participants = n(),
    start_date = min(rfpen_dt)
  ) %>%
  mutate(
    studyid = 'AA-AA-000-0000',
    country = sample(c("US", "China", "Japan"), n(), replace = TRUE),
    region = if_else(country == "US", "NA", "AS"),
    invname = randomNames(n())
  ) %>%
  relocate(studyid, siteid)

write.csv(status_site, paste0(here::here('data-raw', 'ctms'), '/site.csv'), row.names = FALSE)

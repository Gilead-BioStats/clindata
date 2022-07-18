here::i_am("data-raw/rawplus/makeRawPlus.R")
library(here)
library(yaml)
library(dplyr)

set.seed(1)
regions <- c(
  "China",
  "India",
  "United States",
  "Indonesia",
  "Pakistan",
  "Brazil",
  "Nigeria",
  "Bangladesh",
  "Mexico",
  "Japan"
)

# Make subj
rawplus_subj_s <- clindata::raw_dm %>%
  CreateSUBJ(
    dfIXRSrand = clindata::raw_iwrsrand,
    dfEx = clindata::raw_ex,
    dfVisit = clindata::raw_visdt,
    dfStud = clindata::raw_studcomp,
    dfSdrg = clindata::raw_sdrgcomp,
    dtSnapshot = as.Date("2017-12-27")
  ) %>%
  group_by(SiteID) %>%
  mutate(
    RegionID = sample(regions, 1, replace = TRUE)
  ) %>%
  ungroup() %>%
  arrange(SiteID)

rawplus_subj <- rawplus_subj_s %>%
  filter(
    RandFlag == "Y",
    !is.na(TimeOnTreatment)
  )

usethis::use_data(rawplus_subj_s, overwrite = TRUE)
usethis::use_data(rawplus_subj, overwrite = TRUE)

# Domain Mappings
ids <- rawplus_subj %>% pull(SubjectID)
rawplus_ae <- map_rawplus_ae(clindata::raw_ae, ids)
rawplus_pd <- map_rawplus_pd(clindata::raw_protdev, ids)
rawplus_ie <- map_rawplus_ie(clindata::raw_ie_all, ids)
rawplus_consent <- map_rawplus_consent(clindata::raw_ic_elig, ids)

usethis::use_data(rawplus_ae, overwrite=TRUE)
usethis::use_data(rawplus_pd, overwrite=TRUE)
usethis::use_data(rawplus_ie, overwrite=TRUE)
usethis::use_data(rawplus_consent, overwrite=TRUE)
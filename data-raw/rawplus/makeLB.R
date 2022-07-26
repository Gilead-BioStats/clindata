# Ingest full lab dataset from branch origin/fix-15-labflagging:
# ```{git}
# git checkout origin/fix-15-labflagging data/rawplus_covlab.rda
# ```
#
# Load dataset in R session.
# ```{r}
# load('data/rawplus_covlab.rda')
# ```
#
# Remove dataset from branch.
# ```{git}
# git rm -f data/rawplus_covlab.rda
# ```

library(dplyr)

lb <- rawplus_covlab %>%
  filter(
    SUBJID != "",
    BATTRNAM %in% c('HEMATOLOGY&DIFFERENTIAL PANEL', 'CHEMISTRY PANEL'),
    !is.na(SIRESN)
  ) %>%
  mutate(
    LBCAT = stringr::str_extract(BATTRNAM, '[A-Z]+')
  ) %>%
  select(
    INVID, SUBJID, # participant
    VISIT, VISITNUM, LBDTN, # timing
    LBCAT, LBTEST, LBTESTCD, # measure
    LBSTRESN = SIRESN, LBSTNRLO = SINRLO, LBSTNRHI = SINRHI, LBTOXGR = TOXGR
  ) %>%
  arrange(INVID, SUBJID, VISITNUM, LBTEST)

analysis_flag <- lb %>%
  left_join(
    clindata::rawplus_subj %>%
      select(SiteID, SubjectID, FirstDoseDate, LastDoseDate),
    c("INVID" = "SiteID", "SUBJID" = "SubjectID")
  ) %>%
  mutate(
    LB_TE_FLAG = FirstDoseDate <= LBDTN & LBDTN <= LastDoseDate,
    LBSTNRLO = as.numeric(LBSTNRLO),
    LBSTNRHI = as.numeric(LBSTNRHI),
    LB_GRADE = case_when(
      LBTOXGR != "" ~ as.numeric(LBTOXGR),
      LBSTRESN < LBSTNRLO | LBSTNRHI < LBSTRESN ~ 1,
      LBSTNRLO <= LBSTRESN & LBSTRESN <= LBSTNRHI ~ 0
    ),
    LB_ABN_FLAG = if_else(LB_GRADE > 0, TRUE, FALSE)
  ) %>%
  select(-LBTOXGR, -FirstDoseDate, -LastDoseDate) %>%
  rename(
    SiteID = INVID,
    SubjectID = SUBJID
  )

rawplus_lb <- analysis_flag
usethis::use_data(rawplus_lb, overwrite = TRUE)

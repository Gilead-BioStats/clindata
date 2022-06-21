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
# git rm data/rawplus_covlab.rda
# ```

rawplus_lb <- rawplus_covlab %>%
  filter(
    SUBJID != '',
    !is.na(SIRESN)
  ) %>%
  select(
    INVID, SUBJID, # participant
    VISIT, VISITNUM, LBDTN, # timing
    LBTEST, LBTESTCD, # measure
    LBSTRESN = SIRESN, LBSTNRLO = SINRLO, LBSTNRHI = SINRHI, LBTOXGR = TOXGR
  ) %>%
  arrange(INVID, SUBJID, VISITNUM, LBTEST)

analysis_flag <- rawplus_lb %>%
  left_join(
    clindata::rawplus_subj %>%
      select(SiteID, SubjectID, FirstDoseDate, LastDoseDate),
    c('INVID' = 'SiteID', 'SUBJID' = 'SubjectID')
  ) %>%
  mutate(
    LBSTNRLO = as.numeric(LBSTNRLO),
    LBSTNRHI = as.numeric(LBSTNRHI),
    LB_TE_FLAG = FirstDoseDate <= LBDTN & LBDTN <= LastDoseDate,
    LB_GRADE = case_when(
      LBTOXGR != '' ~ as.numeric(LBTOXGR),
      LBSTRESN < LBSTNRLO | LBSTNRHI < LBSTRESN ~ 1,
      LBSTNRLO <= LBSTRESN & LBSTRESN <= LBSTNRHI ~ 0
    )
  ) %>%
  select(-LBTOXGR, -FirstDoseDate, -LastDoseDate) %>%
  rename(
    SiteID = INVID,
    SubjectID = SUBJID
  )

save(analysis_flag, file = 'data/rawplus_lb.rda')

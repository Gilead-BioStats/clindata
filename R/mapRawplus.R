#' map_rawplus_ae
#'
#' mapping to create rawplus_ae from raw
#'
#' @param  dfRawAE data frame of raw AE data
#'
#' @examples
#'
#' rawplus_ae <- map_rawplus_ae(clindata::raw_ae)
#'
map_rawplus_ae <- function(dfRawAE, ids) {
  rawplus_ae <- dfRawAE %>%
    select(
      SubjectID = SUBJID,
      AE_SERIOUS = AESER
    ) %>%
    mutate(AE_TE_FLAG = sample(c(TRUE, FALSE), n(), replace = TRUE)) %>% # Random TE for now - TODO update
    mutate(AE_GRADE = sample(1:4, n(), replace = TRUE)) %>% # Random Grade for now - TODO update
    filter(
      SubjectID != ""
    )

  if (!is.null(ids)) {
    rawplus_ae <- rawplus_ae %>% filter(SubjectID %in% ids)
  }

  return(rawplus_ae)
}

#' map_rawplus_pd
#'
#' mapping to create rawplus_pd from raw
#'
#' @param  dfRawPD data frame of raw AE data
#'
#' @examples
#'
#' rawplus_pd <- map_rawplus_pd(clindata::raw_protdev)
#'
map_rawplus_pd <- function(dfRawPD, ids) {
  rawplus_pd <- dfRawPD %>%
    mutate(PD_IMPORTANT_FLAG = case_when(
      DEVIMP == "y" ~ "Y",
      DEVIMP == "n" ~ "N",
      TRUE ~ DEVIMP
    )) %>%
    select(
      SubjectID = SUBJID,
      PD_CATEGORY = DEVTYPE,
      PD_IMPORTANT_FLAG,
    ) %>%
    filter(
      SubjectID != ""
    )

  if (!is.null(ids)) {
    rawplus_pd <- rawplus_pd %>% filter(SubjectID %in% ids)
  }

  return(rawplus_pd)
}

#' map_rawplus_ie
#'
#' mapping to create rawplus_ie from raw
#'
#' @param  dfRawIE data frame of raw AE data
#'
#' @examples
#'
#' rawplus_ie <- map_rawplus_ie(clindata::raw_ie_all)
#'
map_rawplus_ie <- function(dfRawIE, ids) {
  rawplus_ie <- dfRawIE %>%
    select(
      SubjectID = SUBJID,
      IE_CATEGORY = IECAT_STD,
      IE_VALUE = IEORRES,
      IE_PROTOCOLVERSION = PROTVER_STD
    ) %>%
    filter(
      SubjectID != ""
    )

  if (!is.null(ids)) {
    rawplus_ie <- rawplus_ie %>% filter(SubjectID %in% ids)
  }

  return(rawplus_ie)
}

#' map_rawplus_consent
#'
#' mapping to create map_rawplus_consent from raw
#'
#' @param  dfRawElig data frame of raw AE data
#'
#' @examples
#'
#' rawplus_consent <- map_rawplus_consent(clindata::raw_ic_elig)
#'
map_rawplus_consent <- function(dfRawElig, ids) {
  rawplus_consent <- dfRawElig %>%
    select(
      SubjectID = SUBJID,
      CONSENT_DATE = DSSTDAT_RAW
    ) %>%
    mutate(
      CONSENT_TYPE = "MAINCONSENT",
      CONSENT_VALUE = "Y",
      CONSENT_DATE = as.Date(CONSENT_DATE, format = "%d %B %Y")
    ) %>%
    filter(
      SubjectID != ""
    )

  if (!is.null(ids)) {
    rawplus_consent <- rawplus_consent %>% filter(SubjectID %in% ids)
  }

  return(rawplus_consent)
}

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

map_rawplus_lb <- function(lb, dm = clindata::rawplus_subj) {
  rawplus_lb <- lb %>%
    filter(
      SUBJID != "",
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
      dm %>%
        select(SiteID, SubjectID, FirstDoseDate, LastDoseDate),
      c(
        "INVID" = "SiteID",
        "SUBJID" = "SubjectID"
      )
    ) %>%
    mutate(
      LBSTNRLO = as.numeric(LBSTNRLO),
      LBSTNRHI = as.numeric(LBSTNRHI),
      LB_TE_FLAG = FirstDoseDate <= LBDTN & LBDTN <= LastDoseDate,
      LB_GRADE = case_when(
        LBTOXGR != "" ~ as.numeric(LBTOXGR),
        LBSTRESN < LBSTNRLO | LBSTNRHI < LBSTRESN ~ 1,
        LBSTNRLO <= LBSTRESN & LBSTRESN <= LBSTNRHI ~ 0
      )
    ) %>%
    select(-LBTOXGR, -FirstDoseDate, -LastDoseDate) %>%
    rename(
      SiteID = INVID,
      SubjectID = SUBJID
    )

  analysis_flag
  # save(analysis_flag, file = 'data/rawplus_lb.rda')
}

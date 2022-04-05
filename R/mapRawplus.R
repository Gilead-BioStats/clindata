#' map_rawplus_ae
#'
#' mapping to create rawplus_ae from raw
#'
#' @param  dfRawAE data frame of raw AE data
#'
#' @examples
#'
#' rawplus_ae <- map_rawplus_ae(clindata::raw_ae)

map_rawplus_ae <- function(dfRawAE){
    rawplus_ae <- dfRawAE %>%
    select(
        SubjectID = SUBJID,
        AE_SERIOUS = AESER
    ) %>%
    mutate(AE_TE_FLAG=sample(c(TRUE,FALSE),n(),replace=TRUE)) %>% # Random TE for now - TODO update
    mutate(AE_GRADE = sample(1:4,n(),replace=TRUE)) %>% # Random Grade for now - TODO update
    filter(
        SubjectID != ""
        )

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

map_rawplus_pd <- function(dfRawAE){
    rawplus_pd <- dfRawAE %>%
    mutate(PD_IMPORTANT_FLAG = case_when(
        DEVIMP == 'y' ~ "Y",
        DEVIMP == 'n' ~ "N",
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

map_rawplus_ie <- function(dfRawIE){
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

map_rawplus_consent <- function(dfRawElig){
    rawplus_consent <- dfRawElig %>%
    select(
        SubjectID=SUBJID,
        CONSENT_DATE = DSSTDAT_RAW) %>%
    mutate(
        CONSENT_TYPE = "MAINCONSENT",
        CONSENT_VALUE = "Y",
        CONSENT_DATE = as.Date(CONSENT_DATE, format = "%d %B %Y")
    ) %>%
    filter(
        SubjectID != ""
        )

    return(rawplus_consent)
}

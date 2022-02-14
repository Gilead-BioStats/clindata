#' Utility Function to Create Raw Data Subject Level (RDSL)
#'
#' Create subject level data mapped from raw data for input
#'
#' @details
#'
#' This output of this function is used by assessments as a standardized measurement of time on treatment.
#'
#' @section Data Specification:
#'
#'
#' The following columns are required:
#' - `dfEX`
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - `EXSTDAT` - Start date, dose date
#'     - `EXENDAT` - Stop date
#'
#' The following columns are optional
#' - `dfSdrg`
#'     - `SUBJID` - Unique subject ID
#'     - `SDRGYN_STD` - Y/N Did subject complete study drug closing
#'
#' #' The following columns are required:
#' - `dfvisit`
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - `FOLDERNAME` - description of event, 'Screening' will not be considered for determining date.
#'     - `RECORDDATE` - Clinical Date of record (ex: visit date), Min and max per subject / site will be used to determine time on study (exposure)
#'
#' The following columns are optional
#' - `dfStud`
#'     - `SUBJID` - Unique subject ID
#'     - `COMPYN_STD` - Y/N Did subject complete duration of study
#'     - `COMPREAS` - character, if COMPYN_STD = N, reason for study discontinue.
#'
#'     -`dfSnapshot` - Date, Date of snapshot
#'
#' @param  dfDm data frame of demographic information with required columns SUBJID INVID
#' @param  dfEx data frame of treatment information with required columns SUBJID INVID EXSTDAT EXENDAT. If
#' multiple treatments and only want to focus on one treatment then input data frame will need to be subset
#' by user prior to input
#' @param  dfSdrg optional, if no data frame is supplied will assume no subject completed treatment, otherwise
#' requires data frame of treatment completion information with columns SUBJID SDRGYN_STD. If multiple treatments
#' and only want to focus on one treatment then input data frame will need to be subset
#' by user prior to input
#' @param  dfVisit data frame of visit information with required columns SUBJID INVID FOLDERNAME RECORDDATE
#' @param  dfStud optional, if no data frame is supplied will assume no subject completed study, otherwise
#' requires data frame of study completion information with columns SUBJID COMPYN_STD COMPREAS
#' @param  dtSnapshot date of data snapshot, if NULL, will impute to be current date
#'
#' @return dataframe with SubjectID, ScreenID, SiteID, ScreenFlag, RandFlag, RandDate, firstStudyDate, lastStudyDate
#' TimeonStudy, firstDoseDate, lastDoseDate, TimeOnTreatment
#'
#' @import dplyr
#' @importFrom lubridate is.Date time_length
#'
#' @examples
#'
#' dfRDSL <- CreateRDSL(dfDm = clindata::raw_dm,
#'                      dfIXRSrand = clindata::raw_iwrsrand,
#'                      dfEx = clindata::raw_ex,
#'                      dfVisit = clindata::raw_visdt,
#'                      dfStud = clindata::raw_studcomp,
#'                      dfSdrg = clindata::raw_sdrgcomp)
#'
#' @export
#'

CreateRDSL <- function(
  dfDm = NULL,
  dfIXRSrand = NULL,
  dfVisit = NULL,
  dfStud = NULL,
  dfEx = NULL,
  dfSdrg = NULL,
  dtSnapshot = NULL
){

  ## Create Study Time Info
  dfToS <- TimeOnStudy(dfVisit = dfVisit, dfStud = dfStud, dtSnapshot = dtSnapshot)
  dfToS <- dfToS %>% rename( firstStudyDate = firstDoseDate,
                             lastStudyDate = lastDoseDate,
                             TimeOnStudy = Exposure)

  ## Create Treatment Exposure Info
  dfTrtEx <- TreatmentExposure(dfEx = dfEx, dfSdrg = dfSdrg, dtSnapshot = dtSnapshot) %>%
    rename( TimeOnTrt = Exposure)

  ## Create Randomization Info
  dfRand <- dfIXRSrand %>% select( SUBJID, INVID, RGMNDTC ) %>%
    mutate( RGMNDTC = lubridate::dmy(RGMNDTC) ) %>%
    rename( SubjectID = SUBJID, SiteID = INVID , RandDate = RGMNDTC)

  ## Source SUBJID SCRNID INVID
  dfSubid <- dfDm %>% select( SUBJID, SCRNID, INVID ) %>%
    mutate( ScreenFlag = if_else(SCRNID != "", "Y", "N") ) %>%
    rename( SubjectID = SUBJID, SiteID = INVID , ScreenID = SCRNID)

  ## Combine
  dfRDSL <- dfSubid %>% left_join( dfRand , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    ## Create Randomization Flag
    mutate( RandFlag = if_else(is.na(RandDate), "N", "Y" ) ) %>%
    left_join( dfToS , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    left_join( dfTrtEx , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    arrange( desc(SubjectID) )

  ### Note: can we create a catch_all for subjects that fall out from left_join?
  missToS <- anti_join( dfToS, dfRDSL, by="SubjectID")
  missTrtEx <- anti_join( dfTrtEx, dfRDSL, by="SubjectID")
  missRand <- anti_join( dfRand, dfRDSL, by="SubjectID")

  if( nrow(missToS) > 0 ) warning("Not all subjects matched for Time on Study from dfVisit dataset")
  if( nrow(missTrtEx) > 0 ) warning("Not all subjects matched for Treatment Exposure from dfEx dataset")
  if( nrow(missRand) > 0 ) warning("Not all subjects matched from dfIXRSrand dataset")

  return( dfRDSL )
}



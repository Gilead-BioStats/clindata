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
#' - `dfDm` - Exposure raw dataset
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - `SCRNID` - Screening ID
#'
#' - `dfEX` - Exposure raw dataset
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - `EXSTDAT` - Start date of dose
#'     - `EXENDAT` - Stop date of dose
#'
#' - `dfIXRSrand` - IXRS randomization dataset
#'     - `SUBJID` - Unique subject ID
#'     - `RGMNDTC` - randomization date
#'
#' - `dfVisit`
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - `FOLDERNAME` - description of event, 'Screening' will not be considered for determining date.
#'     - `RECORDDATE` - Clinical Date of record (ex: visit date), Min and max per subject / site will be used to determine time on study (exposure)
#'
#'
#' The following columns are optional
#' - `dfSdrg`
#'     - `SUBJID` - Unique subject ID
#'     - `SDRGYN_STD` - Y/N Did subject complete study drug closing
#'
#' - `dfStud`
#'     - `SUBJID` - Unique subject ID
#'     - `COMPYN_STD` - Y/N Did subject complete duration of study
#'     - `COMPREAS` - character, if COMPYN_STD = N, reason for study discontinue.
#'
#'     -`dfSnapshot` - Date, Date of snapshot
#'
#' @param  dfDm data frame of demographic information with required columns SUBJID INVID SCRNID
#' @param  dfEx data frame of treatment information with required columns SUBJID INVID EXSTDAT EXENDAT. If
#' multiple treatments and only want to focus on one treatment then input data frame will need to be subset
#' by user prior to input
#' @param  dfSdrg optional, if no data frame is supplied will assume no subject completed treatment, otherwise
#' requires data frame of treatment completion information with columns SUBJID SDRGYN_STD. If multiple treatments
#' and only want to focus on one treatment then input data frame will need to be subset
#' by user prior to input
#' @param  dfIXRSrand randomization dataset with required columns SUBJID RGMNDTC
#' @param  dfVisit data frame of visit information with required columns SUBJID INVID FOLDERNAME RECORDDATE
#' @param  dfStud optional, if no data frame is supplied will assume no subject completed study, otherwise
#' requires data frame of study completion information with columns SUBJID COMPYN_STD COMPREAS
#' @param  dtSnapshot date of data snapshot, if NULL, will impute to be current date
#'
#' @return dataframe with SubjectID, ScreenID, SiteID, ScreenFlag, RandFlag, RandDate, FirstStudyDate, LastStudyDate
#' TimeonStudy, FirstDoseDate, LastDoseDate, TimeOnTreatment
#'
#' @import dplyr
#' @importFrom lubridate is.Date time_length
#'
#' @examples
#'
#' rawplus_SUBJ <- CreateSUBJ(dfDm = clindata::raw_dm,
#'                            dfIXRSrand = clindata::raw_iwrsrand,
#'                            dfEx = clindata::raw_ex,
#'                            dfVisit = clindata::raw_visdt,
#'                            dfStud = clindata::raw_studcomp,
#'                            dfSdrg = clindata::raw_sdrgcomp)
#'
#' @export
#'

CreateSUBJ <- function(
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

  ## Create Treatment Exposure Info
  dfTrtEx <- TreatmentExposure(dfEx = dfEx, dfSdrg = dfSdrg, dtSnapshot = dtSnapshot)

  ## Create Randomization Info
  dfRand <- dfIXRSrand %>% select( .data$SUBJID, .data$INVID, .data$RGMNDTC ) %>%
    mutate( RGMNDTC = lubridate::dmy(.data$RGMNDTC) ) %>%
    rename( SubjectID = .data$SUBJID, SiteID = .data$INVID , RandDate = .data$RGMNDTC)

  ## Source SUBJID SCRNID INVID
  dfSubid <- dfDm %>% select( .data$SUBJID, .data$SCRNID, .data$INVID ) %>%
    mutate( ScreenFlag = if_else(.data$SCRNID != "", "Y", "N") ) %>%
    rename( SubjectID = .data$SUBJID, SiteID = .data$INVID , ScreenID = .data$SCRNID)

  ## Create study d/c reason column
  dfStudStatus <- dfStud %>% select( .data$SUBJID, .data$INVID, .data$COMPYN_STD, .data$COMPREAS )%>%
    rename( SubjectID = .data$SUBJID, SiteID = .data$INVID)

  ## Create treatment d/c reason column
  dfTrtStatus <- dfSdrg %>% select( .data$SUBJID, .data$INVID, .data$SDRGYN_STD, .data$SDRGREAS )  %>%
    rename( SubjectID = .data$SUBJID, SiteID = .data$INVID)

  ## Combine
  rawplus_RDSL <- dfSubid %>% left_join( dfRand , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    ## Create Randomization Flag
    mutate( RandFlag = if_else(is.na(.data$RandDate), "N", "Y" ) ) %>%

    left_join( dfToS , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    left_join( dfTrtEx , by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    left_join( dfStudStatus,by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%
    left_join( dfTrtStatus,by = c("SubjectID"="SubjectID", "SiteID"="SiteID")) %>%

    ## Create treatment status
    mutate(SDRGYN_STD = replace( .data$SDRGYN_STD, is.na(.data$SDRGYN_STD) & !is.na(.data$FirstDoseDate), "O")) %>%

    ## Create study status
    mutate(COMPYN_STD = replace( .data$COMPYN_STD, is.na(.data$COMPYN_STD) & .data$RandFlag == "Y", "O")) %>%

    rename( TrtCompletion = .data$SDRGYN_STD ,
            StudCompletion = .data$COMPYN_STD ,
            TrtDCReason = .data$SDRGREAS ,
            StudDCReason = .data$COMPREAS ) %>%

    arrange( desc(.data$SubjectID) )

  ### Note: can we create a catch_all for subjects that fall out from left_join?
  missToS <- anti_join( dfToS, rawplus_RDSL, by="SubjectID")
  missTrtEx <- anti_join( dfTrtEx, rawplus_RDSL, by="SubjectID")
  missRand <- anti_join( dfRand, rawplus_RDSL, by="SubjectID")
  #missStud <- anti_join( dfStudStatus, dfRDSL, by="SubjectID")
  #missSdrg <- anti_join( dfTrtStatus, dfRDSL, by="SubjectID")

  if( nrow(missToS) > 0 ) warning("Not all subjects matched for Time on Study from dfVisit dataset")
  if( nrow(missTrtEx) > 0 ) warning("Not all subjects matched for Treatment Exposure from dfEx dataset")
  if( nrow(missRand) > 0 ) warning("Not all subjects matched from dfIXRSrand dataset")
  #if( nrow(missStud) > 0 ) warning("Not all subjects matched for Study Completion from dfStud dataset")
  #if( nrow(missSdrg) > 0 ) warning("Not all subjects matched for Treatment Completion from dfSdrg dataset")

  return( rawplus_RDSL )
}



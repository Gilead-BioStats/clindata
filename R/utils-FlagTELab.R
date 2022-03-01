#' Utility Function to Identify Treatment Emergent Lab Abnormalities
#'
#' Creates two columns for the baseline toxicity grade and flagging for treatment emergent lab abnormalities
#'
#' @details
#'
#' baseline toxicity grade is defined as the closest lab measurement to participant's first dose date
#' treatment emergent lab abnormalities are defined as graded abnormalities (tox grade > 0) after the first dose date
#' and wrose than the baseline toxicity grade
#'
#' @section Data Specification:
#'
#'
#' The following datasets and column are required:
#' - `dfLab`
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - TO be completed
#'
#' - `dfRDSL`
#'     - `SUBJID` - Unique subject ID
#'     - `INVID` - Unique Investigator ID
#'     - To be completed
#'
#'
#' @param  dfLab data frame of raw lab information
#' @param  dfRDSL data frame of rawplus subject level derived info
#' @param  strToxVar variable name of toxicity grade in dfLab dataset, default is TOXGRG
#'
#' @return original lab dataset with additional columns for BLTOX (baseline toxicity grade) and TOXFLG (lab abnorm flag)
#'
#' @import dplyr
#'
#' @examples
#'
#' FlagTELab(dfLab = clindata::raw_covlab_hema, dfRDSL = clindata::rawplus_rdsl)
#'
#' @export

FlagTELab <- function( dfLab , dfRDSL , strToxVar = "TOXGRG" ){

  # Remove missing tox grade and send warning
  if( any(dfLab$TOXGRG != "") ){
    warning( paste(sum(dfLab$TOXGRG != ""), "lab records do not have toxicity grades and will be excluded" ) )
    dfLab2 <- dfLab %>% filter( TOXGRG != "" )
  }

  # Convert tox grading variable into numeric
  dfLab2$TOXGRG <- as.numeric(dfLab2$TOXGRG)


  # Create Flagging by Subject and Lab Test
  vSubjectIndex <- unique(dfRDSL$SubjectID)

  dfLabAbnorm <- NULL

  # By-subject
  for( p in vSubjectIndex ){
    # Create tmeporary dataset
    dfTmp <- data.frame( dfLab2[ dfLab2$SUBJID == p , ] )
    strFDD <- dfRDSL$FirstDoseDate[ dfRDSL$SubjectID == p ]

    # By observed labs
    vLabIndex <- unique( dfTmp$LBTEST )

    # Check if any labs reported with tox grade - otherwise skip
    if( length(vLabIndex) == 0 |
        all( is.na( select(dfTmp , strToxVar) ) ) ) {
      next
    }

    # Need to figure out the period of each lab abnormality reported
    for( m in vLabIndex){

      dfTest <- dfTmp[ dfTmp$LBTEST == m , ]
      dfTest <- dfTest[ order(dfTest$LBDTM, decreasing=FALSE) , ]
      dfTest <- dfTest[ !is.na( dfTest[,strToxVar]) , ]

      if( nrow(dfTest) == 0 ) next

      if( is.na(strFDD) ) next ### do not include missing FDD

      if( sum(dfTest$LBDTM <= strFDD) == 0){ # If no baseline measurements then put NA for baseline
        nToxBL <- NA
      } else {
        nToxBL <- max( dfTest[ dfTest$LBDTM <= strFDD , strToxVar ] )
      }

      if( sum(dfTest$LBDTM > strFDD) == 0){ # If no post-baseline measurements then put NA for postBL
        nToxPostBL <- NA
        vMaxToxDate <- dfTest[ tail(which(dfTest[,strToxVar] ==
                                            nToxBL ),1) , ]
      } else {
        nToxPostBL <- max( dfTest[ dfTest$LBDTM > strFDD , strToxVar ] )
        vMaxToxDate <- dfTest[ which(dfTest[,strToxVar] ==
                                       nToxPostBL )[1] , ] ## First date with max value
      }

      vMaxToxDate <- data.frame( vMaxToxDate ,
                                 BLTOX = as.numeric(nToxBL) )

      if( is.null(dfLabAbnorm) ) {

        dfLabAbnorm <- vMaxToxDate

      } else {

        dfLabAbnorm <- dfLabAbnorm %>% dplyr::bind_rows( vMaxToxDate )

      }
    }
  }

  dfLabAbnorm$TOXFLG <- 0
  dfLabAbnorm$TOXFLG[ which(
    (dfLabAbnorm[,strToxVar] > 0 & is.na(dfLabAbnorm$BLTOX)) |
      (dfLabAbnorm[,strToxVar] > dfLabAbnorm$BLTOX) )] <- 1
  dfLabAbnorm <- dfLabAbnorm[,c("SUBJID","ACCSNNUM","LBTEST","LBDTM","BLTOX","TOXFLG")]

  dfLab <- dfLab %>% left_join( dfLabAbnorm, by = c("SUBJID" = "SUBJID",
                                                    "ACCSNNUM" = "ACCSNNUM",
                                                    "LBDTM" = "LBDTM",
                                                    "LBTEST" = "LBTEST"))

  return( dfLab )

}

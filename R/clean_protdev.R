#' Add `subjectid` to `protdev`
#'
#' Protocol deviations come from a different data source than EDC so a unique subject identifier
#' needs to be defined to merge with EDC.
#'
#' @param protdev `data.frame` protocol deviation data in rawplus format
#' @param dm `data.frame` subject-level data in rawplus format
#'
#' @importFrom dplyr inner_join select
#'
#' @export

clean_protdev <- function(protdev, dm) {
    protdev %>%
        dplyr::inner_join(
            dm %>%
                dplyr::select(
                    invid = invid_nsv, # dm.invid_nsv matches protdev.invid
                    scrnid,
                    subjid,
                    subjectid
                ),
            c('invid', 'scrnid', 'subjid')
        )
}

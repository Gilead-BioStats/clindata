#' Add `subjectid` to `protdev`
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
                    invid = invid_nsv,
                    scrnid,
                    subjid,
                    subjectid
                ),
            c('invid', 'scrnid', 'subjid')
        )
}

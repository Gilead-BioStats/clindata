#' @export
map_gsm_domains <- function(data) {
    mapping <- c(
        'site' = 'dfSITE',
        'dm' = 'dfSUBJ',
        'ae' = 'dfAE',
        'protdev' = 'dfPD',
        'lb' = 'dfLB',
        'studcomp' = 'dfSTUDCOMP',
        'sdrgcomp' = 'dfSDRGCOMP',
        'queries' = 'dfQUERY',
        'data_entry_lag' = 'dfDATAENT',
        'data_change_rate' = 'dfDATACHG',
        'enroll' = 'dfENROLL'
    )

    return(mapping[names(data)])
}

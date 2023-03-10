#' @export
map_gsm_domains <- function(data) {
  mapping <- c(
    # ctms
    "site" = "dfSITE",

    # clinical
    "dm" = "dfSUBJ",
    "ae" = "dfAE",
    "protdev" = "dfPD",
    "lb" = "dfLB",
    "studcomp" = "dfSTUDCOMP",
    "sdrgcomp" = "dfSDRGCOMP",
    "enroll" = "dfENROLL",

    # edc
    "data_points" = "dfDATACHG",
    "data_pages" = "dfDATAENT",
    "queries" = "dfQUERY"
  )

  return(mapping[names(data)])
}

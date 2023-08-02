#' Sample Date
#'
#' @importFrom dplyr if_else
#' @importFrom lubridate as_date is.Date
#'
#' @keywords internal
sample_date <- function(date1, date2) {
    if_else(
        date1 == date2,
        as.integer(date1),
        sample(date1:date2, 1)
    ) %>% lubridate::as_date()

    #if (!lubridate::is.Date(date1))
    #    date1 <- lubridate::as_date(date1)

    #if (!is.integer(date1))
    #    date1 <- as.integer(date1)

    #if (!lubridate::is.Date(date2))
    #    date2 <- lubridate::as_date(date2)

    #if (!is.integer(date2))
    #    date2 <- as.integer(date2)

    #date <- if_else(
    #    date1 == date2,
    #    date1,
    #    sample(date1:date2, 1)
    #) %>% lubridate::as_date()

    #return(date)
}

#' Map {gsm} Domains
#'
#' @param data data to be mapped to domain
#'
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



#' Impute Date
#'
#' @param date The date to be converted
#'
#' @importFrom lubridate is.Date ymd
#'
#' @export
impute_date <- function(date) {
    if (lubridate::is.Date(date)) {
        return(date)
    }

    # no year-month-day
    no_ymd <- which(date == "-----")
    date[no_ymd] <- NA_character_

    # no month-day
    no_md <- which(grepl("^\\d{4}----$", date))
    date[no_md] <- sub("----$", "-01-01", date[no_md])

    # no day
    no_d <- which(grepl("^\\d{4}-\\d{2}--$", date))
    date[no_d] <- sub("--$", "-01", date[no_d])

    lubridate::ymd(date)
}

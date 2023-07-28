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

    # numeric
    num_d <- which(!grepl("\\-", date))
    date[num_d] <- as.character(as_date(as.numeric(date[num_d])))

    lubridate::ymd(date)
}

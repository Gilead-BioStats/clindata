#' @importFrom dplyr if_else
#' @importFrom lubridate as_date is.Date
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

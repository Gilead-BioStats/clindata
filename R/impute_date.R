impute_date <- function(date) {
  # no year-month-day
  no_ymd <- which(date == '-----')
  date[no_ymd] <- NA_character_

  # no month-day
  no_md <- which(grepl('^\\d{4}----$', date))
  date[no_md] <- sub('----$', '-01-01', date[no_md])

  # no day
  no_d <- which(grepl('^\\d{4}-\\d{2}--$', date))
  date[no_d] <- sub('--$', '-01', date[no_d])

  lubridate::ymd(date)
}

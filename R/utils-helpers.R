#' Update {gsm} version in config files.
#'
#' @param version `character` {gsm} version number.
#'
#' @examples
#' \dontrun{
#' # specify release
#' update_gsm_version(gsm_version = "1.5.0")
#'
#' # latest release via GitHub API
#' update_gsm_version()
#' }
#'
#' @return Updated .rda and .csv files.
#'
#' @noRd
#'
#' @keywords internal

update_gsm_version <- function(version = 'latest') {
    # import
    datasets <- ctms_1_import(data_path = file.path('data-raw', 'config'))

    if (version == 'latest') {
        gsm_releases <- gh::gh(
            '/repos/Gilead-BioStats/gsm/releases',
            token = remotes:::github_pat()
        )
        gsm_version <- gsm_releases[[1]]$name %>% substring(2) %T>% message

    } else {
        gsm_version <- version
    }

    datasets$param$gsm_version <- as.character(gsm_version)
    datasets$workflow$gsm_version <- as.character(gsm_version)


    # process
    datasets_processed <- ctms_2_process(datasets)

    # export
    ctms_3_export(datasets_processed, data_domain = "config")

    # document
    ctms_4_document(
        datasets_processed,
        domain_metadata_path = system.file('data-standards', 'config.yaml', package = 'clindata'),
        documentation_path = paste0(system.file('R', package = 'clindata'), '/config.R'),
        data_domain = "config"
    )

    config_files <- c("param.csv", "workflow.csv")

    update <- purrr::map(config_files, ~read.csv(here::here('data-raw', 'config', .))) %>%
        purrr::map(~.x %>% mutate(gsm_version = version)) %>%
        purrr::set_names(config_files)

    purrr::iwalk(update, function(x, y) {write.csv(x, file = paste0(here::here('data-raw', 'config', y)), row.names = FALSE)})
}


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

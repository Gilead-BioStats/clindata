#' Simulate study data
#'
#' Simulate study data based on desired number of sites, number of subjects, and study duration.
#'
#' @param n_sites `integer` Number of sites
#' @param n_subjects `integer` Number of subjects to be divided among sites
#' @param start_date `Date` Start date of study
#' @param end_date `Date` End date of study
#' @param rename_gsm `logical` Apply {gsm} domain names?
#'
#' @return `list` Named list with simulated study data.
#'
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map_int
#'
#' @export
simulate_study <- function(
  n_sites = NULL,
  n_subjects = NULL,
  start_date = NULL,
  end_date = NULL,
  # duration = NULL,
  rename_gsm = FALSE
) {

  if (is.null(n_sites)) {
    n_sites <- nrow(clindata::ctms_site)
  }

  if (is.null(n_subjects)) {
    n_subjects <- nrow(clindata::rawplus_dm)
  }

  # TODO: add logic around start date, end date, and duration
  if (is.null(start_date)) {
    start_date <- as.Date(min(clindata::rawplus_dm$firstparticipantdate, na.rm = TRUE))
    message('> Start date')
    print(start_date)
  }

  if (is.null(end_date)) {
    end_date <- as.Date(max(clindata::rawplus_dm$lastparticipantdate, na.rm = TRUE))
    message('> End date')
    print(end_date)
  }

  # if (is.null(duration)) {
  #    duration <- as.numeric(
  #        as.Date(end_date) - as.Date(start_date)
  #    ) + 1
  #    message('> Duration')
  #    print(duration)
  # }

  studyid <- glue::glue("study-{n_sites}_sites-{n_subjects}_subjects") %>%
    as.character()

  # sites
  site <- simulate_site(
    n_sites,
    studyid
  )

  # subjects
  dm <- simulate_dm(
    site,
    n_subjects,
    start_date,
    end_date
  )

  # rawplus
  ae <- simulate_ae(dm)
  protdev <- simulate_protdev(dm)
  lb <- simulate_lb(dm)
  studcomp <- simulate_studcomp(dm)
  sdrgcomp <- simulate_sdrgcomp(dm)
  enroll <- simulate_enroll(dm)

  # edc
  queries <- simulate_queries(dm)
  data_pages <- simulate_data_pages(dm)

  # list of data domains
  data <- list(
    site = site,
    dm = dm,
    ae = ae,
    protdev = protdev,
    lb = lb,
    studcomp = studcomp,
    sdrgcomp = sdrgcomp,
    data_points = data_points,
    data_pages = data_pages,
    queries = queries,
    enroll = enroll
  )

  message('--> Rows per domain')
  cat('\n')
  data %>%
    purrr::map_int(~ nrow(.x)) %>%
    stack() %>%
    dplyr::select(
      domain = ind,
      n_rows = values
    ) %>%
    print(row.names = FALSE)
  cat('\n')

  if (rename_gsm) {
    names(data) <- map_gsm_domains(data)
  }

  return(data)
}

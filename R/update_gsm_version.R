#' Update {gsm} version in config files.
#'
#' @param gsm_version
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

update_gsm_version <- function(gsm_version = 'latest') {
    # import
    datasets <- ctms_1_import(data_path = file.path('data-raw', 'config'))

    if (gsm_version == 'latest') {
        gsm_releases <- gh::gh(
            '/repos/Gilead-BioStats/gsm/releases',
            token = remotes:::github_pat()
        )
        gsm_version <- gsm_releases[[1]]$name %>% substring(2) %T>% message

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
        purrr::map(function(x) {
            x %>% mutate(gsm_version = gsm_version)
        }) %>%
        purrr::set_names(config_files)

    purrr::iwalk(update, function(x, y) {write.csv(x, file = paste0(here::here('data-raw', 'config', y)), row.names = FALSE)})
}

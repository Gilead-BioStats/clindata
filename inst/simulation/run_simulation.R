#' @importFrom data.table fwrite
#' @importFrom glue glue
#' @importFrom lubridate year
#' @importFrom purrr iwalk
#' @importFrom stringr str_pad
#' @importFrom tictoc tic toc
#' @importFrom gsm MakeWorkflowList Make_Snapshot
#'
#' @export
run_simulation <- function(
  n_sites,
  n_subjects,
  n_snapshots = 6,
  end_date = lubridate::today(),
  start_date = NULL,
  out_path = 'simulation',
  print_check_rows = FALSE
) {
  message(glue::glue(
    'Simulating [ {sprintf("%3d", n_sites)} ] sites - [ {sprintf("%4d", n_subjects)} ] subjects'
  ))
  cat('\n')


  if (is.null(start_date)) {
    start_date <- end_date
    lubridate::year(start_date) <- lubridate::year(start_date) - 2
  }

  if (!file.exists(out_path)) {
    dir.create(out_path)
  }

  metadata <- list(
    config_param = clindata::config_param,
    config_workflow = clindata::config_workflow,
    meta_params = gsm::meta_param,
    meta_site = clindata::ctms_site,
    meta_study = clindata::ctms_study,
    meta_workflow = gsm::meta_workflow
  )

  workflows <- gsm::MakeWorkflowList()

  tictoc::tic(stringr::str_pad("simulate data", 17))
  data <- simulate_study(
    n_sites,
    n_subjects,
    start_date = start_date,
    end_date = end_date
  )
  tictoc::toc()

  studyid <- unique(data$site$protocol)
  metadata$meta_study$protocol_number <- studyid
  metadata$meta_study$num_plan_site <- n_sites
  metadata$meta_study$num_plan_subj <- n_subjects
  metadata$config_param$studyid <- studyid
  metadata$config_workflow$studyid <- studyid

  study_path <- glue::glue("{out_path}/{studyid}")
  if (!file.exists(study_path)) {
    dir.create(study_path)
  }

  for (i in (1:n_snapshots) - 1) {
    snapshot_date <- end_date
    lubridate::month(snapshot_date) <- lubridate::month(snapshot_date) - i

    snapshot_path <- glue::glue("{study_path}/{snapshot_date}")
    if (!file.exists(snapshot_path)) {
      dir.create(snapshot_path)
    }
    message(paste0('\n--> Output path: ', snapshot_path))
    cat('\n')

    tictoc::tic(stringr::str_pad("snapshot data", 17))
    data_snapshot <- snapshot_all(
      snapshot_date,
      data,
      impute_rf_dt = FALSE,
      print_check_rows = print_check_rows
    )
    tictoc::toc()

    # apply snapshot to study data
    meta_study <- snapshot_study(snapshot_date, data_snapshot$dfSUBJ, metadata$meta_study, print_check_rows = print_check_rows)
    metadata$meta_study <- meta_study

    # apply snapshot to site data
    meta_site <- snapshot_site(snapshot_date, data_snapshot$dfSUBJ, data$site, print_check_rows = print_check_rows)
    metadata$meta_site <- meta_site

    ## randomly tweak thresholds
    # thresholds <- metadata$config_param %>%
    #    filter(
    #        param == 'vThreshold'
    #    ) %>%
    #    mutate(
    #        new_threshold = as.numeric(value) +
    #            as.numeric(value) * .1 * sample(c(-1,1), n(), TRUE),
    #        value = if_else(
    #            runif(n()) > .05,
    #            as.numeric(value),
    #            round(new_threshold, 1)
    #        ) %>% as.character
    #    )

    # metadata$config_param <- metadata$config_param %>%
    #    left_join(
    #        thresholds,
    #        c('studyid', 'workflowid', 'gsm_version', 'param', 'index')
    #    ) %>%
    #    mutate(
    #        value = coalesce(value.y, value.x)
    #    ) %>%
    #    select(-value.x, -value.y)

    tictoc::tic(stringr::str_pad("run gsm", 17))
    gsm_output <- gsm::Make_Snapshot(
      lMeta = metadata,
      lData = data_snapshot,
      lAssessments = workflows,
      bUpdateParams = TRUE,
      bQuiet = TRUE
    )
    tictoc::toc()

    tictoc::tic(stringr::str_pad("output data files", 17))
    gsm_output %>%
      purrr::iwalk(function(value, key) {
        # value$gsm_analysis_date <- format(
        #    as.Date(snapshot_date),
        #    '%Y-%m-%d %H:%M:%S'
        # )
        value$gsm_analysis_date <- snapshot_date

        data.table::fwrite(
          value,
          paste0(snapshot_path, "/", key, ".csv")
        )
      })
    tictoc::toc()
  }

  return(data)
}

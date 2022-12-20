library(magrittr)
devtools::load_all()

# import
datasets <- ctms_1_import(data_path = file.path('data-raw', 'config'))

gsm_releases <- gh::gh(
    '/repos/Gilead-BioStats/gsm/releases',
    token = remotes:::github_pat()
)
gsm_version <- gsm_releases[[1]]$name %>% substring(2) %T>% message

datasets$param$gsm_version <- gsm_version
datasets$workflow$gsm_version <- gsm_version

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

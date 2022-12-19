devtools::load_all()

datasets <- ctms_1_import(data_path = file.path('data-raw', 'config'))

# update description ------------------------------------------------------
# Note: packageVersion() creates a class "package_version" that causes issues when joining/merging data
# easier for us to retain as a character value

datasets$param$gsm_version <- "1.4.1"
datasets$workflow$gsm_version <- "1.4.1"

datasets_processed <- ctms_2_process(datasets)
ctms_3_export(datasets_processed, data_domain = "config")
ctms_4_document(datasets_processed,
  domain_metadata_path = system.file('data-standards', 'config.yaml', package = 'clindata'),
  documentation_path = paste0(system.file('R', package = 'clindata'), '/config.R'),
  data_domain = "config")

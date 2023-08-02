#' Export edc data to .rda files.
#'
#' @importFrom purrr map_int iwalk
#'
#' @keywords internal
export_edc <- function() {

    metadata <- yaml::read_yaml(
        system.file('data-standards', 'edc.yaml', package = 'clindata')
    )

    # Run each simulation script.
    metadata %>%
        .[order(purrr::map_int(., 'order'))] %>%
        purrr::iwalk(function(value, key) {
            print(key)
            source(value$script)
        })

    datasets <- edc_1_import()
    datasets_processed <- edc_2_process(datasets)
    edc_3_export(datasets_processed)
    edc_4_document(datasets_processed)


}

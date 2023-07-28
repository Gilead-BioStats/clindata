#' Export ctms data to .rda files.
#'
#' @keywords internal
export_ctms <- function() {

    datasets <- ctms_1_import()
    datasets_processed <- ctms_2_process(datasets)
    ctms_3_export(datasets_processed)
    ctms_4_document(datasets_processed)

}

#' @importFrom cli cli_alert_success
#'
#' @export
check_rows <- function(df1, df2, domain = NULL) {
  n_rows <- nrow(df1) - nrow(df2)

  if (is.null(domain)) {
    cli::cli_alert_success(
      "{n_rows} rows removed."
    )
  } else {
    cli::cli_alert_success(
      "{n_rows} rows removed from [ {domain} ]."
    )
  }
}

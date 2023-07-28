#' @import dplyr
#' @importFrom glue glue
#'
#' @export
simulate_site <- function(
  n_sites,
  studyid,
  site = clindata::ctms_site
) {
  if (!is.null(n_sites)) {
    site1 <- site %>%
      slice_sample(
        n = n_sites,
        replace = TRUE
      ) %>%
      group_by(.data$site_num) %>%
      mutate(
        site_num = glue::glue("{.data$site_num}-{row_number()}") %>%
          as.character()
      ) %>%
      ungroup()
  } else {
    site1 <- site
  }

  site1$protocol <- studyid

  return(site1)
}

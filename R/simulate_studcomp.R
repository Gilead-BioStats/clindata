#' @import dplyr
#'
#' @export
simulate_studcomp <- function(
  dm,
  studcomp = clindata::rawplus_studcomp,
  disc_rate = runif(1, 0.001, .25)
) {
  compreas <- studcomp$compreas %>% .[. != ""]
  studcomp1 <- dm %>%
    filter(timeonstudy > 0) %>%
    select(subjid) %>%
    slice_sample(
      n = ceiling(disc_rate * nrow(.))
    ) %>%
    mutate(
      compyn = "N",
      compreas = sample(
        unique(compreas),
        n(),
        replace = TRUE,
        prob = table(compreas) / length(compreas)
      )
    )

  return(studcomp1)
}

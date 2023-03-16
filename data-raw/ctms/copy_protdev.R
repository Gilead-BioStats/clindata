library(arrow)
library(dplyr)

protdev <- arrow::read_parquet('data-raw/ctms/protdev.parquet') %>%
    filter(
        subjid != ''
    ) %>%
    dplyr::mutate(
        importnt = case_when(
            importnt == 'Y' ~ 'Yes',
            importnt == 'N' ~ 'No'
        )
    ) %>%
    dplyr::select(
        SubjectEnrollmentNumber = subjid,
        DeviationDate = dv_dt,
        GileadCategory = dvdecod,
        DeemedImportant = importnt
    )

jsonlite::write_json(
    protdev,
    paste0(here::here('data-raw', 'ctms'), '/protdev.json'),
    pretty = 4
)

write.csv(
    protdev,
    paste0(here::here('data-raw', 'ctms'), '/protdev.csv'),
    row.names = FALSE
)

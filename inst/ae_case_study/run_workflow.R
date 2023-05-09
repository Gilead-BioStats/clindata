
# setup -------------------------------------------------------------------

start_date <- '2010-06-01'
end_date <- '2010-12-01'

## read data
dfAE <- readRDS(system.file("ae_case_study", "rawplus_ae_demo.Rda", package = "clindata"))
dfSUBJ <- readRDS(system.file("ae_case_study", "rawplus_dm_demo.Rda", package = "clindata"))
workflows <- list(kri0001 = gsm::MakeWorkflowList()$kri0001)

## choose snapshot dates
snapshot_dates <- seq.Date(as.Date(start_date), as.Date(end_date), 'month')

for (snapshot_date in as.character(snapshot_dates)) {
    print(snapshot_date)

    out_path <- glue::glue("{system.file('ae_case_study', 'snapshots', package = 'gsm')}/{as.character(snapshot_date)}")
    if (!file.exists(out_path))
        dir.create(out_path)

    dm <- dfSUBJ %>%
        mutate(record_dt = lubridate::as_date(record_dt)) %>%
        filter(record_dt <= snapshot_date)

    ae <- dfAE %>%
        dplyr::left_join(
            dm %>% dplyr::select(subjid),
            "subjid"
        ) %>%
        dplyr::mutate(
            aeen_dt = dplyr::if_else(
                impute_date(aeen_dt) > snapshot_date,
                as.character(snapshot_date),
                as.character(aeen_dt)
            )
        ) %>%
        dplyr::filter(
            impute_date(aest_dt) <= snapshot_date
        )

    if (snapshot_date == "2010-06-01") {
        ae <- subset_aes(ae, 120)
        dm <- subset_aes(dm, 120)
    }

    if (snapshot_date == "2010-07-01") {
        ae <- subset_aes(ae, 160)
        dm <- subset_aes(dm, 160)
    }

    if (snapshot_date == "2010-08-01") {
        ae <- subset_aes(ae, 194)
        dm <- subset_aes(dm, 194)
    }

    if (snapshot_date == "2010-09-01") {
        ae <- subset_aes(ae, 240)
        dm <- subset_aes(dm, 240)
    }

    snapshot <- Make_Snapshot(
        lData = list(dfAE = ae, dfSUBJ = dm),
        lAssessments = workflows,
        bQuiet = TRUE,
        strAnalysisDate = snapshot_date
    )

    Save_Snapshot(snapshot, out_path)
}

snapshot <- Make_Snapshot(lData = list(dfAE = rawplus_ae_demo, dfSUBJ = dfSUBJ))
augment <- Augment_Snapshot(snapshot, here::here('inst', 'ae_case_study', 'snapshots'))
Study_Report(augment$lStudyAssessResults, augment$lSnapshot$status_study)

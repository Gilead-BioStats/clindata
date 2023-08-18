#' Snapshot AE domain.
#'
#' @param snapshot_date `Date` Date at which to snapshot data.
#' @param dm `data.frame` Demographic data.
#' @param ae `data.frame` Adverse event data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter if_else left_join select
#'
#' @export
snapshot_ae <- function(snapshot_date, dm, ae = clindata::rawplus_ae, print_check_rows = TRUE) {
    ae_snapshot <- ae %>%
        dplyr::left_join(
            dm %>% dplyr::select(
                "subjid",
                "lastparticipantdate",
                "lastdosedate"
                ),
            "subjid"
        ) %>%
        dplyr::mutate(
            aeen_dt = dplyr::if_else(
                impute_date(.data$aeen_dt) > snapshot_date,
                as.character(snapshot_date),
                as.character(.data$aeen_dt)
            )
        ) %>%
        dplyr::filter(
            impute_date(.data$aest_dt) <= snapshot_date
        ) %>%
        dplyr::select(
            -c(
                "lastparticipantdate",
                "lastdosedate"
            )
        )

    if (print_check_rows) {
        check_rows(ae, ae_snapshot, "ae")
    }

    ae_snapshot
}


#' Snapshot all data domains at a specific point in time.
#'
#' @param snapshot_date `Date` Date at which to snapshot data `Date` Date at which to snapshot data
#' @param data `list` Name list of data domains to snapshot
#' @param impute_rf_dt `logical` Imput reference dates with `visdt` and `ex`?
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`
#' @param rename_gsm `logical` replace data names with names in domain?
#'
#' @return `list` Named list of modified data domains
#'
#' @export
# TODO: add consent and IE back in
snapshot_all <- function(
        snapshot_date = get_snapshot_date(),
        data = list(
            # rawplus
            visdt = clindata::rawplus_visdt,
            ex = clindata::rawplus_ex,
            dm = clindata::rawplus_dm,
            # consent = clindata::rawplus_consent,
            # ie = clindata::rawplus_ie,
            studcomp = clindata::rawplus_studcomp,
            sdrgcomp = clindata::rawplus_sdrgcomp,
            lb = clindata::rawplus_lb,
            ae = clindata::rawplus_ae,
            protdev = clindata::ctms_protdev,
            enroll = clindata::rawplus_enroll,

            # edc
            queries = clindata::edc_queries,
            data_pages = clindata::edc_data_pages,
            data_points = clindata::edc_data_points
        ),
        impute_rf_dt = TRUE,
        print_check_rows = TRUE,
        rename_gsm = TRUE
) {
    # rawplus
    if (impute_rf_dt) {
        visdt <- snapshot_visdt(snapshot_date, data$visdt, print_check_rows = print_check_rows)
        ex <- snapshot_ex(snapshot_date, data$ex, print_check_rows = print_check_rows)
        dm <- snapshot_dm(snapshot_date, visdt, ex, data$dm, print_check_rows = print_check_rows)
    } else {
        dm <- snapshot_dm(snapshot_date, dm = data$dm, print_check_rows = print_check_rows)
    }

    # consent <- snapshot_consent(snapshot_date, dm, data$consent, print_check_rows = print_check_rows)
    # ie <- snapshot_ie(snapshot_date, dm, data$ie, print_check_rows = print_check_rows)
    studcomp <- snapshot_studcomp(snapshot_date, dm, data$studcomp, print_check_rows = print_check_rows)
    sdrgcomp <- snapshot_sdrgcomp(snapshot_date, dm, data$sdrgcomp, print_check_rows = print_check_rows)
    lb <- snapshot_lb(snapshot_date, dm, data$lb, print_check_rows = print_check_rows)
    ae <- snapshot_ae(snapshot_date, dm, data$ae, print_check_rows = print_check_rows)
    protdev <- snapshot_protdev(snapshot_date, dm, data$protdev, print_check_rows = print_check_rows)
    enroll <- snapshot_enroll(snapshot_date, data$enroll, print_check_rows = print_check_rows)

    # edc
    queries <- snapshot_queries(snapshot_date, data$queries, print_check_rows = print_check_rows)
    data_pages <- snapshot_data_pages(snapshot_date, data$data_pages, print_check_rows = print_check_rows)
    data_points <- snapshot_data_points(snapshot_date, data$data_points, print_check_rows = print_check_rows)

    data <- list(
        snapshot_date = snapshot_date,

        # rawplus
        dm = dm,
        # consent = consent,
        # ie = ie,
        studcomp = studcomp,
        sdrgcomp = sdrgcomp,
        lb = lb,
        ae = ae,
        protdev = protdev,
        enroll = enroll,

        # edc
        queries = queries,
        data_pages = data_pages,
        data_points = data_points
    )

    if (rename_gsm) {
        names(data) <- map_gsm_domains(data)
    }

    return(data)
}



#' Snapshot Consent Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#'
#' @param dm `data.frame` Demographic data. `data.frame` Demographic data.
#' @param consent `data.frame` Consent data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`. `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_consent <- function(snapshot_date, dm, consent = clindata::rawplus_consent, print_check_rows = TRUE) {
    consent_snapshot <- consent %>%
        dplyr::filter(
            .data$subjid %in% dm$subjid
        )

    if (print_check_rows) {
        check_rows(consent, consent_snapshot, "consent")
    }

    consent_snapshot
}


#' Snapshot Data Pages Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param data_pages `data.frame` EDC data page data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_data_pages <- function(snapshot_date, data_pages = clindata::edc_data_pages, print_check_rows = TRUE) {

    data_pages_snapshot <- data_pages %>%
        dplyr::filter(
            impute_date(.data$visit_date) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(data_pages, data_pages_snapshot, "data_pages")
    }

    data_pages_snapshot
}


#' Snapshot Data Points Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param data_points `data.frame` EDC Data Points data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_data_points <- function(snapshot_date, data_points = clindata::edc_data_points, print_check_rows = TRUE) {
    data_points_snapshot <- data_points %>%
        dplyr::filter(
            impute_date(.data$visit_date) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(data_points, data_points_snapshot, "data_points")
    }

    data_points_snapshot
}


#' Snapshot Demographic Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param visdt `Date` Visit date.
#' @param ex `data.frame` #TODO: complete documentation.
#' @param dm `data.frame` Demographic data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @import dplyr
#'
#' @export
snapshot_dm <- function(snapshot_date, visdt = NULL, ex = NULL, dm = clindata::rawplus_dm, print_check_rows = TRUE) {
    if (!is.null(visdt) & !is.null(ex)) {
        lastparticipantdate <- visdt %>%
            dplyr::group_by(.data$subjid) %>%
            dplyr::summarize(
                lastparticipantdate = max(impute_date(.data$visit_dt))
            )

        lastdosedate <- ex %>%
            dplyr::group_by(.data$subjid) %>%
            dplyr::summarize(
                lastdosedate = max(impute_date(.data$exen_dt))
            )

        dm_snapshot <- dm %>%
            dplyr::filter(
                impute_date(.data$firstparticipantdate) <= snapshot_date
            ) %>%
            dplyr::rename(
                lastparticipantdate0 = lastparticipantdate,
                lastdosedate0 = lastdosedate
            ) %>%
            dplyr::left_join(
                lastparticipantdate,
                "subjid"
            ) %>%
            dplyr::left_join(
                lastdosedate,
                "subjid"
            )
    } else {
        dm_snapshot <- dm %>%
            dplyr::filter(
                impute_date(.data$firstparticipantdate) <= snapshot_date
            ) %>%
            dplyr::rename(
                lastparticipantdate0 = lastparticipantdate,
                lastdosedate0 = lastdosedate
            ) %>%
            dplyr::mutate(
                lastparticipantdate = snapshot_date,
                lastdosedate = snapshot_date
            )
    }

    if (print_check_rows) {
        check_rows(dm, dm_snapshot, "dm")
    }

    dm_snapshot
}


#' Snapshot Enroll Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param enroll `data.frame` Enrollment data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_enroll <- function(snapshot_date, enroll = clindata::rawplus_enroll, print_check_rows = TRUE) {
    enroll_snapshot <- enroll %>%
        dplyr::filter(
            impute_date(.data$enroll_dt) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(enroll, enroll_snapshot, "enroll")
    }

    enroll_snapshot
}


#' Snapshot EX Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param ex `data.frame` data
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter if_else mutate
#'
#' @export
snapshot_ex <- function(snapshot_date, ex = clindata::rawplus_ex, print_check_rows = TRUE) {
    ex_snapshot <- ex %>%
        dplyr::filter(
            impute_date(.data$exst_dt) <= snapshot_date
        ) %>%
        dplyr::mutate(
            exen_dt = dplyr::if_else(
                impute_date(.data$exen_dt) > snapshot_date,
                as.character(snapshot_date),
                .data$exen_dt
            )
        )

    if (print_check_rows) {
        check_rows(ex, ex_snapshot, "ex")
    }

    ex_snapshot
}


#' Snapshot Inclusion/Exclusion Domain.
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param ie `data.frame` Inclusion/exclusion data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_ie <- function(snapshot_date, dm, ie = clindata::rawplus_ie, print_check_rows = TRUE) {
    ie_snapshot <- ie %>%
        dplyr::filter(
            .data$subjid %in% dm$subjid
        )

    if (print_check_rows) {
        check_rows(ie, ie_snapshot, "ie")
    }

    ie_snapshot
}


#' Snapshot Labs Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param lb `data.frame` Lab values data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_lb <- function(snapshot_date, dm, lb = clindata::rawplus_lb, print_check_rows = TRUE) {
    if (is.character(lb$lb_dt)) {
        lb$lb_dt <- substring(lb$lb_dt, 1, 10)
    }

    lb_snapshot <- lb %>%
        dplyr::filter(
            .data$subjid %in% dm$subjid,
            impute_date(.data$lb_dt) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(lb, lb_snapshot, "lb")
    }

    lb_snapshot
}

#' Snapshot Protocol Deviation Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param protdev `data.frame` Protocol deviation data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @import dplyr
#' @importFrom readr parse_number
#'
#' @export
snapshot_protdev <- function(snapshot_date, dm, protdev = clindata::ctms_protdev, print_check_rows = TRUE) {
    if ("visit_nsv" %in% names(protdev)) {
        protdev_snapshot <- protdev %>%
            left_join(
                dm %>% select("subjid", "firstparticipantdate"),
                "subjid"
            ) %>%
            mutate(
                week = case_when(
                    visit_nsv == "Screening" ~ -5,
                    visit_nsv == "Baseline" ~ 0,
                    grepl("week", visit_nsv, TRUE) ~ readr::parse_number(visit_nsv)
                ),
                dv_dt = if_else(
                    .data$dv_dt == "",
                    impute_date(.data$firstparticipantdate) + 7 * .data$week,
                    impute_date(.data$dv_dt)
                )
            ) %>%
            select(-c("firstparticipantdate", "week"))
    } else {
        protdev_snapshot <- protdev
    }

    protdev_snapshot1 <- protdev_snapshot %>%
        dplyr::filter(
            impute_date(.data$deviationdate) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(protdev, protdev_snapshot, "protdev")
    }

    protdev_snapshot
}


#' Snapshot Queries Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param queries `data.frame` EDC queries data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_queries <- function(snapshot_date, queries = clindata::edc_queries, print_check_rows = TRUE) {
    queries_snapshot <- queries %>%
        dplyr::filter(
            impute_date(.data$created) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(queries, queries_snapshot, "queries")
    }

    queries_snapshot
}


#' Snapshot SDRCOMP Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param sdrgcomp `data.frame` SDRGCOMP data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr if_else inner_join mutate select
#'
#' @export
snapshot_sdrgcomp <- function(snapshot_date, dm, sdrgcomp = clindata::rawplus_sdrgcomp, print_check_rows = TRUE) {
    sdrgcomp_snapshot <- sdrgcomp %>%
        dplyr::inner_join(
            dm %>% select("subjid", "lastdosedate", "lastdosedate0"),
            "subjid"
        ) %>%
        dplyr::mutate(
            sdrgyn = dplyr::if_else(
                .data$lastdosedate < .data$lastdosedate0,
                "",
                .data$sdrgyn
            ),
            sdrgreas = dplyr::if_else(
                .data$lastdosedate < .data$lastdosedate0,
                "",
                .data$sdrgreas
            )
        ) %>%
        dplyr::select(
            -"lastdosedate",
            -"lastdosedate0"
        )

    if (print_check_rows) {
        check_rows(sdrgcomp, sdrgcomp_snapshot, "sdrgcomp")
    }

    sdrgcomp_snapshot
}


#' Snapshot CTMS Site Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param site `data.frame` CTMS site data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_site <- function(
        snapshot_date,
        dm = clindata::rawplus_dm,
        site = clindata::ctms_site,
        print_check_rows = TRUE) {
    n_enrolled <- dm %>%
        filter(
            .data$firstparticipantdate <= snapshot_date
        ) %>%
        group_by(.data$siteid) %>%
        summarize(
            enrolled_participants = n()
        ) %>%
        ungroup()

    site_snapshot <- site %>%
        inner_join(
            n_enrolled,
            c("site_num" = "siteid")
        )

    if (print_check_rows) {
        check_rows(site, site_snapshot, "site")
    }

    site_snapshot
}


#' Snapshot STUDCOMP Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param studcomp `data.frame` Study completion data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr if_else inner_join mutate select
#'
#' @export
snapshot_studcomp <- function(snapshot_date, dm, studcomp = clindata::rawplus_studcomp, print_check_rows = TRUE) {
    studcomp_snapshot <- studcomp %>%
        dplyr::inner_join(
            dm %>% dplyr::select("subjid", "lastparticipantdate", "lastparticipantdate0"),
            "subjid"
        ) %>%
        dplyr::mutate(
            compyn = dplyr::if_else(
                .data$lastparticipantdate < .data$lastparticipantdate0,
                "",
                .data$compyn
            ),
            compreas = dplyr::if_else(
                .data$lastparticipantdate < .data$lastparticipantdate0,
                "",
                .data$compreas
            )
        ) %>%
        dplyr::select(
            -"lastparticipantdate",
            -"lastparticipantdate0"
        )

    if (print_check_rows) {
        check_rows(studcomp, studcomp_snapshot, "studcomp")
    }

    studcomp_snapshot
}


#' Snapshot CTMS Study Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param dm `data.frame` Demographic data.
#' @param study `data.frame` CTMS study data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter group_by inner_join summarize ungroup
#'
#' @export
snapshot_study <- function(
        snapshot_date,
        dm = clindata::rawplus_dm,
        study = clindata::ctms_study,
        print_check_rows = TRUE
) {
    n_enrolled <- dm %>%
        filter(
            .data$firstparticipantdate <= snapshot_date
        ) %>%
        group_by(.data$studyid) %>%
        summarize(
            enrolled_sites = length(unique(.data$siteid)),
            enrolled_participants = n()
        ) %>%
        ungroup()

    study_snapshot <- study %>%
        select(-any_of(c('enrolled_sites', 'enrolled_participants'))) %>%
        inner_join(
            n_enrolled,
            c("protocol_number" = "studyid")
        )

    if (print_check_rows) {
        check_rows(study, study_snapshot, "study")
    }

    study_snapshot
}


#' Snapshot VISDT Domain
#'
#' @param snapshot_date `Date` Date at which to snapshot data
#' @param visdt `data.frame` Visit date data.
#' @param print_check_rows `logical` Print row-checking output to console? Default: `TRUE`.
#'
#' @importFrom dplyr filter
#'
#' @export
snapshot_visdt <- function(snapshot_date, visdt = clindata::rawplus_visdt, print_check_rows = TRUE) {

    visdt_snapshot <- visdt %>%
        dplyr::filter(
            impute_date(.data$visit_dt) <= snapshot_date
        )

    if (print_check_rows) {
        check_rows(visdt, visdt_snapshot, "visdt")
    }

    visdt_snapshot
}


#' Returns an earlier date
#'
#' @param snapshot_date `Date` Initial snapshot date
#' @param interval `function` Interval size as implemented in {lubridate}, one of `year`, `month`, or `day`
#' @param n_intervals `numeric` Number of intervals
#'
#' @return `Date`
#'
#' @importFrom cli cli_alert_success
#' @importFrom lubridate interval month
#'
#' @export
get_snapshot_date <- function(
        snapshot_date = NULL,
        interval = lubridate::month,
        n_intervals = 1
) {
    if (is.null(snapshot_date)) {
        snapshot_date <- clindata::rawplus_visdt$visit_dt %>%
            impute_date() %>%
            max(na.rm = TRUE)
    } else {
        snapshot_date <- interval(snapshot_date) - n_intervals
    }

    cat(paste0("\n", paste(rep("-", 64), collapse = ""), "\n"))
    cli::cli_alert_success("New snapshot date: {snapshot_date}")
    cat(paste0(paste(rep("-", 64), collapse = ""), "\n\n"))

    snapshot_date
}


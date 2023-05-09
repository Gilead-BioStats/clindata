library(dplyr)
library(clindata)
set.seed(321)

# helper functions --------------------------------------------------------
subset_aes <- function(data, n_rows) {
    data <- data %>%
        mutate(
            tmpid = row_number(),
            remove = ifelse(siteid == "43" & tmpid > n_rows, FALSE, TRUE)
        ) %>%
        filter(remove)

    return(data)
}

# modify mean ae rate -----------------------------------------------------
ae_count <- clindata::rawplus_ae %>%
    count(siteid)

time_sum <- clindata::rawplus_dm %>%
    count(siteid, wt = timeonstudy) %>%
    rename(time = n)

all_sites <- left_join(ae_count, time_sum) %>%
    mutate(rate = n / time,
           q30 = quantile(rate, 0.30),
           q70 = quantile(rate, 0.70),
           q95 = quantile(rate, 0.95))

base_sites <- all_sites %>%
    filter(rate > q30, rate < q70) %>%
    pull(siteid) %>%
    sample(size = 50, replace = FALSE)

hi_sites <- all_sites %>%
    filter(time > quantile(time, 0.5), rate > q95) %>%
    pull(siteid)

sites_to_keep <- c(base_sites, hi_sites, "139")

site_139 <- clindata::rawplus_ae %>%
    filter(siteid == "139") %>%
    slice(1)


ae <- clindata::rawplus_ae %>%
    filter(siteid %in% sites_to_keep, siteid != "139") %>%
    bind_rows(site_139)


dm <- clindata::rawplus_dm %>%
    filter(siteid %in% sites_to_keep) %>%
    mutate(timeonstudy = round(timeonstudy))



# save demo data ----------------------------------------------------------
saveRDS(ae, file = here::here("inst", "ae_case_study", "rawplus_ae_demo.Rda"))
saveRDS(dm, file = here::here("inst", "ae_case_study", "rawplus_dm_demo.Rda"))


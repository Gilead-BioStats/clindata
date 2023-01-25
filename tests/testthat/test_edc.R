# TODO: add tests checking uniqueness of key variables in each dataset using data standard YAML
standard <- 'edc'
metadata <- read_yaml(system.file('data-standards', paste0(standard, '.yaml'), package = 'clindata'))

test_that('domain keys uniquely identify each record', {
    for (domain in names(metadata)) {
        if (domain != 'data_points') { # data points isn't attached to package because it takes forever to load
            data <- get(paste0(standard, '_', domain))

            unique_rows <- data %>%
                distinct(across(metadata[[ domain ]]$keys))

            expect_equal(nrow(unique_rows), nrow(data))
        }
    }
})

test_that("[ edc_data_change_rate ] and [ edc_queries ] can be joined one-to-one", {
    query_count <- edc_queries %>%
        group_by(across(metadata$data_change_rate$keys)) %>%
        summarize(
            n_queries = n()
        ) %>%
        ungroup()

    data_change_rate_with_queries <- edc_data_change_rate %>%
        left_join(
            query_count,
            metadata$data_change_rate$keys
        )

    expect_equal(nrow(data_change_rate_with_queries), nrow(edc_data_change_rate))
})

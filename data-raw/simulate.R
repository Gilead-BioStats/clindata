load_all('.')
#walk(list.files('R', full.names = TRUE), ~source(.x))
#load_all('../gsm')

params <- yaml::read_yaml('data-raw/simulate.yaml')

for (n_sites in params$n_sites) {
    for (n_subjects in params$n_subjects) {
        run_simulation(
            n_sites,
            n_subjects
        )
    }
}

# clindata
A repository of anonymized clinical trial data for testing and development

## Data inclusion and update process
1. Place data files in the folder under `./data-raw/` named according to the data standard.
2. Define domain descriptions in the .yaml file under `./inst/data-standards/` named according to the data standard.
3. Run each .R script under `./data-raw/`:
  - `./data-raw/ctms.R`
  - `./data-raw/rawplus.R`
4. Run `devtools::document()`.

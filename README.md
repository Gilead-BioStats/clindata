# clindata
A repository of anonymized clinical trial data for testing and development

## Data inclusion and update process
1. Place data files in the folder under ./data-raw named according to the data standard.
2. Define domain descriptions in the .yaml file under ./inst/data-standards/ named according to the data standard.
3. Save data to .rda for inclusion in the package by running ./data-raw/save_[data standard].R.
4. Document data by running ./data-raw/document_data.R.
5. Run `devtools::load_all()`.
6. Run `devtools::document()`.

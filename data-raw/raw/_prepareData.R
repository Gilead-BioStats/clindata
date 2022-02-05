here::i_am("data-raw/raw/_prepareData.R")
library(haven)
library(usethis)
library(here)

# Copy csv files in /data-raw to /data and create roxygen documentation in /R
paths<-list.files(
    here::here("data-raw","raw"), 
    pattern = "\\.sas7bdat$", 
    ignore.case=TRUE, 
    full.names=FALSE, 
    recursive=TRUE
)  

for(path in paths){
    # remove file extension
    file <- paste0(
        "raw_",
        substr(path,1,nchar(path)-9)
    ) 

    # Read csv to data frame   
    assign(
        file, 
        haven::read_sas(here('data-raw','raw',path))
    ) 
    
    #save to .RDA to /data 
    do.call(
        'use_data', 
        list(as.name(file), overwrite = TRUE)
    ) 
}

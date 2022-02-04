here::i_am("data-raw/raw/prepareRoxygen.R")
library(here)
options(useFancyQuotes = FALSE)


# Generate basic roxygen documentation
# Thanks to: https://stackoverflow.com/questions/51092509/whats-the-best-way-to-automatically-generate-roxygen2-documentation-for-a-data
paths<-list.files(
    here::here("data"), 
    pattern = "raw_", 
    ignore.case=TRUE, 
    full.names=FALSE, 
    recursive=TRUE
)  

roxyheaders <- unlist(
    sapply(
        paths, 
        function(path){
            d<- substr(path,1,nchar(path)-4)
            load(file=here("data",path))
            df<-get(d)

            roxy<-c(
                paste("#' @title", d), 
                paste("#' @description Raw", substring(d,5) ,"data frame"), 
                paste("#' @format a data frame with",dim(df)[1], "rows and", dim(df)[2],"columns."),
                dQuote(d),
                "\n"
            )
            return(roxy)
        },
        simplify=FALSE
    ),
    use.names=FALSE
)

cat(roxyheaders, file=here::here("R/raw.R"), sep="\n")
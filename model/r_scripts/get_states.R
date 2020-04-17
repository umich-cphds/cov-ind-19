library(tidyverse)
library(vroom)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")

x <- vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
filter(State != "la" & State != "jk") %>%
filter(Date == max(Date)) %>%
filter(Cases >= 50) %>%
filter(Deaths >= 1)

cat(x$State, "\n")

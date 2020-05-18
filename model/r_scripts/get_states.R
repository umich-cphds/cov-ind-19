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
group_by(State) %>%
filter(Date == max(Date)) %>%
ungroup() %>%
top_n(20, Cases)

cat(x$State, "\n")

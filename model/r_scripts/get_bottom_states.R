suppressPackageStartupMessages({
  library(tidyverse)
  library(vroom)
})

data_repo <- Sys.getenv("data_repo")
today <- Sys.getenv("today")

x <- vroom(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
  group_by(State) %>%
  filter(Date == max(Date) & State != "un" & State != "la" & State != "dd" & State != "hp" & State != "py" & State != "dn" & State != "ga" & State != "ld" ) %>%
  ungroup() %>%
  top_n(-13, Cases)

cat(x$State, "\n")

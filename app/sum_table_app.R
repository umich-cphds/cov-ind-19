
suppressPackageStartupMessages({
  library(tidyverse)
  library(vroom)
  library(EpiEstim)
  library(glue)
  library(gt)
  library(lubridate)
  library(janitor)
  library(scales)
  library(ggtext)
  library(here)
  library(httr)
  library(covid19india)
})

data_repo <- Sys.getenv("data_repo")
today     <- Sys.getenv("today")

x <- vroom(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
  group_by(State) %>%
  filter(Date == max(Date) & State != "un" & State != "la" & State != "dd" & State != "hp" & State != "py") %>%
  ungroup() %>%
  top_n(20, Cases)

top20 = x$State

India_gt_table = function() { 

  tabs   <- get_metrics_tables(top20 = top20)
  
  return(
    list(
       full20          = tabs$full_t20,
       point_in_time20 = tabs$point_in_time_t20,
       cumulative20    = tabs$cumulative_t20,
       full            = tabs$full,
       point_in_time   = tabs$point_in_time,
       cumulative      = tabs$cumulative
    )
  )
  
}

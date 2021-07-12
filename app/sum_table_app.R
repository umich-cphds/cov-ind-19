
suppressPackageStartupMessages({
  library(tidyverse)
  library(vroom)
})

data_repo <- Sys.getenv("data_repo")
today <- Sys.getenv("today")

x <- vroom(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
  group_by(State) %>%
  filter(Date == max(Date) & State != "un" & State != "la" & State != "dd" & State != "hp" & State != "py") %>%
  ungroup() %>%
  top_n(20, Cases)

top20 = x$State

India_gt_table = function() { 
  
  remotes::install_github("maxsal/covid19india")
  
  covid19india::biblioteca(
    c("tidyverse", "EpiEstim", "gt", "glue", "lubridate", "janitor",
      "scales", "ggtext", "here", "httr", "maxsal/covid19india")
  )
  
  # source(here("code", "functions", "functions.R"))
  set_seed <- 46342
  set.seed(set_seed)
  dat <- get_all_data()
  cfr    <- get_cfr(dat) %>% distinct()
  r0_est <- get_r_est(dat)
  tabs   <- get_metrics_tables(top20 = top20)
  
  return(
    list(
       full20 = tabs$full_t20,
       point_in_time20 = tabs$point_in_time_t20,
       cumulative20 = tabs$cumulative_t20,
       full = tabs$full,
       point_in_time = tabs$point_in_time,
       cumulative = tabs$cumulative
    )
  )
  
}

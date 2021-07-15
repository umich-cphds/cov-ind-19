suppressPackageStartupMessages({
library(tidyverse)
library(janitor)
})

today <- Sys.getenv("today")
data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")
path  <- paste0(data_repo, "/", today, "/")
if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Creating ", path)
}

start_date <- as.Date(today) - 45

# load observed data ----------
jhu_data <- read_tsv(paste0(data_repo, "/", today, "/jhu_data_mod.csv"), col_types = cols()) %>%
filter(Country == "India" & Date >= start_date) %>%
clean_names()

state_data <- read_tsv(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
filter(Date >= start_date) %>%
clean_names()

# get states ----------
source(paste0(code_repo, "/model/r_scripts/get_all_states.R"))
forecasts <- c("India", x$State)

# create data
for (forecast in forecasts) {

  if (forecast == "India") {
    data <- jhu_data
  } else {
    data <- state_data %>% filter(state == forecast)
  }

  path <- paste0(data_repo, "/", today, "/1wk")

  observed <- data %>%
  arrange(date) %>%
  mutate(
    incidence = cases - dplyr::lag(cases)
  ) %>%
  dplyr::select(
    date,
    value = cases,
    incidence
  ) %>%
  add_column(scenario = "Observed")

  no_int_forecast   <- read_tsv(paste0(path, "/", tolower(forecast), "_no_int_data.txt"), col_types = cols())

  combined_dat <- bind_rows(
    observed,
    no_int_forecast,
  )

  write_tsv(combined_dat, file = paste0(path, "/", forecast, "_plot_data.txt"))

}

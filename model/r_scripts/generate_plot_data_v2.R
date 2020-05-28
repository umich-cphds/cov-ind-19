library(tidyverse)
library(janitor)

# set variables based on testing or production ----------
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")
path  <- paste0(data_repo, today, "/")
if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Creating ", path)
}

start_date <- as.Date(today) - 45

# load observed data ----------
jhu_data <- read_tsv(paste0(data_repo, today, "/jhu_data_mod.csv"), col_types = cols()) %>%
filter(Country == "India" & Date >= start_date) %>%
clean_names()

state_data <- read_tsv(paste0(data_repo, today, "/covid19india_data.csv"), col_types = cols()) %>%
filter(Date >= start_date) %>%
clean_names()

# get states ----------
source("~/cov-ind-19/model/r_scripts/get_states.R")
forecasts <- c("India", x$State)

# create data
for (forecast in forecasts) {

  if (forecast == "India") {
    data <- jhu_data
  } else {
    data <- state_data %>% filter(state == forecast)
  }

  path <- paste0(data_repo, today, "/1wk")

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

  social_forecast   <- read_tsv(paste0(path, "/", tolower(forecast), "_social_dist_data.txt"), col_types = cols())
  no_int_forecast   <- read_tsv(paste0(path, "/", tolower(forecast), "_no_int_data.txt"), col_types = cols())
  moderate_forecast <- read_tsv(paste0(path, "/", tolower(forecast), "_moderate_data.txt"), col_types = cols())
  normal_forecast   <- read_tsv(paste0(path, "/", tolower(forecast), "_normal_data.txt"), col_types = cols())
  cautious_forecast <- read_tsv(paste0(path, "/", tolower(forecast), "_cautious_data.txt"), col_types = cols())

  combined_dat <- bind_rows(
    observed,
    social_forecast,
    no_int_forecast,
    moderate_forecast,
    normal_forecast,
    cautious_forecast
  )

  write_tsv(combined_dat, path = paste0(path, "/", tolower(forecast), "_plot_data.txt"))

}

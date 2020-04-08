# libraries ----------
library(tidyverse)
library(glue)
library(vroom)
library(chron)
library(reshape2)
library(rjags)
library(gtools)     # rdirichlet(n, alpha)
library(scales)     # alpha　function
library(data.table)
library(devtools)

arrayid=Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# specificatioons ----------
Ms                 <- 5e5           # 5e5 recommended (5e3 for testing - but not stable)
nburnins           <- 2e5           # 2e5 recommended (2e3 for testing - but not stable)
R_0                <- 2             # basic reproduction number
save_mcmc          <- FALSE         # output MCMC files (default = TRUE; needed for incidence CI calculations)
save_plot_data     <- TRUE
start_date         <- "2020-03-01"

# eSIR ----------
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE") # relevant model code

# !! directory ----------
today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today, "/2wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# data ----------
jhu_cases     <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhu_recovered <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
jhu_deaths    <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

### check that datasets end on same date ----------
last_jhuc_case_date     <- as.Date(tail(names(jhu_cases), 1), format("%m/%d/%y"))
last_jhur_case_date     <- as.Date(tail(names(jhu_recovered), 1), format("%m/%d/%y"))
last_jhud_case_date     <- as.Date(tail(names(jhu_deaths), 1), format("%m/%d/%y"))
min_date                <- min(c(last_jhuc_case_date, last_jhur_case_date, last_jhud_case_date))
end_date                <- as.Date("2020-04-04")

format_data <- function(dat, x) {
  dat %>%
    filter(`Country/Region` %in% c(x)) %>%
    select(-c(`Province/State`, "Lat", "Long")) %>%
    pivot_longer(names_to = "date", values_to = "count", -`Country/Region`) %>%
    mutate(date = as.Date(sub("X", "", date), format = "%m/%d/%y")) %>%
    filter(date >= start_date & date <= end_date)
}

cases     <- format_data(jhu_cases, x = c("India", "Italy", "US"))
recovered <- format_data(jhu_recovered, x = c("India", "Italy", "US"))
deaths    <- format_data(jhu_deaths, x = c("India", "Italy", "US"))

NI_india <- cases %>% filter(`Country/Region` == "India") %>% .$count
RI_india <- recovered %>% filter(`Country/Region` == "India") %>% .$count +
            deaths %>% filter(`Country/Region` == "India") %>% .$count
N_india  <- 1.34e9
R_india  <- RI_india / N_india
Y_india  <- NI_india / N_india - R_india

NI_italy <- cases %>% filter(`Country/Region` == "Italy") %>% .$count
RI_italy <- recovered %>% filter(`Country/Region` == "Italy") %>% .$count +
            deaths %>% filter(`Country/Region` == "Italy") %>% .$count
N_italy  <- 60.36e6
R_italy  <- RI_italy / N_italy
Y_italy  <- NI_italy / N_italy - R_italy

NI_usa <- cases %>% filter(`Country/Region` == "US") %>% .$count
RI_usa <- recovered %>% filter(`Country/Region` == "US") %>% .$count +
          deaths %>% filter(`Country/Region` == "US") %>% .$count
N_usa  <- 327.2e6
R_usa  <- RI_usa / N_usa
Y_usa  <- NI_usa / N_usa - R_usa

# models ---------

if (arrayid == 1) {
# india
print("Running India model")
res <- tvt.eSIR(
  Y_india,
  R_india,
  begin_str      = as.character(chron(chron("03-01-2020",format="m-d-Y"),format="m/d/y")),
  T_fin          = 200,
  M              = Ms,
  nburnin        = nburnins,
  casename       = "india_med_fig3",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = save_plot_data,
  R0             = 2
  )
}

if (arrayid == 2) {
  print("Running Italy model")
# italy
res <- tvt.eSIR(
  Y_italy,
  R_italy,
  begin_str      = as.character(chron(chron("03-01-2020",format="m-d-Y"),format="m/d/y")),
  T_fin          = 200,
  M              = Ms,
  nburnin        = nburnins,
  casename       = "italy_med_fig_3",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = save_plot_data,
  R0             = 3.5
)
}

if (arrayid == 3) {
  print("Running US model")
# us
res <- tvt.eSIR(
  Y_usa,
  R_usa,
  begin_str      = as.character(chron(chron("03-01-2020",format="m-d-Y"),format="m/d/y")),
  T_fin          = 200,
  M              = Ms,
  nburnin        = nburnins,
  casename       = "usa_med_fig_3",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = save_plot_data,
  R0             = 3.5
)
}

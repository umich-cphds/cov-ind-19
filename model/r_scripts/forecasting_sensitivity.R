# libraries ----------
library(tidyverse)
library(chron)
library(reshape2)
library(rjags)
library(gtools) #rdirichlet(n, alpha)
library(scales) #alpha　function
library(data.table)
library(devtools)
library(eSIR)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
        data_repo <- "~/cov-ind-19-data/"
        Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
        data_repo <- "~/cov-ind-19-test/"
        Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

today     <- Sys.getenv("today")
arrayid=Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# !! directory ----------
wd <- paste0(data_repo, today, "/sensitivity/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# data ----------
jhu_cases     <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhu_recovered <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") # pull data daily

  ### check that datasets end on same date ----------
  last_jhuc_case_date     <- as.Date(sub("X", "", rev(names(jhu_cases))[1]), format("%m.%d.%y"))
  last_jhur_case_date     <- as.Date(sub("X", "", rev(names(jhu_recovered))[1]), format("%m.%d.%y"))
  min_date                <- min(last_jhuc_case_date, last_jhur_case_date)

  jhu_recovered <- jhu_recovered %>%
    select(-c("Province.State", "Lat", "Long")) %>%
    gather(key = "date", value = "count", -Country.Region) %>%
    mutate(date = as.Date(sub("X", "", date), format = "%m.%d.%y")) %>%
    filter(date >= "2020-03-01" & date <= min_date )

  jhu_cases <- jhu_cases %>%
    select(-c("Province.State", "Lat", "Long")) %>%
    gather(key = "date", value = "count", -Country.Region) %>%
    mutate(date = as.Date(sub("X", "", date), format = "%m.%d.%y")) %>%
    filter(date >= "2020-03-01" & date <= min_date )

india_cases     <- jhu_cases %>% filter(Country.Region == "India")
india_recovered <- jhu_cases %>% filter(Country.Region == "India")
N_india         <- 1.34e9
NI_india        <- india_cases$count
RI_india        <- india_recovered$count
R_india         <- unlist(RI_india / N_india)
Y_india         <- unlist(NI_india / N_india - R_india)

N_indmet <- 32e6
NI_indmet <- india_cases$count
RI_indmet <- india_recovered$count
R_indmet  <- unlist(RI_indmet / N_indmet)
Y_indmet  <- unlist(NI_indmet / N_indmet - R_indmet)

NI10_india        <- india_cases$count * 10
RI10_india        <- india_recovered$count
R10_india         <- unlist(RI10_india / N_india)
Y10_india         <- unlist(NI10_india / N_india - R10_india)

# model prep ---------

# MODEL DESCRIPTION
# LOCKDOWN + MODERATE RETURN (they actually take effect one week earlier)
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4 > 0.6
#   - May 20:              0.6

change_time <- c(format(seq(as.Date("2020/03/25") - 7, as.Date("2020/04/07") - 7, "days"), "%m/%d/%Y"),
                 format(seq(as.Date("2020/04/08") - 7, as.Date("2020/04/15") - 7, "days"), "%m/%d/%Y"),
                 format(seq(as.Date("2020/04/29") - 7, as.Date("2020/05/20") - 7, "days"), "%m/%d/%Y"))
pi0         <- c(1,
                 rev(seq(0.75, 1, 0.25 / 14))[-1],
                 rev(seq(0.4, 0.75, 0.35 / 7))[-1],
                 seq(0.4, 0.75, 0.35 / 21),
                 0.75)

if (arrayid == 1) {
  print("Running model 1: India w/ R0 = 2.5")
  # Model 1: lockdown + moderate return in India w/ R0 = 2.5 ----------
  india_mod <- tvt.eSIR(
   Y_india,
   R_india,
   begin_str      = "03/01/2020",
   death_in_R     = 0.2,
   T_fin          = 200,
   R0             = 2.5,
   dic            = TRUE,
   change_time    = change_time,
   pi0            = pi0,
   casename       = "india_mod",
   save_files     = FALSE,
   save_mcmc      = FALSE,
   save_plot_data = TRUE,
   M              = Ms,
   nburnin        = nburnins
  )
}

if (arrayid == 2) {
  print("Running model 2: India metro population")
  # Model 2: lockdown + moderate return in India (metro population)
  indmet_mod <- tvt.eSIR(
   Y_indmet,
   R_indmet,
   begin_str      = "03/01/2020",
   death_in_R     = 0.2,
   T_fin          = 200,
   R0             = 2,
   dic            = TRUE,
   change_time    = change_time,
   pi0            = pi0,
   casename       = "indmet_mod",
   save_files     = FALSE,
   save_mcmc      = FALSE,
   save_plot_data = TRUE,
   M              = Ms,
   nburnin        = nburnins
  )
}

if (arrayid == 3) {
  print("Running model 3: India 10x cases")
  # Model 3: lockdown + moderate return in India (10x cases)
  india10_mod <- tvt.eSIR(
   Y10_india,
   R10_india,
   begin_str      = "03/01/2020",
   death_in_R     = 0.2,
   T_fin          = 200,
   R0             = 2,
   dic            = TRUE,
   change_time    = change_time,
   pi0            = pi0,
   casename       = "india10_mod",
   save_files     = FALSE,
   save_mcmc      = FALSE,
   save_plot_data = TRUE,
   M              = Ms,
   nburnin        = nburnins
  )
}

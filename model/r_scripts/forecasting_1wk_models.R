# libraries ----------
library(tidyverse)
library(chron)
library(reshape2)
library(rjags)
library(gtools) #rdirichlet(n, alpha)
library(scales) #alpha　function
library(nCov2019)
library(data.table)
library(devtools)

arrayid=Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020
R_0      <- 2      # basic reproduction number
Ms       <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
nburnins <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)

# eSIR ----------
# library(eSIR)  # trouble installing
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE") # relevant model code
# source_url("https://github.com/lilywang1988/eSIR/blob/master/R/qh.eSIR.R?raw=TRUE")  # irrelevant??

# !! directory ----------
today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today, "/1wk/")
if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
    message("Creating ", wd)
}
setwd(wd)

# data ----------
jhu_cases     <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
jhu_recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) # pull data daily

### check that datasets end on same date ----------
  last_jhuc_case_date     <- as.Date(sub("X", "", rev(names(jhu_cases))[1]), format("%m.%d.%y"))
  last_jhur_case_date     <- as.Date(sub("X", "", rev(names(jhu_cases))[1]), format("%m.%d.%y"))
  min_date <- min(last_jhuc_case_date, last_jhur_case_date)

  india_recovered <- jhu_recovered %>%
    filter(Country.Region == "India") %>%
    select(-c("Province.State", "Lat", "Long")) %>%
    gather(key = "date", value = "count", -Country.Region) %>%
    mutate(date = as.Date(sub("X", "", date), format = "%m.%d.%y")) %>%
    filter(date >= "2020-03-01" & date <= min_date )

  india_cases <- jhu_cases %>%
    filter(Country.Region == "India") %>%
    select(-c("Province.State", "Lat", "Long")) %>%
    gather(key = "date", value = "count", -Country.Region) %>%
    mutate(date = as.Date(sub("X", "", date), format = "%m.%d.%y")) %>%
    filter(date >= "2020-03-01" & date <= min_date )

NI_complete <- india_cases$count
RI_complete <- india_recovered$count
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# models ---------

# --------------------------------------------------------------------- #
# all model parameters are 1 week ahead of those stated in the scenario #
# --------------------------------------------------------------------- #

if (arrayid == 1) {
print("Running model_1")
# Model 1 ----------
# Model 1 Scenario: Travel ban + social distancing + lockdown
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4
change_time <- c(format(seq(as.Date("2020/03/25") - 7, as.Date("2020/04/07") - 7, "days"), "%m/%d/%Y"),
                 format(seq(as.Date("2020/04/08") - 7, as.Date("2020/04/15") - 7, "days"), "%m/%d/%Y"))
pi0         <- c(1,
                 rev(seq(0.75, 1, 0.25 / 14))[-1],
                 rev(seq(0.4, 0.75, 0.35 / 7))[-1],
                 0.4)

model_1 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_1",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid == 2){
print("Running model_2")
# Model 2 ----------
# Model 2 Scenario: Travel ban + social distancing
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
change_time <- c(format(seq(as.Date("2020/03/25") - 7, as.Date("2020/04/07") - 7, "days"), "%m/%d/%Y"),
                 format(as.Date("2020/04/08") - 7, "%m/%d/%Y"))
pi0         <- c(1,
                 rev(seq(0.75, 1, 0.25 / 14))[-1],
                 0.75)

model_2 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_2",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==3){
print("Running model_3")
# Model 3 ----------
# Model 3 Scenario: No intervention
model_3 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_3",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==4){
print("Running model_4")
# Model 4 ----------
# Model 4 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (average)
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

model_4 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_4",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==5){
print("Running model_5")
# Model 5 ----------
# Model 5 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (party)
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4 > 1
#   - May 20:              1
change_time <- c(format(seq(as.Date("2020/03/25") - 7, as.Date("2020/04/07") - 7, "days"), "%m/%d/%Y"),
                 format(seq(as.Date("2020/04/08") - 7, as.Date("2020/04/15") - 7, "days"), "%m/%d/%Y"),
                 format(seq(as.Date("2020/04/29") - 7, as.Date("2020/05/20") - 7, "days"), "%m/%d/%Y"))
pi0         <- c(1,
                 rev(seq(0.75, 1, 0.25 / 14))[-1],
                 rev(seq(0.4, 0.75, 0.35 / 7))[-1],
                 seq(0.4, 1, 0.6 / 21),
                 1)

model_5 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_5",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==6){
print("Running model_6")
# Model 6 ----------
# Model 6 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (scared)
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
                 seq(0.4, 0.6, 0.2 / 21),
                 0.6)

model_6 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = "India_6",
  save_files     = FALSE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

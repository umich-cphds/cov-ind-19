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

# eSIR ----------
# library(eSIR)  # trouble installing
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE") # relevant model code
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/qh.eSIR.R?raw=TRUE")  # irrelevant??

# !! directory ----------
today <- Sys.Date()
wd <- paste0("/mnt/biostat/ftp/ncov2019/India/", today)
setwd(wd)

# !!  data ----------
JHUCases     <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")     # pull data daily
JHURecovered <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") # pull data daily
# These two datasets above need to be updated from https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases.
#   just pull from JHU CSSE GitHub? Preprocessed elsewhere

IndiaRecovered <- JHURecovered[JHURecovered$Country.Region == "India", ]
IndiaCases     <- JHUCases[JHUCases$Country.Region == "India", ]


NI_complete <- IndiaCases[-(1:43)]      # cases per day (starting March 1)
RI_complete <- IndiaRecovered[-(1:43)]  # recovered per day (starting March 1)
N=1.38e9                                # Population of India

R=unlist(RI_complete/N)   # proportion of recovered per day
Y=head(unlist(NI_complete/N-R), -1) # proportion of cases per day

# These three  below are the models that we run. You may need to separate them into jobs to make it more efficient

# Model 1 ----------
# Model 1 Scenario: Travel ban + social distancing + lockdown
#   - March 1 - March 12:  1
#   - March 12:            0.8
#   - March 12 - March 25: 0.6 (but in equal drops per day)
#   - March 26:            0.3
#   - March 28:            0.2
change_time <- c("03/12/2020",
                 format(seq(as.Date("2020/03/13"), as.Date("2020/03/25"), "days"), "%m/%d/%Y"),
                 "03/26/2020",
                 "03/28/2020")
pi0         <- c(1, 0.8, rev(seq(0.6, 0.8, 0.2/13))[-1], 0.3, 0.2)

if(arrayid==1){
print("Running model_1")
model_1 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.02,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_1",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
)
}

if(arrayid==2){
print("Running model_2")
# Model 2 ----------
# Model 2 Scenario: Travel ban + social distancing
#   - March 1 - March 12:  1
#   - March 12:            0.8
#   - March 12 - March 25: 0.6 (but in equal drops per day)
change_time <- c("03/12/2020",
                 format(seq(as.Date("2020/03/13"), as.Date("2020/03/25"), "days"), "%m/%d/%Y"))
pi0         <- c(1, 0.8, rev(seq(0.6, 0.8, 0.2/13))[-1])

model_2 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.02,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_2",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
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
  death_in_R     = 0.02,
  T_fin          = 200,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_3",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
)
}

if(arrayid==4){
print("Running model_4")
# Model 4 ----------
# Model 4 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (moderate)
#   - March 1 - March 12:  1
#   - March 12:            0.8
#   - March 12 - March 25: 0.6 (but in equal drops per day)
#   - March 26:            0.3
#   - March 28:            0.2
#   - April 16:            0.6
change_time <- c("03/12/2020",
                 format(seq(as.Date("2020/03/13"), as.Date("2020/03/25"), "days"), "%m/%d/%Y"),
                 "03/26/2020",
                 "03/28/2020",
                 "04/16/2020"
)
pi0         <- c(1, 0.8, rev(seq(0.6, 0.8, 0.2/13))[-1], 0.3, 0.2, 0.6)

model_4 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.02,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_4",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
)
}

if(arrayid==5){
print("Running model_5")
# Model 5 ----------
# Model 5 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (party)
#   - March 1 - March 12:  1
#   - March 12:            0.8
#   - March 12 - March 25: 0.6 (but in equal drops per day)
#   - March 26:            0.3
#   - March 28:            0.2
#   - April 16:            0.7
change_time <- c("03/12/2020",
format(seq(as.Date("2020/03/13"), as.Date("2020/03/25"), "days"), "%m/%d/%Y"),
"03/26/2020",
"03/28/2020",
"04/16/2020"
)
pi0         <- c(1, 0.8, rev(seq(0.6, 0.8, 0.2/13))[-1], 0.3, 0.2, 0.7)

model_5 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.02,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_5",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
)
}

if(arrayid==6){
print("Running model_6")
# Model 6 ----------
# Model 6 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (scared)
#   - March 1 - March 12:  1
#   - March 12:            0.8
#   - March 12 - March 25: 0.6 (but in equal drops per day)
#   - March 26:            0.3
#   - March 28:            0.2
#   - April 16:            0.5
change_time <- c("03/12/2020",
format(seq(as.Date("2020/03/13"), as.Date("2020/03/25"), "days"), "%m/%d/%Y"),
"03/26/2020",
"03/28/2020",
"04/16/2020"
)
pi0         <- c(1, 0.8, rev(seq(0.6, 0.8, 0.2/13))[-1], 0.3, 0.2, 0.5)

model_6 <- tvt.eSIR(
  Y,
  R,
  begin_str      = "03/01/2020",
  death_in_R     = 0.02,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = 2,             # basic reproduction number
  dic            = TRUE,
  casename       = "India_6",
  save_files     = TRUE,
  save_mcmc      = FALSE,
  save_plot_data = TRUE,
  M              = 5e5,           # should be 5e5 (5e3 for testing - but not stable)
  nburnin        = 2e5            # should be 2e5 (2e3 for testing - but not stable)
)
}

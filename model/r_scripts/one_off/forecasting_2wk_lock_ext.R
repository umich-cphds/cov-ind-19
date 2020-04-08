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
delay              <- 14             # in days (default = 7)
length_of_lockdown <- 21            # in days (default = 21)
Ms                 <- 5e5           # 5e5 recommended (5e3 for testing - but not stable)
nburnins           <- 2e5           # 2e5 recommended (2e3 for testing - but not stable)
pi_cautious        <- 0.6           # pi corresponding to cautious return
pi_lockdown        <- 0.4           # pi corresponding to lockdown
pi_moderate        <- 0.75          # pi corresponding to moderate return
pi_normal          <- 1             # pi corresponding to normal (pre-intervention) return
pi_sdtb            <- 0.75          # pi corresponding to social distancing and travel ban
R_0                <- 2             # basic reproduction number
replace_apr3       <- FALSE         # replace April 3 number with count from MOHFW
save_mcmc          <- TRUE          # output MCMC files (default = TRUE; needed for incidence CI calculations)
speed_lockdown     <- 7             # length of time for lockdown to drop (in days)
speed_return       <- 21            # length of time for pi to return to post-lockdown pi (in days)
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

india_recovered <- jhu_recovered %>%
  filter(`Country/Region` == "India") %>%
  select(-c(`Province/State`, "Lat", "Long")) %>%
  pivot_longer(names_to = "date", values_to = "count", -`Country/Region`) %>%
  mutate(date = as.Date(sub("X", "", date), format = "%m/%d/%y")) %>%
  filter(date >= start_date & date <= min_date)

india_deaths <- jhu_deaths %>%
  filter(`Country/Region` == "India") %>%
  select(-c(`Province/State`, "Lat", "Long")) %>%
  pivot_longer(names_to = "date", values_to = "count", -`Country/Region`) %>%
  mutate(date = as.Date(sub("X", "", date), format = "%m/%d/%y")) %>%
  filter(date >= start_date & date <= min_date)

india_cases <- jhu_cases %>%
  filter(`Country/Region` == "India") %>%
  select(-c(`Province/State`, "Lat", "Long")) %>%
  pivot_longer(names_to = "date", values_to = "count", -`Country/Region`) %>%
  mutate(date = as.Date(sub("X", "", date), format = "%m/%d/%y")) %>%
  filter(date >= start_date & date <= min_date)

NI_complete <- india_cases$count
RI_complete <- india_recovered$count + india_deaths$count
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# models ---------

if(arrayid==1){
  length_of_lockdown <- 21 + 7
  print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
  # 1 week extension ----------
  change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                          as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                          as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

  pi0         <- c(1,
                   rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                   rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                   seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                   pi_moderate)

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
    casename       = "India_4_l1",
    save_files     = FALSE,
    save_mcmc      = save_mcmc,
    save_plot_data = TRUE,
    M              = Ms,
    nburnin        = nburnins
  )
}
  
if(arrayid==2){
length_of_lockdown <- 21 + (7*2)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 2 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l2",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==3){
length_of_lockdown <- 21 + (7*3)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 3 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l3",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==4){
length_of_lockdown <- 21 + (7*4)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 4 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l4",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}
  

if(arrayid==5){
length_of_lockdown <- 21 + (7*5)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 5 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l5",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}
  

if(arrayid==6){
length_of_lockdown <- 21 + (7*6)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 6 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l6",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}


if(arrayid==7){
length_of_lockdown <- 21 + (7*7)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 7 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l7",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==8){
length_of_lockdown <- 21 + (7*8)
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# 8 week extension ----------
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_moderate, (pi_moderate - pi_lockdown) / speed_return),
                 pi_moderate)

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
  casename       = "India_4_l8",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

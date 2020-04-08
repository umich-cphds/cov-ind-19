# libraries  ----------
library(tidyverse)
library(httr)
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
replace_apr3       <- TRUE          # replace April 3 number with count from MOHFW
save_mcmc          <- TRUE          # output MCMC files (default = TRUE; needed for incidence CI calculations)
speed_lockdown     <- 7             # length of time for lockdown to drop (in days)
speed_return       <- 21            # length of time for pi to return to post-lockdown pi (in days)

# STATES
# dl = Delhi
# mh = Maharashtra
# kl = Kerala
state_sub <- "dl"

# data ----------
request <- GET("https://api.covid19india.org/states_daily.json")
json    <- content(request)
data      <- map_dfr(json[[1]], ~ .x)

state.codes <- setdiff(names(data), c("date", "status"))
data <- data %>% gather(!!state.codes, key = state, value = count) %>%
  mutate(count = as.numeric(count)) %>%
  spread(status, count) %>%
  filter(state == state_sub) %>%
  mutate(date = as.Date(date, "%d-%b-%y")) %>%
  arrange(date) %>%
  add_column(
    cases      = NA,
    deaths     = NA,
    recovereds = NA
  )

# populations from http://www.census2011.co.in/states.php
pops <- c("dl" = 16.8e6, "mh" = 112.4e6, "kl" = 33.4e6)

for (i in 1:dim(data)[1]) {
  data$cases[i]       <- sum(data$Confirmed[1:i], na.rm = T)
  data$deaths[i]      <- sum(data$Deceased[1:i], na.rm = T)
  data$recovereds[i]  <- sum(data$Recovered[1:i], na.rm = T)
}


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

NI_complete <- data$cases
RI_complete <- data$recovereds + data$deaths
N           <- pops[state_sub]                         # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)
start_date  <- min(data$date)

# dir.create(here("output", glue("{round(delay / 7, 0)}wk")), recursive = TRUE, showWarnings = FALSE)
# setwd(here("output", glue("{round(delay / 7, 0)}wk")))

# models ---------

if (arrayid == 1) {
print(glue("Running model_1 (perpetual lockdown) with {delay/7} week delay"))
# Model 1 ----------
# Model 1 Scenario: Travel ban + social distancing + lockdown
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date(as.Date("2020-03-11") + delay + 14 + speed_lockdown, origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown)),
                 pi_lockdown)

model_1 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_1"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}


if(arrayid == 2){
print(glue("Running model_2 (perpetual social distancing and travel ban) with {delay/7} week delay"))
# Model 2 ----------
# Model 2 Scenario: Travel ban + social distancing
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
# change_time <- c(format(seq(as.Date("2020/03/25"), as.Date("2020/04/07"), "days"), "%m/%d/%Y"),
#                  "04/08/2020")
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date(as.Date("2020-03-11") + delay + 14, origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1 - pi_sdtb) / 14))[-1],
                 pi_sdtb)

model_2 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_2"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==3){
print("Running model_3 (no intervention)")
# Model 3 ----------
# Model 3 Scenario: No intervention
model_3 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_3"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==4){
print(glue("Running model_4 (lockdown with moderate return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# Model 4 ----------
# Model 4 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (average)
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4 > 0.6
#   - May 20:              0.6
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
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_4"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==5){
print(glue("Running model_5 (lockdown with normal [pre-intervention] return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# Model 5 ----------
# Model 5 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (party)
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4 > 1
#   - May 20:              1
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_normal, (pi_normal - pi_lockdown) / speed_return),
                 pi_normal)

model_5 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_5"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if(arrayid==6){
print(glue("Running model_6 (lockdown with cautious return) with {delay/7} week delay and {length_of_lockdown}-day lockdown"))
# Model 6 ----------
# Model 6 Scenario: Travel ban + social distancing + lockdown,
#                   then release lockdown (scared)
#   - March 1 - March 25:  1
#   - March 25 - April 8:  1 > 0.75
#   - April 8 - April 15:  0.75 > 0.4
#   - April 29 - May 20:   0.4 > 0.6
#   - May 20:              0.6
change_time <- format(c(as.Date((as.Date("2020-03-11") + delay):(as.Date("2020-03-11") + delay + 13), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14):(as.Date("2020-03-11") + delay + 14 + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date("2020-03-11") + delay + 14 + length_of_lockdown):(as.Date("2020-03-11") + delay + 14 + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")

pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / 14))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_cautious, (pi_cautious - pi_lockdown) / speed_return),
                 pi_cautious)

model_6 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(start_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  pi0            = pi0,
  change_time    = change_time,
  R0             = R_0,
  dic            = TRUE,
  casename       = glue("{state_sub}_6"),
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

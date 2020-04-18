# libraries ----------
library(tidyverse)
library(here)
library(glue)
library(chron)
library(rjags)
library(reshape2)
library(gtools)     # rdirichlet(n, alpha)
library(scales)     # alpha　function
library(data.table)
library(devtools)
library(eSIR)

if ( Sys.getenv("production") == "TRUE" ) { 
        data_repo <- "~/cov-ind-19-data/"
        Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
        data_repo <- "~/cov-ind-19-test/"
        Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

today   <- Sys.getenv("today")
arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# specificatioons ----------
delay              <- 7             # in days (default = 7)
pi_cautious        <- 0.6           # pi corresponding to cautious return
pi_lockdown        <- 0.4           # pi corresponding to lockdown
pi_moderate        <- 0.75          # pi corresponding to moderate return
pi_normal          <- 1             # pi corresponding to normal (pre-intervention) return
pi_sdtb            <- 0.75          # pi corresponding to social distancing and travel ban
R_0                <- 2             # basic reproduction number
save_mcmc          <- FALSE         # output MCMC files (default = TRUE; needed for incidence CI calculations)
speed_lockdown     <- 14            # length of time for lockdown to drop (in days)
speed_return       <- 21            # length of time for pi to return to post-lockdown pi (in days)
start_date         <- "2020-03-01"
soc_dist_start     <- "2020-03-15"
soc_dist_end       <- "2020-03-24"
lockdown_start     <- as.Date(soc_dist_end) + 1
lockdown_end       <- "2020-05-03"
length_of_lockdown <- length(as.Date(lockdown_start):as.Date(lockdown_end))
l                  <- length(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"))

# data ----------
dat <- read_tsv(paste0(data_repo, today, "/jhu_data_mod.csv")) %>%
  filter(Country == "India" &  Date >= "2020-03-01" & Date <= "2020-04-14")

NI_complete <- dat$Cases
RI_complete <- dat$Recovered + dat$Deaths
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# directory ----------
wd <- paste0(data_repo, today, "/medium3/2wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# models ---------

if (arrayid == 1) {
# Social distancing model
print("Running social distancing and travel ban (model 2)...")
change_time <- format(c(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"),
                        as.Date(as.Date(soc_dist_start) + delay + l, origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1 - pi_sdtb) / l))[-1],
                 pi_sdtb)

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
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if (arrayid == 2) {
# No intervention model
print("Running no intervention (model 3)...")
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
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if (arrayid == 3) {
# Moderate return ----------
print("Running moderate return (model 4)...")
change_time <- format(c(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay):(as.Date(lockdown_start) + delay + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay + length_of_lockdown):(as.Date(lockdown_start) + delay + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / l))[-1],
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
  casename       = "India_4",
  save_files     = FALSE,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if (arrayid == 4) {  
# Normal (pre-intervenntion) ---------
print("Running normal (pre-intervention) return (model 5)...")
change_time <- format(c(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay):(as.Date(lockdown_start) + delay + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay + length_of_lockdown):(as.Date(lockdown_start) + delay + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / l))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_normal, (pi_normal - pi_lockdown) / speed_return),
                 pi_normal)

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
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

if (arrayid == 5) {
# Cautious return ----------
print("Running cautious return (model 6)...")
change_time <- format(c(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay):(as.Date(lockdown_start) + delay + speed_lockdown), origin = "1970-01-01"),
                        as.Date((as.Date(lockdown_start) + delay + length_of_lockdown):(as.Date(lockdown_start) + delay + length_of_lockdown + speed_return), origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1-pi_sdtb) / l))[-1],
                 rev(seq(pi_lockdown, pi_sdtb, (pi_sdtb-pi_lockdown) / speed_lockdown))[-1],
                 seq(pi_lockdown, pi_cautious, (pi_cautious - pi_lockdown) / speed_return),
                 pi_cautious)

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
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)
}

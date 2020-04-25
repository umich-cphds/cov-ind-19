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
set.seed(20192020) # default: 20192020

# specificatioons ----------
delay              <- 7             # in days (default = 7)
pi_cautious        <- 0.6           # pi corresponding to cautious return
pi_lockdown        <- 0.4           # pi corresponding to lockdown
pi_moderate        <- 0.75          # pi corresponding to moderate return
pi_normal          <- 1             # pi corresponding to normal (pre-intervention) return
pi_sdtb            <- 0.75          # pi corresponding to social distancing and travel ban
R_0                <- 2             # basic reproduction number
save_files         <- TRUE
save_mcmc          <- FALSE          # output MCMC files (default = TRUE; needed for incidence CI calculations)
speed_lockdown     <- 7             # length of time for lockdown to drop (in days)
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
  filter(Country == "India" &  Date >= "2020-03-01" & Date <= "2020-03-25")

NI_complete <- dat$Cases
RI_complete <- dat$Recovered + dat$Deaths
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# directory ----------
wd <- paste0(data_repo, today, "/medium3/1wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# models ---------

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
  casename       = "India_2_Mar25",
  save_files     = save_files,
  save_mcmc      = save_mcmc,
  save_plot_data = TRUE,
  M              = Ms,
  nburnin        = nburnins
)

# libraries ----------
library(tidyverse)
library(chron)
library(rjags)
library(gtools) #rdirichlet(n, alpha)
library(here)
library(devtools)

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

arrayid=Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# specificatioons ----------
pi_sdtb            <- 0.75          # pi corresponding to social distancing and travel ban
R_0                <- 2             # basic reproduction number
save_mcmc          <- TRUE          # output MCMC files (default = TRUE; needed for incidence CI calculations)
start_date         <- "2020-03-01"
soc_dist_start     <- "2020-03-15"
soc_dist_end       <- "2020-03-24"

# eSIR ----------
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE") # relevant model code

# directory ----------
today <- Sys.getenv("today")

# data ----------
dat <- read_tsv(paste0(data_repo, today, "/jhu_data_mod.csv")) %>%
  filter(Country == "India" &  Date >= "2020-03-01" & Date <= soc_dist_start)

NI_complete <- dat$Cases
RI_complete <- dat$Recovered + dat$Deaths
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

l <- length(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"))

# models ---------
if (arrayid == 1) {
wd <- paste0(data_repo, today, "/1wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

delay <- 7

print(paste0("Running model_2 (perpetual social distancing and travel ban) with", delay/7, "week delay"))

change_time <- format(c(as.Date((as.Date(soc_dist_start) + delay):(as.Date(soc_dist_end) + delay), origin = "1970-01-01"),
                        as.Date(as.Date(soc_dist_start) + delay + l, origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1 - pi_sdtb) / l))[-1],
                 pi_sdtb)

soc_dist <- tvt.eSIR(
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
wd <- paste0(data_repo, today, "/2wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

delay <- 14

print(paste0("Running model_2 (perpetual social distancing and travel ban) with", delay/7, "week delay"))

change_time <- format(c(as.Date((as.Date("2020-03-15") + delay):(as.Date("2020-03-15") + delay + 13), origin = "1970-01-01"),
                        as.Date(as.Date("2020-03-15") + delay + 14, origin = "1970-01-01")), "%m/%d/%Y")
pi0         <- c(1,
                 rev(seq(pi_sdtb, 1, (1 - pi_sdtb) / 14))[-1],
                 pi_sdtb)

soc_dist <- tvt.eSIR(
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

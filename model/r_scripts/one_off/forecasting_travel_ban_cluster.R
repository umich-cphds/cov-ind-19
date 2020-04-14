# libraries ----------
library(tidyverse)
library(chron)
library(rjags)
library(gtools) #rdirichlet(n, alpha)
library(scales) #alpha　function
library(data.table)
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
R_0                <- 2             # basic reproduction number
save_mcmc          <- TRUE          # output MCMC files (default = TRUE; needed for incidence CI calculations)
start_date         <- "2020-03-01"
travel_ban_date    <- "2020-03-15"

# eSIR ----------
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE") # relevant model code

# directory ----------
today <- Sys.getenv("today")
wd    <- paste0("~/cov-ind-19-data/", today, "/1wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# data ----------
dat <- read_tsv(paste0(data_repo, today, "jhu_data_mod.csv")) %>%
  filter(Country == "India" &  Date >= "2020-03-01" & Date <= travel_ban_date)

NI_complete <- dat$Cases
RI_complete <- dat$Recovered + dat$Deaths
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# models ---------
travel_ban <- tvt.eSIR(
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

# libraries ----------
suppressPackageStartupMessages({
  library(tidyverse)
  library(chron)
  library(rjags)
  library(gtools) #rdirichlet(n, alpha)
  library(here)
  library(eSIR)
  library(devtools)
})

data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
  Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
  Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

source(paste0(code_repo, "/model/r_scripts/cleanr_esir.R"))

today    <- as.Date(Sys.getenv("today"))
state    <- Sys.getenv("state")
arrayid  <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020
min_date <- today - 45

if (state == "ct") { state <- "cg" }
if (state == "ut") { state <- "uk" }

# specificatioons ----------
R_0                <- 2     # basic reproduction number
save_files         <- FALSE
save_mcmc          <- FALSE # output MCMC files (default = TRUE; needed for incidence CI calculations)
save_plot_data     <- FALSE

# data ----------
dat        <- covid19india::get_all_data()[abbrev == state & date >= min_date]
start_date <-  min(dat$date)

# directory ----------
wd <- paste0(data_repo, "/", today, "/1wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

NI_complete <- dat$total_cases
RI_complete <- dat$total_recovered + dat$total_deaths
N           <- covid19india::pop[abbrev == state, population] # population
R           <- unlist(RI_complete/N)                          # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# models ---------
if (arrayid == 1) {
  
  casename <- paste0(state, "_no_int")      
  
  model_3 <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_date, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = 200,
    R0             = R_0,
    dic            = TRUE,
    casename       = paste0(state, "_3"),
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <-suppressWarnings({model_3 %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "No intervention") })
  
}

write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))

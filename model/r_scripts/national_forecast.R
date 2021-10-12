# libraries ----------
suppressPackageStartupMessages({
  library(data.table)
  library(chron)
  library(rjags)
  library(gtools) #rdirichlet(n, alpha)
  library(here)
  library(devtools)
  library(eSIR)
})

today     <- as.Date(Sys.getenv("today"))
data_repo     <- Sys.getenv("data_repo")
code_repo     <- Sys.getenv("code_repo")

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) { 
        Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
        Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
        nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

source(paste0(code_repo, "/model/r_scripts/cleanr_esir.R"))

arrayid=Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020
min_date <- today - 45

# specifications ----------
R_0                <- 2             # basic reproduction number
save_files         <- FALSE
save_mcmc          <- FALSE         # output MCMC files (default = TRUE; needed for incidence CI calculations)
save_plot_data     <- FALSE

# directory ----------
wd <- paste0(data_repo, "/", today, "/1wk/")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}
setwd(wd)

# data ----------
dat <- covid19india::get_nat_counts()[date >= min_date]

NI_complete <- dat$total_cases
RI_complete <- dat$total_recovered + dat$total_deaths
N           <- 1.34e9                          # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

# models ---------

if (arrayid == 1) {
casename  <- "india_no_int"        
        
model_3 <- tvt.eSIR(
  Y,
  R,
  begin_str      = format(min_date, "%m/%d/%Y"),
  death_in_R     = 0.2,
  T_fin          = 200,
  R0             = R_0,
  dic            = TRUE,
  casename       = casename,
  save_files     = save_files,
  save_mcmc      = save_mcmc,
  save_plot_data = save_plot_data,
  M              = Ms,
  nburnin        = nburnins
)
        
clean_out <- cleanr_esir(f_out = model_3, N = N, adj = T, adj_len = 2, name = "No intervention")       

}

data.table::fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
data.table::fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")

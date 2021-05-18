source(paste0(code_repo, "/model/r_scripts/cleanr_covind/packages.R"))
source(paste0(code_repo, "/model/r_scripts/cleanr_covind/functions.R"))

set_seed <- 46342
set.seed(set_seed)

d <- Sys.getenv("today")

dat <- do_it_all(d = d)

#abbrevs <- get_abbrevs(dat) # old version
# source("../model/r_scripts/get_states.R") # integrated!
# abbrevs <- x$State
#f_dat <- get_forecast_data(d = d, ab = abbrevs)

cfr <- get_cfr(dat)

r0_est <- get_r_est(dat)

#write_csv(f_dat, file = paste0(data_repo, today, '/.csv'))

write_csv(dat, paste0(data_repo, "/", today, '/everything.csv'))

write_csv(cfr, paste0(data_repo, "/", today, '/cfr_t7_avg.csv'))

write_csv(r0_est, paste0(data_repo, "/", today, '/r0_t7_avg.csv'))

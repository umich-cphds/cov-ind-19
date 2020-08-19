source("scripts/packages.R")
source("scripts/functions.R")

set_seed <- 46342
set.seed(set_seed)

d <- "2020-08-18"

dat <- do_it_all(d = d)

abbrevs <- get_abbrevs(dat)

f_dat <- get_forecast_data(d = d, ab = abbrevs)

cfr <- get_cfr(dat)

r0_est <- get_r_est(dat)


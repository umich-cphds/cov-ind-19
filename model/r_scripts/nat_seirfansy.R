# looks for `data_repo`, `code_repo`, and `production` in environment
data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")

setwd(paste0(code_repo, "/model/r_scripts/"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(SEIRfansy)
  library(covid19india)
})

f <- c("clean_prediction.R", "get_impo.R", "get_init.R", "get_phase.R")
sapply(paste0("functions/", f), source)

# specs -----------
state    <- "tt" # as abbreviation; `tt` is the abbreviation for india in the data
t_pred   <- 50 # number of predicted days
plt      <- FALSE
save_plt <- FALSE

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	n_iter    <- 1e5
	burn_in   <- 1e5
	opt_num   <- 200
} else {
	n_iter    <- 1e3 #default 1e5
	burn_in   <- 1e2 #default 1e5
	opt_num   <- 20  #default 200
}

# auto-specs -----------
state_name <- covid19india::pop %>% dplyr::filter(abbrev == state) %>% pull(place)

data <- get_nat_counts()[, .(date, Confirmed = daily_cases, Recovered = daily_recovered, Deceased = daily_deaths)][]

max_date   <- data[, max(date)]
min_date   <- as.Date(max_date - 99)
obs_days   <- length(as.Date(min_date):as.Date(max_date))
N          <- covid19india::pop %>% filter(abbrev == state) %>% pull(population)

# prepare ----------
data_initial <- get_init(data)
data         <- data[date >= min_date]
mCFR         <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)
phases       <- get_phase(start_date = min_date, end_date = max_date)

# model ----------
tryCatch(
  expr = {
    # predict -----------
    result    <- SEIRfansy.predict(
      data            = abs(data %>% dplyr::select(-date)),
      init_pars       = NULL,
      data_init       = data_initial,
      T_predict       = t_pred,
      niter           = n_iter,
      BurnIn          = burn_in,
      model           = "Multinomial",
      N               = N,
      lambda          = 1/(69.416 * 365),
      mu              = 1/(69.416 * 365),
      period_start    = phases,
      opt_num         = opt_num,
      auto.initialize = T,
      alpha_u         = 0.5,
      f               = 0.15,
      plot            = plt,
      save_plots      = save_plt
    )
    
    # directory ----------
    wd <- paste0(data_repo, "/", max_date, "/seirfansy")
    if (!dir.exists(wd)) {
      dir.create(wd, recursive = TRUE)
      message("Creating ", wd)
    }
    
    # output ----------
    def_obs_days <- length(data[, date])
    obs_dates    <- data[, date]
    
    pred_clean <- clean_prediction(result$prediction,
                                   state     = state_name,
                                   obs_days  = def_obs_days,
                                   obs_dates = obs_dates,
                                   t_pred    = t_pred)
    
    write_tsv(pred_clean, paste0(wd, "/prediction_", state, "_", format(max_date, "%Y%m%d"), ".txt"))
    write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, "_", format(max_date, "%Y%m%d"),".txt"))
    
    write_tsv(pred_clean, paste0(wd, "/prediction_", state, "_latest.txt"))
    write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, "_latest.txt"))
    
    p_pred <- pred_clean %>%
      filter(section == "positive_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    r_pred <- pred_clean %>%
      filter(section == "recovered_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    d_pred <- pred_clean %>%
      filter(section == "death_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    t_d <- r_pred + d_pred + p_pred
    total_pred <- rowSums(matrix(rowMeans(result$prediction), nrow = obs_days + t_pred)[, 3:9])
    UF_p <- total_pred / t_d
    
    d_u <- pred_clean %>%
      filter(section == "death_unreported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    total_death <- d_u + d_pred
    UF_d <- total_death / d_pred
    ifr <- total_death / total_pred
    
    impo <- tibble(
      "underreporting_cases"  = UF_p[obs_days + 1],
      "underreporting_deaths" = UF_d[obs_days + 1],
      "ifr"                   = ifr[obs_days + 1]
    )
    
    write_tsv(impo, paste0(wd, "/important_", state, "_", format(max_date, "%Y%m%d"), ".txt"))
    write_tsv(impo, paste0(wd, "/important_", state,"_latest.txt"))
    message("Successfully executed the call.")
  },
  error = function(e){
    message('Caught an error!')
    print(e)
  }
)

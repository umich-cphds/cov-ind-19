data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")
today <- as.Date(Sys.getenv("today"))

setwd(paste0(code_repo, "/model/r_scripts/"))
source("libraries.R")

f <- list.files(paste0(code_repo, "/model/r_scripts/functions"))
sapply(paste0("functions/", f), source)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	n_iter    <- 1e5
	burn_in   <- 1e5
	opt_num   <- 200
} else {
	n_iter    <- 1e3 #default 1e5
	burn_in   <- 1e2 #default 1e5
	opt_num   <- 1   #default 200
}

# specs -----------
state    <- Sys.getenv("state")
max_date <- as.Date(today - 1)
min_date <- as.Date(max_date - 99)
obs_days <- length(as.Date(min_date):as.Date(max_date))
t_pred   <- 150 # number of predicted days
N        <- get_pop(state)
plt      <- FALSE
save_plt <- FALSE

# load and prepare ----------
#danbarke should change this to use a prepull data job and store the data locally. This will ensure all of the jobs are using the same data each time the workload is run.
data <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                        col_types = cols()) %>%
  janitor::clean_names() %>%
  dplyr::select("date" = "date_ymd", "status", "val" = tolower(state)) %>%
  arrange(date) %>%
  tidyr::pivot_wider(
    names_from  = "status",
    values_from = "val",
    id_cols = "date"
  ) %>%
  dplyr::filter(date <= max_date)

data_initial <- get_init(data)

data <- data %>% dplyr::filter(date >= min_date)

mCFR <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)

phases <- get_phase(start_date = min_date, end_date = max_date)

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
wd <- paste0(data_repo, "/", today, "/seirfansy")
if (!dir.exists(wd)) {
  dir.create(wd, recursive = TRUE)
  message("Creating ", wd)
}

pred_clean <- clean_prediction(result$prediction,
                               state    = pop %>% filter(abbrev == tolower(state)) %>% pull(full) %>% unique(),
                               obs_days = obs_days,
                               t_pred   = t_pred)

write_tsv(pred_clean, paste0(wd, "/prediction_", state, ".txt"))
write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, ".txt"))

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

write_tsv(impo, paste0(wd, "/important_", state, ".txt"))

library(dplyr)
library(arm)
library(data.table)
source("par_initializeR.r")
source("model_estimateR.r")
source("model_initializeR.r")
source("mcmc_performR.r")
source("model_deterministic_simulateR.r")
source("R0_calculateR.r")
source("model_predictR.r")
source("model_stochastic_simulateR.r")
source("model_plotR.r")
source("get_init.R")
source("get_phase.R")
source("get_impo.R")
source("clean_prediction.R")

#install.packages("remotes")
#remotes::install_gitlab("maxsal/ally")
ally::libri(tidyverse, janitor, arm, covid19india, glue, data.table, cli)

#phases = c(as.Date("2020-12-09"),as.Date("2021-01-01"), as.Date("2021-01-16"),
#as.Date("2021-02-01"),as.Date("2021-02-16"), as.Date("2021-03-01"),as.Date("2021-03-12"))
# specs -----------
state       <- "tt" # as abbreviation; `tt` is the abbreviation for india in the data
t_pred      <- 50 # number of predicted days
alpha_u_val <- 0.3
f_val       <- 0.15  # false positivity rate
plt         <- FALSE
save_plt    <- FALSE
production  <- TRUE

# Set variables based on testing or production
if (production == TRUE) {
  n_iter    <- 3e5
  burn_in   <- 3e5
  opt_num   <- 200
} else {
  n_iter    <- 5e2 #default 1e5
  burn_in   <- 5e2 #default 1e5
  opt_num   <- 20   #default 200
}

# auto-specs -----------
state_name <- covid19india::pop[abbrev == state, place]

data <- get_nat_counts()[, .(date, Confirmed = daily_cases, Recovered = daily_recovered, Deceased = daily_deaths)]

max_date   <- data[, max(date)]
min_date   <- as.Date(max_date - 99)
obs_days   <- length(as.Date(min_date):as.Date(max_date))
N          <- covid19india::pop %>% filter(abbrev == state) %>% pull(population)

# prepare ----------
data_initial <- get_init(data)
data         <- data[date >= min_date]
mCFR         <- data[(.N-55):.N][, lapply(.SD, sum), .SDcols = c("Recovered", "Deceased")][, mCFR := Deceased / (Deceased + Recovered)][, mCFR][]
# mCFR         <- data[(.N-20):.N][, lapply(.SD, sum), .SDcols = c("Recovered", "Deceased")][, mCFR := Deceased / (Deceased + Recovered)][, mCFR][]
# mCFR         <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)
phases       <- get_phase(start_date = min_date, end_date = max_date)


result <- model_predictR(
  data            = abs(data[, !c("date")]),
  init_pars       = NULL,
  data_init       = data_initial,
  T_predict       = t_pred, 
  niter           = n_iter,
  BurnIn          = burn_in,
  model           = "Poisson",
  mCFR            = mCFR,
  N               = N, 
  lambda          = 1/(69.416 * 365), 
  De              = 3,
  Dr              = 7,
  mu              = 1/(69.416 * 365),
  delta_1         = 0.135,
  period_start    = phases,
  opt_num         = opt_num,
  auto.initialize = TRUE,
  alpha_u         = alpha_u_val,
  f               = f_val,
  plot            = plt,
  save_plots      = save_plt)

# saveRDS(Result,"Result8.rds")

def_obs_days <- length(data[, date])
obs_dates    <- data[, date]

pred_clean <- clean_prediction(result$prediction,
                               state     = state_name,
                               obs_days  = def_obs_days,
                               obs_dates = obs_dates,
                               t_pred    = t_pred) |>
  as.data.table()

fwrite(pred_clean, "~/Downloads/seir_poisson_d10.135_t8_20220128.csv")

# quick plots ----------
case_plot <- rbindlist(list(
  pred_clean[section == "positive_daily_reported"][, .(date, cases = mean, pred)][, scenario := fifelse(pred == 1, "prediction", "training")][],
  data[, .(date, cases = Confirmed, scenario = "observed")]
), fill = TRUE, use.names = TRUE)[date <= (data[, max(date)] + 30)] |>
  ggplot(aes(x = date, y = cases, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title   = "SEIRfansy predicted case counts in India",
    subtitle = glue("{data[, min(date)]} to {data[, max(date)]}; predicting 30 days through {data[, max(date)] + 30}"),
    x       = "Date",
    y       = "Daily reported case count",
    caption = glue("Initial values: f = {f_val}; alpha_u = {alpha_u_val}")
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top"
  )

# ggsave(
#   filename = paste0(wd, "/seirfansy_national_cases_latest.pdf"),
#   plot = case_plot,
#   width = 7, height = 5, device = cairo_pdf)

death_plot <- rbindlist(list(
  pred_clean[section == "death_daily_reported"][, .(date, deaths = mean, pred)][, scenario := fifelse(pred == 1, "prediction", "training")][],
  data[, .(date, deaths = Deceased, scenario = "observed")]
), fill = TRUE, use.names = TRUE)[date <= (data[, max(date)] + 30)] |>
  ggplot(aes(x = date, y = deaths, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title   = "SEIRfansy predicted death counts in India",
    subtitle = glue("{data[, min(date)]} to {data[, max(date)]}; predicting 30 days through {data[, max(date)] + 30}"),
    x       = "Date",
    y       = "Daily reported death count",
    caption = glue("Initial values: f = {f_val}; alpha_u = {alpha_u_val}")
  ) +
  theme_classic() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top"
  )
# ggsave(
#   filename = paste0(wd, "/seirfansy_national_deaths_latest.pdf"),
#   plot = death_plot,
#   width = 7, height = 5, device = cairo_pdf)

case_bar_plot <- pred_clean[section %in% c("unreported_daily", "positive_daily_reported") & pred == 1][
  section == "unreported_daily", reported := "Unreported"][
    section == "positive_daily_reported", reported := "Reported"][
      , reported := factor(reported, c("Unreported", "Reported"))][] |>
  ggplot(aes(x = date, y = mean, fill = reported)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_labels = "%B %e") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date",
    y = "Daily counts",
    title = "Predicted daily reported and unreported COVID-19 case counts"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold")
  )
# ggsave(
#   filename = paste0(wd, "/seirfansy_national_cases_bar_latest.pdf"),
#   plot = case_bar_plot,
#   width = 7, height = 5, device = cairo_pdf)

death_dat <- pred_clean[section %in% c("death_unreported", "death_daily_reported") & pred == 1][section == "death_daily_reported", daily := mean][section == "death_unreported", daily := mean - shift(mean)][]

death_bar_plot <- death_dat[
  section == "death_unreported", reported := "Unreported"][
    section == "death_daily_reported", reported := "Reported"][
      , reported := factor(reported, c("Unreported", "Reported"))][] |>
  ggplot(aes(x = date, y = daily, fill = reported)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_labels = "%B %e") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date",
    y = "Daily counts",
    title = "Predicted daily reported and unreported COVID-19 death counts"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold")
  )
# ggsave(
#   filename = paste0(wd, "/seirfansy_national_deaths_bar_latest.pdf"),
#   plot = case_plot,
#   width = 7, height = 5, device = cairo_pdf)

library(patchwork)

patch <- (case_plot + death_plot) /
  (case_bar_plot + death_bar_plot)
ggsave(filename = "~/Downloads/seir_poisson_d10.135_t8_20220128.pdf",
       plot = patch,
       height = 8, width = 15, device = cairo_pdf)

# looks for `data_repo`, `code_repo`, and `production` in environment
data_repo <- Sys.getenv("data_repo")
code_repo <- Sys.getenv("code_repo")

setwd(paste0(code_repo, "/model/r_scripts/"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(arm)
  library(covid19india)
  library(glue)
  library(data.table)
  library(cli)
})

f <- c("clean_prediction.R", "get_impo.R", "get_init.R", "get_phase.R")
# sapply(paste0("../model/r_scripts/functions/", f), source)
sapply(paste0("functions/", f), source)

# g <- list.files("../model/r_scripts/seir")
g <- list.files("seir/")
# sapply(paste0("../model/r_scripts/seir/", g), source)
sapply(paste0("../r_scripts/seir/", g), source)

# specs -----------
state       <- "tt" # as abbreviation; `tt` is the abbreviation for india in the data
t_pred      <- 50 # number of predicted days
alpha_u_val <- 0.5
f_val       <- 0.15  # false positivity rate
plt         <- FALSE
save_plt    <- FALSE

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
  n_iter    <- 3e5
  burn_in   <- 3e5
  opt_num   <- 200
} else {
  n_iter    <- 1e3 #default 1e5
  burn_in   <- 1e2 #default 1e5
  opt_num   <- 20  #default 200
}

# auto-specs -----------
state_name <- covid19india::pop[abbrev == state, place]

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
result <- model_predictR(
  data            = abs(data[, !c("date")]),
  init_pars       = NULL,
  data_init       = data_initial,
  T_predict       = t_pred,
  De              = 3,
  pi              = rep(1, t_pred),
  niter           = n_iter,
  BurnIn          = burn_in,
  model           = "Multinomial",
  N               = N,
  lambda          = 1/(69.416 * 365),
  mu              = 1/(69.416 * 365),
  period_start    = phases,
  opt_num         = opt_num,
  auto.initialize = TRUE,
  alpha_u         = alpha_u_val,
  f               = f_val,
  plot            = plt,
  save_plots      = save_plt
  )

# directory ----------
wd <- paste0(data_repo, "/source_data/seir")
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
                               t_pred    = t_pred) |>
  as.data.table()

write_tsv(pred_clean, paste0(wd, "/prediction_", state, "_", format(max_date, "%Y%m%d"), ".txt"))
write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, "_", format(max_date, "%Y%m%d"),".txt"))

write_tsv(pred_clean, paste0(wd, "/prediction_", state, "_latest.txt"))
write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, "_latest.txt"))

# extract underreporting ----------
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

ggsave(
  filename = paste0(wd, "/seirfansy_national_cases_latest.pdf"),
  plot = case_plot,
  width = 7, height = 5, device = cairo_pdf)

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
ggsave(
  filename = paste0(wd, "/seirfansy_national_deaths_latest.pdf"),
  plot = death_plot,
  width = 7, height = 5, device = cairo_pdf)

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
ggsave(
  filename = paste0(wd, "/seirfansy_national_cases_bar_latest.pdf"),
  plot = case_bar_plot,
  width = 7, height = 5, device = cairo_pdf)

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
ggsave(
  filename = paste0(wd, "/seirfansy_national_deaths_bar_latest.pdf"),
  plot = case_plot,
  width = 7, height = 5, device = cairo_pdf)


cli::cli_alert_success("*beep boop* Done!!!")

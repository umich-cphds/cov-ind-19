# libraries ----------
library(tidyverse)
library(scales)

options(stringsAsFactors=FALSE)

# directories ----------

arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today, "/", arrayid, "wk")
setwd(wd)
getwd()

# data ----------
jhu_cases <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

india_cases     <- jhu_cases[jhu_cases$Country.Region == "India", ]
latest_date     <- as.Date(sub("X", "", rev(names(india_cases))[1]), format("%m.%d.%y"))
dates_1         <- seq(as.Date("03-01-2020", format = "%m-%d-%y"), latest_date, "days")
dataf           <- unlist(india_cases[-(1:43)])
N               <- 1.34e9
forecast_length <- 200
forecast_dt     <- seq(from = as.Date(dates_1[length(dates_1)] + 1, format = "%m-%d-%y"), to = as.Date(dates_1[length(dates_1)] + forecast_length, format = "%m-%d-%y"), "days")
adj_len         <- 2
adj             <- T
plot_start_date <- "2020-04-30"
plot_end_date   <- "2020-08-31"

# function ----------
fig_5_data <- function(x) {
  load(x)
  
  T_prime     <- plot_data_ls[[2]][[1]]
  y_text_ht   <- plot_data_ls[[4]][[1]]
  data_comp   <- plot_data_ls[[4]][[3]]
  data_comp_R <- plot_data_ls[[5]][[3]]
  
  india_confirm    <- round(N * (data_comp[(T_prime + 1):(T_prime + forecast_length), "mean"] +
                            data_comp_R[(T_prime + 1):(T_prime + forecast_length),"mean"]))
  india_confirm_up <- round(N*(data_comp[(T_prime + 1):(T_prime + forecast_length),"upper"] +
                            data_comp_R[(T_prime + 1):(T_prime + forecast_length),"upper"]))
  if(adj == T) {
    adj_v            <- mean(as.vector(dataf[(T_prime - adj_len):T_prime]) / N / (data_comp[(T_prime - adj_len):T_prime, "mean"] +
                             data_comp_R[(T_prime - adj_len):T_prime, "mean"]))
    india_confirm_up <- round(india_confirm_up * adj_v)
    india_confirm    <- round(india_confirm * adj_v)
  }
  return(list(india_confirm, india_confirm_up))
}

# processing ----------
mod_2    <- fig_5_data("India_2_plot_data.RData")
mod_2_up <- mod_2[[2]]
mod_2    <- mod_2[[1]]

mod_3    <- fig_5_data("India_3_plot_data.RData")
mod_3_up <- mod_3[[2]]
mod_3    <- mod_3[[1]]

mod_4    <- fig_5_data("India_4_plot_data.RData")
mod_4_up <- mod_4[[2]]
mod_4    <- mod_4[[1]]

mod_5    <- fig_5_data("India_5_plot_data.RData")
mod_5_up <- mod_5[[2]]
mod_5    <- mod_5[[1]]

mod_6    <- fig_5_data("India_6_plot_data.RData")
mod_6_up <- mod_6[[2]]
mod_6    <- mod_6[[1]]

observed_plot <- tibble(
  Dates    = dates_1, 
  variable = rep("True", length(dates_1)), 
  value    = dataf
  )

forecasts_plot <- tibble(
  Dates    = forecast_dt,
  mod_2    = mod_2,
  mod_3    = mod_3,
  mod_4    = mod_4,
  mod_5    = mod_5,
  mod_6    = mod_6,
  ) %>%
  gather(variable, value, -Dates)

forecasts_plot_ci <- tibble(
  Dates    = forecast_dt,
  mod_2 = mod_2_up,
  mod_3 = mod_3_up,
  mod_4 = mod_4_up,
  mod_5 = mod_5_up,
  mod_6 = mod_6_up,
  ) %>%
  gather(variable, upper_ci, -Dates)

forecasts_plot <- left_join(forecasts_plot, forecasts_plot_ci, by = c("Dates", "variable"))

connect_plot <- tibble(
  Dates    = rep(as.Date("03-23-2020", format = "%m-%d-%y"), 5),
  variable = c("mod_2", "mod_3", "mod_4", "mod_5", "mod_6"),
  value    = rep(499, 5)
  )

complete_plot <- bind_rows(observed_plot,
                           forecasts_plot,
                           connect_plot) %>%
  mutate(variable = as.factor(variable)) %>%
  arrange(Dates) %>%
  mutate(
    color = as.factor(case_when(
      variable == "True" ~ "Observed",
      variable == "mod_2" ~ "Soc. Dist. + Travel Ban",
      variable == "mod_3" ~ "No Intervention",
      variable == "mod_4" ~ "Moderate return",
      variable == "mod_5" ~ "Normal (pre-intervention)",
      variable == "mod_6" ~ "Cautious return",
      variable == "Limit" ~ "Limit"
    )),
    type = as.factor(if_else(variable == "Limit", "dashed", "solid"))
  )

write_csv(complete_plot, path = "./figure_5_data.csv")

complete_plot <- complete_plot %>%
  filter(Dates <= as.Date(plot_end_date, format = "%Y-%m-%d") & Dates >= as.Date(plot_start_date, format = "%Y-%m-%d")) %>%
  filter(color != "No Intervention")

complete_plot_solid <- complete_plot %>% filter(type == "solid")
complete_plot_dash  <- complete_plot %>% filter(type == "dashed")

# plot ----------
color_values <- c("Soc. Dist. + Travel Ban"   = "#f2c82e",
                  "No Intervention"           = "#ED553B",
                  "Moderate return"           = "#0472cf",
                  "Normal (pre-intervention)" = "#3CAEA3",
                  "Cautious return"           = "#173F5F",
                  Limit                       = "#3c4c55",
                  Observed                    = "black")

my_title    <- paste0("Predicted number of COVID-19 infections")
my_subtitle <- paste0("as of ", format(latest_date, "%d %B, %Y"))

p1 <- ggplot(data = complete_plot_solid, mapping = aes(x = Dates, y = value * 100000 / !!N, group = variable, color = color)) +
  geom_smooth(se = FALSE, span = 0.2) +
  scale_color_manual(values = color_values) +
  scale_x_date(labels = date_format("%b %d")) +
  # ylim(0, 250) +
  labs(title    = my_title,
       subtitle = my_subtitle,
       y        = "Number of infected cases per 100,000 people in India",
       x        = "Date",
       color    = "Scenario",
       caption  = "\uA9 COV-IND-19 Study Group") +
  theme_minimal() +
  theme(axis.text.x      = element_text(angle = 45, vjust = 0.5, size = 15),
        axis.text.y      = element_text(size = 15),
        plot.title       = element_text(size = 22),
        legend.position  = "bottom",
        plot.caption     = element_text(color = "blue",face = "bold"))

p1

write_rds(p1, "./Figure5.Rds")

# libraries ----------
library(tidyverse)
options(stringsAsFactors = FALSE)

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
plot_start_date <- "2020-03-01"
plot_end_date   <- "2020-04-30"

fig_4_data <- function(x) {
  load(x)

  T_prime     <- plot_data_ls[[2]][[1]]
  y_text_ht   <- plot_data_ls[[4]][[1]]
  data_comp   <- plot_data_ls[[4]][[3]]
  data_comp_R <- plot_data_ls[[5]][[3]]

  india_confirm <- round(N * (data_comp[(T_prime + 1):(T_prime + forecast_length), "mean"] +
                                 data_comp_R[(T_prime + 1):(T_prime + forecast_length), "mean"]))
  india_confirm_up <- round(N*(data_comp[(T_prime + 1):(T_prime + forecast_length), "upper"] +
                                    data_comp_R[(T_prime + 1):(T_prime + forecast_length), "upper"]))
  if (adj == TRUE) {
    adj_v <- mean(as.vector(dataf[(T_prime - adj_len):T_prime])/N/(data_comp[(T_prime - adj_len):T_prime, "mean"] +
                            data_comp_R[(T_prime - adj_len):T_prime, "mean"]))

    india_confirm_up <- round(india_confirm_up * adj_v)
    india_confirm    <- round(india_confirm * adj_v)
  }
  return(list(india_confirm, india_confirm_up))
}

# processing ----------
mod_2    <- fig_4_data("India_2_plot_data.RData")
mod_2_up <- mod_2[[2]]
mod_2    <- mod_2[[1]]

mod_3    <- fig_4_data("India_3_plot_data.RData")
mod_3_up <- mod_3[[2]]
mod_3    <- mod_3[[1]]

mod_4    <- fig_4_data("India_4_plot_data.RData")
mod_4_up <- mod_4[[2]]
mod_4    <- mod_4[[1]]

observed_plot  <- tibble(
  Dates    = dates_1,
  variable = rep("True", length(dates_1)),
  value    = dataf
  )

forecasts_plot <- tibble(
  Dates    = forecast_dt,
  mod_2    = mod_2,
  mod_3    = mod_3,
  mod_4    = mod_4,
  ) %>%
  gather(variable, value, -Dates)

forecasts_plot_ci <- tibble(
  Dates    = forecast_dt,
  mod_2 = mod_2_up,
  mod_3 = mod_3_up,
  mod_4 = mod_4_up,
  ) %>%
  gather(variable, upper_ci, -Dates)

forecasts_plot <- left_join(forecasts_plot, forecasts_plot_ci, by = c("Dates", "variable"))

complete_plot <- bind_rows(observed_plot,
                           forecasts_plot) %>%
  mutate(variable = as.factor(variable)) %>%
  arrange(Dates) %>%
  mutate(
    color = as.factor(case_when(
      variable == "True" ~ "Observed",
      variable == "mod_2" ~ "Social distancing",
      variable == "mod_3" ~ "No intervention",
      variable == "mod_4" ~ "Lockdown with moderate release",
      variable == "Limit" ~ "Limit"
    )),
    type = as.factor(if_else(variable == "Limit", "dashed", "solid"))
  )

write_csv(complete_plot, path = "./figure_4_data.csv")

ymax        <- log(4e6)
my_title    <- paste0("COVID-19 Cumulative Cases by Day for India")
my_subtitle <- paste0("as of ", format(latest_date, "%d %B %Y"))
mybreaks    <- seq(0, ymax, length.out = 10)

complete_plot <- complete_plot %>%
  filter(Dates <= as.Date(plot_end_date, format = "%Y-%m-%d"), Dates >= as.Date(plot_start_date, format = "%Y-%m-%d"))

f4plotdata <-  complete_plot

options(scipen=10000)

p1 <- ggplot(complete_plot %>% filter(color == "Observed"), aes(Dates, log(value))) +
  geom_bar(stat = 'identity', fill = '#979799') +
  geom_bar(data = f4plotdata %>% filter(color == "No intervention"), aes(Dates, log(value)), # no intervention
           stat = 'identity', fill = '#ED553B') +
  geom_bar(data = f4plotdata %>% filter(color == "Social distancing"), aes(Dates, log(value)), # social distancing
           stat = 'identity', fill = '#f2c82e') +
  geom_bar(data = f4plotdata %>% filter(color == "Lockdown with moderate release"), aes(Dates, log(value)), # lockdown
           stat = 'identity', fill = '#173F5F') +
  labs(title    = my_title,
       subtitle = my_subtitle,
       caption  = "© COV-IND-19 Study Group",
       x        = "Date",
       y        = "Cumulative number of cases") +
  theme_minimal() +
  scale_y_continuous(breaks   = mybreaks,
                     limits   = c(0, ymax),
                     labels   = format(round(exp(mybreaks), 0), big.mark = ","),
                     sec.axis = sec_axis(~., name = "", breaks = mybreaks, labels = format(round(exp(mybreaks), 0), big.mark = ","))) +
  theme(legend.position   = "bottom",
        axis.text.x       = element_text(angle=45,vjust=0.5,size=15),
        axis.text.y       = element_text(size=15, hjust = 0),
        plot.title        = element_text(size=22),
        plot.caption      = element_text(size=12,face="bold",color="blue")) +
  geom_line(data = complete_plot %>% filter(variable == "mod_4"),
            aes(x = Dates, y = log(upper_ci)),
            stat     = "identity",
            linetype = 2,
            size     = 1,
            color    = "#264652") +
  geom_vline(aes(xintercept = !!forecast_dt[1]),
             linetype   = 2,
             alpha      = 0.6,
             size       = 1) +
  annotate(geom="text",x=as.Date("2020-03-10"),
           y=6,label="Observed",fontface="bold", color = '#979799') +
  annotate(geom="text",x=as.Date("2020-04-10"),
           y=12,label="No intervention",fontface="bold", color = '#ED553B') +
  annotate(geom="text",x=as.Date("2020-04-01"),
           y=10,label="Social distancing",fontface="bold", color = '#f2c82e') +
  annotate(geom="text",x=as.Date("2020-03-26"),
           y=8,label="Lockdown with moderate release",fontface="bold", color = '#173F5F')

p1

write_rds(p1, "./Figure4.Rds")

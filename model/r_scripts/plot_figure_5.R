# libraries ----------
library(tidyverse)
require(reshape2)

options(stringsAsFactors=FALSE)

# functions ----------
LineType=function(x) {
  if(x=="Limit") {
    return("dashed")
  } else {
    return("solid")
  }
}

LineColor=function(x)
{
  if(x=="True") {
    return("black")
  }
  if(x=="mod_2") {
    return("red")
  }
  if(x=="mod_3") {
    return("green")
  }
  if(x=="mod_4") {
    return("blue")
  }
  if(x=="mod_5") {
    return("yellow")
  }
  if(x=="mod_6") {
    return("purple")
  }
  if(x=="Limit") {
    return("grey")
  }
}

# directories ----------
#wd <- "/Users/maxsalvatore/Downloads/Codes/test/test_0705"
today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today, "/1wk")
setwd(wd)

# data ----------
jhu_cases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
# jhu_cases=read.csv("JHU Cases.csv")
# This dataset above need to be updated from https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases.

india_cases     <- jhu_cases[jhu_cases$Country.Region == "India", ]
latest_date     <- as.Date(sub("X", "", rev(names(india_cases))[1]), format("%m.%d.%y"))
dates_1         <- seq(as.Date("03-01-2020", format = "%m-%d-%y"), latest_date, "days")
dataf           <- unlist(india_cases[-(1:43)])
N               <- 1.38e9
forecast_length <- 120
forecast_dt     <- seq(from = as.Date(dates_1[length(dates_1)] + 1, format = "%m-%d-%y"), to = as.Date(dates_1[length(dates_1)] + forecast_length, format = "%m-%d-%y"), "days")
adj_len         <- 2
adj             <- T
plot_end_date   <- "2020-06-01"
# Now we just load the output files from the models for each case and use them to create the plot.

fig_4_data <- function(x) {
  load(x) # Assuming the .RDA files are in the same directory and you only need to supply the name. Change with the directories accordingly.

  other_plot       <- plot_data_ls[[2]]
  T_fin            <- other_plot[[2]]
  T_prime          <- other_plot[[1]]
  chron_ls         <- other_plot[[3]]
  R0_p_mean        <- other_plot[[10]]
  beta_p_mean      <- other_plot[[8]]
  dthetaI_tp1      <- other_plot[[4]]
  dthetaI_tp2      <- other_plot[[5]]
  gamma_p_mean     <- other_plot[[9]]
  dthetaI_tp1_date <- other_plot[[6]]
  dthetaI_tp2_date <- other_plot[[7]]

  spaghetti_plot_ls       <- plot_data_ls[[3]]
  spaghetti_ht            <- spaghetti_plot_ls[[1]]
  dthetaI_mean_data       <- spaghetti_plot_ls[[2]]
  sample_dthetaI_mat_long <- spaghetti_plot_ls[[3]]
  second_tp_date_ci       <- spaghetti_plot_ls[[5]]
  first_tp_date_ci        <- spaghetti_plot_ls[[4]]

  infection_plot_ls <- plot_data_ls[[4]]
  y_text_ht         <- infection_plot_ls[[1]]
  data_poly         <- infection_plot_ls[[2]]
  data_comp         <- infection_plot_ls[[3]]
  data_pre          <- infection_plot_ls[[4]]

  removed_plot_ls <- plot_data_ls[[5]]
  r_text_ht       <- removed_plot_ls[[1]]
  data_poly_R     <- removed_plot_ls[[2]]
  data_comp_R     <- removed_plot_ls[[3]]
  data_pre_R      <- removed_plot_ls[[4]]

  india_confirm <- round(N * (data_comp[(T_prime + 1):(T_prime + forecast_length), "mean"] +
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

mod_2 = fig_4_data("India_2_plot_data.RData")[[1]]
mod_3 = fig_4_data("India_3_plot_data.RData")[[1]]
mod_4 = fig_4_data("India_4_plot_data.RData")[[1]]
mod_5 = fig_4_data("India_5_plot_data.RData")[[1]]
mod_6 = fig_4_data("India_6_plot_data.RData")[[1]]

observed_plot  <- data.frame(Dates = dates_1, variable = rep("True", length(dates_1)), value = dataf)
forecasts      <- data.frame(Dates = forecast_dt, mod_2, mod_3, mod_4, mod_5, mod_6)
forecasts_plot <- melt(forecasts, id = "Dates")

connect_plot  <- data.frame(Dates    = rep(as.Date("03-23-2020", format = "%m-%d-%y"), 5),
                            variable = c("mod_2", "mod_3", "mod_4", "mod_5", "mod_6"),
                            value    = rep(499, 5))
limit_plot    <- data.frame(Dates    = c(dates_1, forecast_dt),
                            variable = rep("Limit", length(c(dates_1, forecast_dt))),
                            value    = rep(0.7 * N / 1000, length(c(dates_1, forecast_dt))))
complete_plot <- rbind(observed_plot, forecasts_plot, connect_plot, limit_plot)
complete_plot <- complete_plot[order(complete_plot$Dates), ]

rownames(complete_plot) <- NULL
complete_plot$color     <- unlist(lapply(complete_plot$variable, LineColor))
complete_plot$type      <- unlist(lapply(complete_plot$variable, LineType))

complete_plot <- complete_plot %>%
  filter(Dates <= as.Date(plot_end_date, format = "%Y-%m-%d"))

plt <- ggplot(data = complete_plot, mapping = aes(x = Dates, y = value * 100000 / !!N, group = variable, color = color)) +
  geom_line(aes(linetype = type), size = 1.35) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("black", "blue", "green", "grey", "red", "yellow", "purple")) +
  ylab("Number of infected cases per 100,000 people in India") +
  labs(caption = "\uA9 COV-IND-19 Study Group") +
  ggtitle(paste0("Predicted number of COVID19 infections based on data up to ", latest_date)) +
  # guides(linetype = FALSE, color = FALSE) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 15),
        axis.text.y  = element_text(size = 15),
        plot.title   = element_text(size = 22),
        plot.caption = element_text(color = "blue",face = "bold"))

write_rds(plt, "./Figure5.Rds")

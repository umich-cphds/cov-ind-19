# libraries ----------
library(tidyverse)
require(reshape2)
options(stringsAsFactors = FALSE)

# function ----------
LineColor = function(x) {
  if (x == "True") {
    return("black")
  }
  if (x == "mod_3") {
    return("red")
  }
  if (x == "mod_2") {
    return("green")
  }
  if (x == "mod_1") {
    return("blue")
  }
  if (x == "mod_1_up") {
    return("blue")
  }
}

# directories ----------
# wd <- "/Users/maxsalvatore/Downloads/Codes/test/test_0705"
today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today)
setwd(wd)

# data ----------
jhu_cases <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
# This dataset above need to be updated from https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases.

india_cases     <- jhu_cases[jhu_cases$Country.Region == "India", ]
latest_date     <- as.Date(sub("X", "", rev(names(india_cases))[1]), format("%m.%d.%y")) # look here
dates_1         <- seq(as.Date("03-01-2020", format = "%m-%d-%y"), latest_date, "days")
dataf           <- unlist(india_cases[-(1:43)])
N               <- 1.38e9
forecast_length <- 120
forecast_dt     <- seq(from = as.Date(dates_1[length(dates_1)] + 1, format = "%m-%d-%y"),
                       to = as.Date(dates_1[length(dates_1)] + forecast_length, format = "%m-%d-%y"),
                       "days")
adj_len         <- 2
adj             <- T
plot_end_date   <- "2020-04-15"
# Now we just load the output files from the models for each case and use them to create the plot.

fig_3_data <- function(x) {

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
  spaghetti_ht            <-spaghetti_plot_ls[[1]]
  dthetaI_mean_data       <-spaghetti_plot_ls[[2]]
  sample_dthetaI_mat_long <-spaghetti_plot_ls[[3]]
  second_tp_date_ci       <-spaghetti_plot_ls[[5]]
  first_tp_date_ci        <-spaghetti_plot_ls[[4]]

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

mod_3 <- fig_3_data("./India_3_plot_data.RData")[[1]] #
mod_2 <- fig_3_data("./India_2_plot_data.RData")[[1]]
mod_1 <- fig_3_data("./India_1_plot_data.RData")

mod_1_up <- mod_1[[2]]
mod_1    <- mod_1[[1]]

observed_plot  <- data.frame(Dates = dates_1,variable = rep("True", length(dates_1)), value = dataf)
forecasts      <- data.frame(Dates = forecast_dt, mod_3, mod_2, mod_1, mod_1_up)
forecasts_plot <- melt(forecasts, id = "Dates")

complete_plot           <- rbind(observed_plot, forecasts_plot)
complete_plot           <- complete_plot[order(complete_plot$Dates), ]
rownames(complete_plot) <- NULL

complete_plot$color <- unlist(lapply(complete_plot$variable, LineColor))

ymax     <- max(log(complete_plot$value))
mytitle  <- paste0("COVID-19 Cumulative Cases by Day for India")
mybreaks <- seq(0, ymax, length.out = 10)

complete_plot <- complete_plot %>%
  filter(Dates <= as.Date(plot_end_date, format = "%Y-%m-%d"))

f3plotdata = complete_plot[complete_plot$variable != "mod_1_up", ]

ggplot(f3plotdata %>% filter(color == 'black'), aes(Dates, log(value))) +
  geom_bar(stat = 'identity', fill = 'gray') +
  geom_bar(data = f3plotdata %>% filter(color == 'red'), aes(Dates, log(value)),
           stat = 'identity', fill = '#264652') +
  geom_bar(data = f3plotdata %>% filter(color == 'green'), aes(Dates, log(value)),
           stat = 'identity', fill = '#e76f51') +
  geom_bar(data = f3plotdata %>% filter(color == 'blue'), aes(Dates, log(value)),
           stat = 'identity', fill = '#e9c46a') +

  labs(title = mytitle,
       subtitle = paste0("as of ", latest_date),
       caption = "© COV-IND-19 Study Group") +
  theme_bw() +
  scale_y_continuous(breaks   = mybreaks,
                     limits   = c(0, ymax),
                     labels   = round(exp(mybreaks), 0),
                     sec.axis = sec_axis(~., name = "", breaks = mybreaks, labels = round(exp(mybreaks), 0))) +
  theme(legend.position = "none",
        axis.text.x     = element_text(angle=45,vjust=0.5,size=15),
        axis.text.y     = element_text(size=15),
        plot.title      = element_text(size=22),
        plot.caption    = element_text(size=12,face="bold",color="blue"),
        axis.title.x    = element_blank(),
        axis.title.y    = element_blank()) +
  geom_line(data = complete_plot[complete_plot$variable == "mod_1_up", ],
            aes(x = Dates, y = log(value)),
            stat     = "identity",
            linetype = 2,
            size     = 1,
            color    = "#e9c46a") +
  # geom_line(data = spline_int, aes(x = x, y = y)) +
  geom_vline(aes(xintercept = !!forecast_dt[1]),
             linetype   = 2,
             alpha      = 0.6,
             size       = 1) +
  annotate(geom="text",x=as.Date("2020-04-10"),
         y=12,label="No intervention",fontface="bold", color = '#264652') +
  annotate(geom="text",x=as.Date("2020-04-01"),
           y=10,label="Social distancing",fontface="bold", color = '#e76f51') +
  annotate(geom="text",x=as.Date("2020-03-26"),
           y=8,label="Current intervention",fontface="bold", color = '#e9c46a') ->

  p1

  write_rds(p1, file = "./Figure4.Rds")

library(tidyverse)
library(vroom)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")
path <- paste0(data_repo, today, "/")
if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Creating ", path)
}

message("Generating plot data in ", path)

jhu.data <- vroom(paste0(data_repo, today, "/jhu_data.csv")) %>%
filter(Country == "India" & Date >= "2020-03-01")

state.data <- vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
filter(Date >= "2020-03-01")

adj_len         <- 2
adj             <- T
plot_start_date <- "2020-03-01"
plot_end_date   <- "2020-04-30"

source("~/cov-ind-19/model/r_scripts/get_states.R")
forecasts <- c("India", x$State)

pops <- c("India" = 1.34e9, "up" = 199.8e6, "mh" = 112.4e6, "br" = 104.1e6,
		  "wb" = 91.3e6, "ap" = 84.6e6, "mp" = 72.1e6, "tn" = 72.1e6, "rj" = 68.5e6,
		  "ka" = 61.1e6, "gj" = 60.4e6, "or" = 42.0e6, "kl" = 33.4e6, "jh" = 33.0e6,
		  "as" = 31.2e6, "pb" = 27.7e6, "ct" = 25.5e6, "hr" = 25.4e6, "dl" = 16.8e6,
		  "jk" = 12.5e6, "ut" = 10.1e6, "hp" = 6.9e6, "tr" = 3.7e6, "ml" = 3.0e6,
		  "mn" = 2.9e6, "nl" = 2.0e6, "ga" = 1.6e6, "ar" = 1.4e6, "py" = 1.2e6,
		  "mz" = 1.1e6, "ch" = 1.1e6, "sk" = 6.1e5, "an" = 3.8e5, "dn" = 3.4e5,
		  "dd" = 2.4e5, "ld" = 6.4e4, "la" = NA)

for (forecast in forecasts) {
    pop  <- pops[forecast]
    if (forecast == "India")
        data <- jhu.data
    else
        data <- state.data %>% filter(State == forecast)

    observed.data    <- data$Cases
    forecast.len     <- 200
    forecasted.dates <- seq(from = max(data$Date) + 1, by =  1,
                            length.out = forecast.len)
    for (arrayid in 1:2) {
        path    <- paste0(data_repo, today, "/", arrayid, "wk")

        fig_4_data <- function(x)
        {
            load(x)

            t           <- plot_data_ls[[2]][[1]]
            y_text_ht   <- plot_data_ls[[4]][[1]]
            data_comp   <- plot_data_ls[[4]][[3]]
            data_comp_R <- plot_data_ls[[5]][[3]]

            confirm <- round(pop * (data_comp[(t + 1):(t + forecast.len), "mean"] +
                data_comp_R[(t + 1):(t + forecast.len), "mean"]))
            confirm_up <- round(pop * (data_comp[(t + 1):(t + forecast.len), "upper"] +
                data_comp_R[(t + 1):(t + forecast.len), "upper"]))
            if (adj == TRUE) {
                adj_v <- mean(as.vector(observed.data[(t - adj_len):t]) /
                    pop / (data_comp[(t - adj_len):t, "mean"] +
                    data_comp_R[(t - adj_len):t, "mean"]), na.rm = T)

                confirm_up <- round(confirm_up * adj_v)
                confirm    <- round(confirm * adj_v)
            }
            return(list(confirm, confirm_up))
        }

        mod_2    <- fig_4_data(paste0(path, "/", forecast, "_2_plot_data.RData"))
        mod_2_up <- mod_2[[2]]
        mod_2    <- mod_2[[1]]

        mod_3    <- fig_4_data(paste0(path, "/", forecast, "_3_plot_data.RData"))
        mod_3_up <- mod_3[[2]]
        mod_3    <- mod_3[[1]]

        mod_4    <- fig_4_data(paste0(path, "/", forecast, "_4_plot_data.RData"))
        mod_4_up <- mod_4[[2]]
        mod_4    <- mod_4[[1]]

        observed_plot <- tibble(
            Dates    = data$Date,
            variable = "True",
            value    = observed.data
        )

        forecasts_plot <- tibble(
            Dates    = forecasted.dates,
            mod_2    = mod_2,
            mod_3    = mod_3,
            mod_4    = mod_4,
        ) %>%
        gather(variable, value, -Dates)

        forecasts_plot_ci <- tibble(
            Dates = forecasted.dates,
            mod_2 = mod_2_up,
            mod_3 = mod_3_up,
            mod_4 = mod_4_up,
        ) %>%
        gather(variable, upper_ci, -Dates)

        forecasts_plot <- left_join(forecasts_plot, forecasts_plot_ci,
                                    by = c("Dates", "variable"))

        complete_plot <- bind_rows(observed_plot, forecasts_plot) %>%
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

        vroom_write(complete_plot, path = paste0(path, "/", forecast, "_figure_4_data.csv"))

        fig_5_data <- function(x)
        {
            load(x)

            t           <- plot_data_ls[[2]][[1]]
            y_text_ht   <- plot_data_ls[[4]][[1]]
            data_comp   <- plot_data_ls[[4]][[3]]
            data_comp_R <- plot_data_ls[[5]][[3]]

            confirm    <- round(pop * (data_comp[(t + 1):(t + forecast.len), "mean"] +
                data_comp_R[(t + 1):(t + forecast.len),"mean"]))
            confirm_up <- round(pop*(data_comp[(t + 1):(t + forecast.len),"upper"] +
            data_comp_R[(t + 1):(t + forecast.len),"upper"]))
            if (adj == T) {
                adj_v <- mean(as.vector(observed.data[(t - adj_len):t]) / pop
                    / (data_comp[(t - adj_len):t, "mean"] +
                    data_comp_R[(t - adj_len):t, "mean"]), na.rm = T)

                confirm_up <- round(confirm_up * adj_v)
                confirm    <- round(confirm * adj_v)
            }
            return(list(confirm, confirm_up))
        }

        mod_2    <- fig_5_data(paste0(path, "/", forecast, "_2_plot_data.RData"))
        mod_2_up <- mod_2[[2]]
        mod_2    <- mod_2[[1]]

        mod_3    <- fig_5_data(paste0(path, "/", forecast, "_3_plot_data.RData"))
        mod_3_up <- mod_3[[2]]
        mod_3    <- mod_3[[1]]

        mod_4    <- fig_5_data(paste0(path, "/", forecast, "_4_plot_data.RData"))
        mod_4_up <- mod_4[[2]]
        mod_4    <- mod_4[[1]]

        mod_5    <- fig_5_data(paste0(path, "/", forecast, "_5_plot_data.RData"))
        mod_5_up <- mod_5[[2]]
        mod_5    <- mod_5[[1]]

        mod_6    <- fig_5_data(paste0(path, "/", forecast, "_6_plot_data.RData"))
        mod_6_up <- mod_6[[2]]
        mod_6    <- mod_6[[1]]

        observed_plot <- tibble(
          Dates    = data$Date,
          variable = "True",
          value    = observed.data
          )

        forecasts_plot <- tibble(
          Dates    = forecasted.dates,
          mod_2    = mod_2,
          mod_3    = mod_3,
          mod_4    = mod_4,
          mod_5    = mod_5,
          mod_6    = mod_6,
          ) %>%
          gather(variable, value, -Dates)

        forecasts_plot_ci <- tibble(
          Dates    = forecasted.dates,
          mod_2 = mod_2_up,
          mod_3 = mod_3_up,
          mod_4 = mod_4_up,
          mod_5 = mod_5_up,
          mod_6 = mod_6_up,
          ) %>%
          gather(variable, upper_ci, -Dates)

        forecasts_plot <- left_join(forecasts_plot, forecasts_plot_ci,
                                    by = c("Dates", "variable"))

        connect_plot <- tibble(
          Dates    = rep(as.Date("03-23-2020", format = "%m-%d-%y"), 5),
          variable = c("mod_2", "mod_3", "mod_4", "mod_5", "mod_6"),
          value    = rep(499, 5)
          )

        complete_plot <- bind_rows(observed_plot, forecasts_plot, connect_plot) %>%
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

        vroom_write(complete_plot, path = paste0(path, "/", forecast, "_figure_5_data.csv"))

        observed_plot <- tibble(
            Dates    = data$Date,
            variable = "True",
            value    = observed.data
        )

        mod_2               <- mod_2    - dplyr::lag(mod_2)
        mod_2_up            <- mod_2_up - dplyr::lag(mod_2_up)
        mod_3               <- mod_3    - dplyr::lag(mod_3)
        mod_3_up            <- mod_3_up - dplyr::lag(mod_3_up)
        mod_4               <- mod_4    - dplyr::lag(mod_4)
        mod_4_up            <- mod_4_up - dplyr::lag(mod_4_up)
        mod_5               <- mod_5    - dplyr::lag(mod_5)
        mod_5_up            <- mod_5_up - dplyr::lag(mod_5_up)
        mod_6               <- mod_6    - dplyr::lag(mod_6)
        mod_6_up            <- mod_6_up - dplyr::lag(mod_6_up)
        observed_plot$value <- observed_plot$value - lag(observed_plot$value)

        forecasts_plot <- tibble(
            Dates    = forecasted.dates,
            mod_2    = mod_2,
            mod_3    = mod_3,
            mod_4    = mod_4,
            mod_5    = mod_5,
            mod_6    = mod_6,
        ) %>%
        gather(variable, value, -Dates)

        forecasts_plot_ci <- tibble(
            Dates    = forecasted.dates,
            mod_2 = mod_2_up,
            mod_3 = mod_3_up,
            mod_4 = mod_4_up,
            mod_5 = mod_5_up,
            mod_6 = mod_6_up,
        ) %>%
        gather(variable, upper_ci, -Dates)

        forecasts_plot <- left_join(forecasts_plot, forecasts_plot_ci,
                                    by = c("Dates", "variable"))

        connect_plot <- tibble(
          Dates    = rep(as.Date("03-23-2020", format = "%m-%d-%y"), 10),
          variable = c("mod_2", "mod_2_up", "mod_3","mod_3_up", "mod_4",
                       "mod_4_up", "mod_5","mod_5_up", "mod_6", "mod_6_up"),
          value    = rep(499, 10)
          )

        complete_plot <- bind_rows(observed_plot, forecasts_plot, connect_plot) %>%
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

        vroom_write(complete_plot, path = paste0(path, "/", forecast,
                                                 "_figure_5_inc_data.csv"))
    }
}

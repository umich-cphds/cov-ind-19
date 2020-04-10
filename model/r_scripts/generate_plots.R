library(tidyverse)
library(vroom)
library(plotly)

today <- Sys.getenv("today")

generate_forecast_plots <- function(forecast)
{
    start.date <- as.Date("2020-03-01")
    latest     <- today
    path       <- paste0("~/cov-ind-19-data/", latest, "/", forecast)

    if (!dir.exists(path)){
        dir.create(path, recursive = T)
        message("creating ", path)
    }

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_1.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_23.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_4a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_4b.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_5a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_5b.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_6a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_6b.R")

    assign(paste0(forecast, "_p1"), plot_fig_1(latest = latest))
    p23 <- plot_fig_23(latest = latest)
    assign(paste0(forecast, "_p2"), p23$p2)
    assign(paste0(forecast, "_p3"), p23$p3)

    assign(paste0(forecast, "_p4a"), plot_fig_4a(forecast, latest = latest))
    assign(paste0(forecast, "_p4b"), plot_fig_4b(forecast, latest = latest))
    assign(paste0(forecast, "_p5a"), plot_fig_5a(forecast, latest = latest))
    assign(paste0(forecast, "_p5b"), plot_fig_5b(forecast, latest = latest))
    assign(paste0(forecast, "_p6a"), plot_fig_6a(forecast, latest = latest))
    assign(paste0(forecast, "_p6b"), plot_fig_6b(forecast, latest = latest))
    if (forecast == "India") {
        eval(expr(save(
            !!paste0(forecast, "_p1"),
            !!paste0(forecast, "_p2"),
            !!paste0(forecast, "_p3"),
            !!paste0(forecast, "_p4a"),
            !!paste0(forecast, "_p4b"),
            !!paste0(forecast, "_p5a"),
            !!paste0(forecast, "_p5b"),
            !!paste0(forecast, "_p6a"),
            !!paste0(forecast, "_p6b"),
            file = paste0(path, "/plots.RData")
        )))
    } else {
        eval(expr(save(
            !!paste0(forecast, "_p4a"),
            !!paste0(forecast, "_p4b"),
            !!paste0(forecast, "_p5a"),
            !!paste0(forecast, "_p5b"),
            !!paste0(forecast, "_p6a"),
            !!paste0(forecast, "_p6b"),
            file = paste0(path, "/plots.RData")
        )))
    }
}

forecasts <- c("India", "dl", "mh", "kl")
for (forecast in forecasts)
    generate_forecast_plots(forecast)

load(paste0("~/cov-ind-19-data/", today, "/India/plots.RData"))

p1  <- India_p1
p2  <- India_p2
p3  <- India_p3
p4a <- India_p4a
p4b <- India_p4b
p5a <- India_p5a
p5b <- India_p5b
p6a <- India_p6a
p6b <- India_p6b


save(p1, p2, p3, p4a, p4b, p5a, p5b, p6a, p6b,
     file = paste0("~/cov-ind-19-data/", today, "/plots.RData"))

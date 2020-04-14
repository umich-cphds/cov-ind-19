library(tidyverse)
library(vroom)
library(plotly)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
	today     <- Sys.getenv("today")
} else {
	data_repo <- "~/cov-ind-19-test/"
	today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

generate_forecast_plots <- function(forecast)
{
    start.date <- as.Date("2020-03-01")
    latest     <- today
    path       <- paste0(data_repo, latest, "/", forecast)

    if (!dir.exists(path)){
        dir.create(path, recursive = T)
        message("creating ", path)
    }

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_1.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_2.R")
	source("~/cov-ind-19/model/r_scripts/plots/plot_fig_3.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_4a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_4b.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_5a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_5b.R")

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_6a.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_6b.R")

	if (forecast == "India") {
    	assign(paste0(forecast, "_p1"), plot_fig_1())
    	assign(paste0(forecast, "_p2"), plot_fig_2())
		p <- plot_fig_3()
    	assign(paste0(forecast, "_p3a"), p$p3a)
		assign(paste0(forecast, "_p3b"), p$p3b)
	}

    assign(paste0(forecast, "_p4a"), plot_fig_4a(forecast))
    assign(paste0(forecast, "_p4b"), plot_fig_4b(forecast))
    assign(paste0(forecast, "_p5a"), plot_fig_5a(forecast))
    assign(paste0(forecast, "_p5b"), plot_fig_5b(forecast))
    assign(paste0(forecast, "_p6a"), plot_fig_6a(forecast))
    assign(paste0(forecast, "_p6b"), plot_fig_6b(forecast))
    if (forecast == "India") {
        eval(expr(save(
            !!paste0(forecast, "_p1"),
            !!paste0(forecast, "_p2"),
            !!paste0(forecast, "_p3a"),
			!!paste0(forecast, "_p3b"),
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

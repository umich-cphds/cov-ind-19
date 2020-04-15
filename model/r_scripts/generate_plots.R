library(tidyverse)
library(vroom)
library(plotly)

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")
generate_forecast_plots <- function(forecast)
{
    start.date <- as.Date("2020-03-01")
    path       <- paste0(data_repo, today, "/", forecast)

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
	source("~/cov-ind-19/model/r_scripts/plots/plot_fig_7.R")


	plots <- list()
	if (forecast == "India") {
		p <- plot_fig_3()
		plots[["p1"]] = plot_fig_1()
		plots[["p2"]] = plot_fig_2()
		plots[["p3a"]] = p$p3a
		plots[["p3b"]] = p$p3b

		p <- plot_fig_7()
		plots[["p7a"]] = p$p7a
		plots[["p7b"]] = p$p7b
		plots[["p7c"]] = p$p7c
		plots[["p7d"]] = p$p7d

	}
	plots[["p4a"]] = plot_fig_4a(forecast)
	plots[["p4b"]] = plot_fig_4b(forecast)
	plots[["p5a"]] = plot_fig_5a(forecast)
	plots[["p5b"]] = plot_fig_5b(forecast)
	plots[["p6a"]] = plot_fig_6a(forecast)
	plots[["p6b"]] = plot_fig_6b(forecast)

    plots
}

forecasts <- c("India")

plots <- list()
for (forecast in forecasts)
    plots[[forecast]] <- generate_forecast_plots(forecast)


save(plots, file = paste0(data_repo, today, "/new_plots.RData"))

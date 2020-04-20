library(tidyverse)
library(vroom)
library(plotly)

# Set variables based on testing or production
data_repo <- "~/cov-ind-19-data/"

today <- Sys.getenv("today")

state.data <- vroom(paste0(data_repo, today, "/covid19india_data.csv"))
generate_forecast_plots <- function(state)
{
    start.date <- as.Date("2020-03-01")
    path       <- paste0(data_repo, today, "/", state)

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

    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_8.R")
    source("~/cov-ind-19/model/r_scripts/plots/plot_fig_9.R")

	plots <- list()
	if (state == "India") {
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

		plots[["p8"]] = plot_fig_8()
		plots[["p9"]] = plot_fig_9()

	}
	plots[["p4a"]] = plot_fig_4a(state)
	plots[["p4b"]] = plot_fig_4b(state)
	plots[["p5a"]] = plot_fig_5a(state)
	plots[["p5b"]] = plot_fig_5b(state)
	plots[["p6a"]] = plot_fig_6a(state)
	plots[["p6b"]] = plot_fig_6b(state)

    plots
}

data <- list(India = generate_forecast_plots("India"),
			 plots = list(),
			 states = c(),
             codes = c()
)

states.to.forecast <- c("dl", "kl", "mh")
for (state in states.to.forecast) {
	data$states <- c(data$states, state.data$Name[match(state, state.data$State)])
    data$codes  <- c(data$codes, state)
	data$plots[[state]] <- generate_forecast_plots(state)
}

save(data, file = paste0(data_repo, today, "/data.RData"))

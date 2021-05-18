suppressPackageStartupMessages({
library(tidyverse)
library(plotly)
library(ggtext)
})

today <- Sys.getenv("today")
code_repo <- Sys.getenv("code_repo")
data_repo <- Sys.getenv("data_repo")

state.data <- read_tsv(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols())
generate_forecast_plots <- function(state)
{
    start.date <- as.Date("2020-03-01")
    path       <- paste0(data_repo, "/", today, "/", state)

    if (!dir.exists(path)){
        dir.create(path, recursive = T)
        message("creating ", path)
    }

    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_1.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_2.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_3.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_4.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_5a.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_7.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_8.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_9.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_x.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_10.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_11.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_14.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_15.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_tvr.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_forest.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_dbl.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_vax.R"))
    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_vax_state.R"))
#    source(paste0(code_repo, "/model/r_scripts/plots/plot_fig_SEIR.R"))

	plots <- list()
	if (state == "India") {
		p <- plot_fig_3()
		plots[["p1"]] = plot_fig_1()
		plots[["pvax"]] = plot_fig_vax()
		plots[["pvax_state"]] = plot_fig_vax_state()
		plots[["p2"]] = plot_fig_2()
		plots[["p3a"]] = p$p3a
		plots[["p3b"]] = p$p3b

                plots[["p5a"]] = plot_fig_5a("India")

		p <- plot_fig_7()
		plots[["p7a"]] = p$p7a
		plots[["p7b"]] = p$p7b
		plots[["p7c"]] = p$p7c
		plots[["p7d"]] = p$p7d

		plots[["p8"]] = plot_fig_8()
		plots[["p9"]] = plot_fig_9()
		plots[["p10"]] = plot_fig_10()
		plots[["p11"]] = plot_fig_11()

		plots[["p14"]] = plot_fig_14()$p14
		pforest = plot_fig_forest()
		plots[["pforest_cfr1"]] = pforest$cfr1_for
		plots[["pforest_dbl"]] = pforest$dbl_for
		plots[["pforest_r_est"]] = pforest$r_est_for
		plots[["pforest_tp"]] = pforest$tp_for
		plots[["pforest_ga"]] = pforest$ga_for

	} else {
		plots[["x"]] <- plot_fig_x(state)
	}

	plots[["p4"]] = plot_fig_4(state)
	plots[["p15"]] = plot_fig_15(state)
	plots[['ptvr']] = plot_fig_tvr(state)
	plots[['pdbl']] = plot_fig_dbl(state)
#	plots[["pSEIR"]] = plot_fig_SEIR(state)
	
    plots
}

data <- list(India = generate_forecast_plots("India"),
			       plots = list(),
			       states = c(),
			       codes = c(),
			       gt = list()
)
source(paste0(code_repo, "/model/r_scripts/get_states.R"))

states.to.forecast <- x$State
for (state in states.to.forecast) {
	data$states <- c(data$states, state.data$Name[match(state, state.data$State)])
    data$codes  <- c(data$codes, state)
	data$plots[[state]] <- generate_forecast_plots(state)
}

source(paste0(code_repo, "/app/sum_table_app.R"))
data$gt <- India_gt_table()
gtsave(data$gt$point_in_time, filename = path.expand(paste0(data_repo, "/", today, "/COVIND_table_point_in_time.png")))
gtsave(data$gt$cumulative, filename = path.expand(paste0(data_repo, "/", today, "/COVIND_table_cumulative.png")))
data$gt = data$gt$full

save(data, file = paste0(data_repo, "/", today, "/data.RData"))

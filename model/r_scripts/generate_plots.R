library(tidyverse)
library(vroom)
library(plotly)

start.date <- as.Date("2020-03-01")
latest <- Sys.Date() - 1

source("~/cov-ind-19/model/r_scripts/plot_fig_1.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_23.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_4a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_4b.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_5a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_5b.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_6a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_6b.R")

plot_fig_1(latest = latest)
plot_fig_23(latest = latest)
plot_fig_4a(latest = latest)
plot_fig_4b(latest = latest)
plot_fig_5a(latest = latest)
plot_fig_5b(latest = latest)
plot_fig_6a(latest = latest)
plot_fig_6b(latest = latest)

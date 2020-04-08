library(tidyverse)
library(vroom)
library(plotly)

start.date <- as.Date("2020-03-01")
latest <- Sys.Date()

source("~/cov-ind-19/model/r_scripts/plot_fig_1.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_23.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_4a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_4b.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_5a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_5b.R")

source("~/cov-ind-19/model/r_scripts/plot_fig_6a.R")
source("~/cov-ind-19/model/r_scripts/plot_fig_6b.R")

p1 <-  plot_fig_1(latest = latest)
p23 <- plot_fig_23(latest = latest)
p2  <- p23$p2
p3  <- p23$p3
p4a <- plot_fig_4a(latest = latest)
p4b <- plot_fig_4b(latest = latest)
p5a <- plot_fig_5a(latest = latest)
p5b <- plot_fig_5b(latest = latest)
p6a <- plot_fig_6a(latest = latest)
p6b <- plot_fig_6b(latest = latest)

save(p1, p2, p3, p4a, p4b, p5a, p5b, p6a, p6b,
     file = paste0("~/cov-ind-19-data/", latest, "/plots.RData")
)

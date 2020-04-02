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

plot_fig_1(latest = latest)
plot_fig_23(latest = latest)
plot_fig_4a(latest = latest)
plot_fig_4b(latest = latest)
plot_fig_5a(latest = latest)
plot_fig_5b(latest = latest)
plot_fig_6a(latest = latest)
plot_fig_6b(latest = latest)


p1 <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot1.RDS"))
p2 <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot2.RDS"))
p3 <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot3.RDS"))
p4a <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot4a.RDS"))
p4b <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot4b.RDS"))
p5a <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot5a.RDS"))
p5b <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot5b.RDS"))
p6a <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot6a.RDS"))
p6b <- readRDS(paste0("~/cov-ind-19-data/", latest, "/plot6b.RDS"))


save(p1, p2, p3, p4a, p4b, p5a, p5b, p6a, p6b,
     file = paste0("~/cov-ind-19-data/", latest, "/plots.RData")
)

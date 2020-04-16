library(shiny)
library(plotly)
library(tidyverse)

if (Sys.getenv("IRIS") == "TRUE") {
    branch <- "IRIS"
} else {
    branch <- "master"
}

file <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/", branch,
               "/day_sp_animation.gif")

shinyUI(uiOutput("out"))

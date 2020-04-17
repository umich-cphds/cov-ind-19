library(shiny)
library(plotly)
library(tidyverse)

if (Sys.getenv("IRIS") == "TRUE") {
    branch <- "IRIS"
} else {
    branch <- "master"
}

shinyUI(uiOutput("out"))

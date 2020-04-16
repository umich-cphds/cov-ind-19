library(shiny)
library(tidyverse)
library(plotly)

if (Sys.getenv("IRIS") == "TRUE") {
    branch <- "IRIS"
} else {
    branch <- "master"
}

source("latest.R")
github.api.path <- paste0("https://api.github.com/repos/umich-cphds/",
                          "cov-ind-19-data/git/trees/", branch)

# authenticate as alexander rix and pull the latest data
latest <- get_latest(github.api.path)
file <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/", branch,
               "/", latest, "/new_plots.RData")

url <- url(file)
load(url)
close(url)


source("top_matter.R", local = T)
source("observed.R", local = T)
source("forecast.R", local = T)

shinyServer(function(input, output)
{
    output$latest <- renderText(paste0("Data last updated ",
                                       format(latest, format = "%B %d")))

    iwalk(plots,
    function(p, forecast) {
        iwalk(p,
        function(plot, name) {
            if ("plotly" %in% class(plot))
                out <- renderPlotly(plot)
            else if ("ggplot" %in% class(plot))
                out <- renderPlot(plot)
            else
                stop("Unrecognized plot type!")

            output[[paste(forecast, name, sep = "_")]] <<- out
        })
    })

    states <- c("Delhi", "Kerala")
    output$out <- renderUI({

        tabs <- map(states, ~ tabPanel(.x, paste0(.x, ": its a place!")))

        eval(expr(navbarPage("COVID-19 Outbreak in India",
          observed, forecast, navbarMenu("State Forecasts", !!!tabs))))

    })
})

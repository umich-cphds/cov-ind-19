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
               "/", latest, "/data.RData")

url <- url(file)
load(url)
close(url)

# order here matters
source("top_matter.R", local = T)
source("observed.R", local = T)
source("forecast.R", local = T)
source("state.R", local = T)
source("testing.R", local = T)

shinyServer(function(input, output)
{
    output$latest <- renderText(paste0("Data last updated ",
                                       format(latest, format = "%B %d")))

    extract_and_render_plot <- function(plot, plot.name, state.code)
    {
        if ("plotly" %in% class(plot))
            out <- renderPlotly(plot)
        else if ("ggplot" %in% class(plot))
            out <- renderPlot(plot)
        else
            stop("Unrecognized plot type!")

        print(paste(state.code, plot.name, sep = "_"))
        output[[paste(state.code, plot.name, sep = "_")]] <<- out
    }

    # extract and render national observed and forecast
    iwalk(data$India, extract_and_render_plot, state.code = "India")

    # extract and render state forecasts
    walk2(data$plots, data$codes, function(states.plots, state.code)
        iwalk(states.plots, extract_and_render_plot, state.code = state.code)
    )

    states <- data$states
    codes <- data$codes
    output$out <- renderUI({

        tabs <- map2(states, codes, generate_state_tab)

        eval(expr(navbarPage("COVID-19 Outbreak in India",
          observed, forecast, navbarMenu("State Forecasts", !!!tabs), testing)))

    })
})

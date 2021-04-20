library(shiny)
library(tidyverse)
library(plotly)
library(gridExtra)
library(ggtext)
library(grid)
library(extrafont)
library(gt)

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

img.file <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/",
                   branch, "/", latest, "/day_sp_animation.gif")


source("observed.R", local = T)
source("forecast.R", local = T)
source("state.R", local = T)
source("testing.R", local = T)
source("metrics.R", local = T)
source("india_snapshot.R", local = TRUE)

print(sessionInfo())

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
        else if ("grob" %in% class(plot))
            out <- renderPlot(grid.arrange(plot))
        else
            stop("Unrecognized plot type!")

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
          observed, forecast, testing, navbarMenu("State Forecasts", !!!tabs),
          metrics)))

    })

    output$downloadFacet_cases = downloadHandler(
        filename = function() {'cases_by_state_in_India.png'},
        content = function(con) {
            png(con, width = 3000, height = 2000, res = 200)
            plot(data$India$p7b)
            dev.off()
        }
    )

    output$downloadFacet_deaths = downloadHandler(
        filename = function() {'deaths_by_state_in_India.png'},
        content = function(con) {
            png(con, width = 3000, height = 2000, res = 200)
            plot(data$India$p7d)
            dev.off()
        }
    )
    
    output$downloadFacet_inc_projection = downloadHandler(
        filename = function() {'projected_incidences_by_state_in_India.png'},
        content = function(con) {
            png(con, width = 3000, height = 6000, res = 200)
            plot(data$India$p12a)
            dev.off()
        }
    )
    
    output$downloadFacet_cumul_projection = downloadHandler(
        filename = function() {'projected_cumulative_cases_by_state_in_India.png'},
        content = function(con) {
            png(con, width = 3000, height = 6000, res = 200)
            plot(data$India$p12b)
            dev.off()
        }
    )
    
    output$download_dashboard = downloadHandler(
        filename = function() {'dashboard.pdf'},
        content = function(con) {
            cairo_pdf(file = con, width = 12, height = 12)
            grid.arrange(data$India$pforest_ga)
            dev.off()
        }
    )
    
    output$India_gt = render_gt({
        data$gt
    })
    
    output$gt_india_snapshot = render_gt({
        snapshot()
    })
    
    output$download_gt_point = downloadHandler(
        filename = function() {'COVIND_table_point_in_time.png'},
        content = function(con) {
            download.file(paste0('https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/', latest, '/COVIND_table_point_in_time.png'),
                          'COVIND_table_point_in_time.png', mode = 'wb')
            file.copy(from = 'COVIND_table_point_in_time.png',
                      to = con)
        }
    )
    
    output$download_gt_cumulative = downloadHandler(
        filename = function() {'COVIND_table_cumulative.png'},
        content = function(con) {
            download.file(paste0('https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/', latest, '/COVIND_table_cumulative.png'),
                          'COVIND_table_cumulative.png', mode = 'wb')
            file.copy(from = 'COVIND_table_cumulative.png',
                      to = con)
        }
    )
})

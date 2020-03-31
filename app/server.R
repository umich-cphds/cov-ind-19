library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)
library(jsonlite)
library(scales)


github.path <- "https://github.com/umich-cphds/cov-ind-19-data/raw/master/"
# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
    get_latest <- function()
    {
        # authenticate as alexander rix and pull the latest data from the kaggle dataset.
        github.auth <- read_json(".github.json")

        auth    <- authenticate(github.auth$user, github.auth$key)
        request <- GET("https://api.github.com/repos/umich-cphds/cov-ind-19-data/git/trees/master", auth)

        stop_for_status(request)

        header <- headers(request)

        if (http_type(request) != "application/json")
            stop(header$date, ": GET did not result in the correct content type.")

        limit     <- as.numeric(header["x-ratelimit-limit"])
        remaining <- as.numeric(header["x-ratelimit-remaining"])

        if (limit == 60)
            warning(header$date, ": Github authorization failed",
                    ". Limited to 60 queries an hour!")

        if (remaining < limit / 10)
            warning(header$date, ": ", remaining, " remaining api calls!")

        json <- content(request)
        dates <- as.Date(map_chr(keep(json$tree, ~.x$type == "tree"), ~.x$path))

        max(dates)
    }
    latest <- get_latest()

    output$latest <- renderText(paste0("Data last updated ", format(latest, format = "%B %d")))

    output$plot1 <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot1.RDS")))
    })

    # output$download_plot1 <- downloadHandler(
    #     filename = glue("cov-ind-19_figure1_{Sys.Date()}.pdf"),
    #     content = function(file) {
    #         orca(plot1_input(), file)
    #     }
    # )

    output$plot2 <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot2.RDS")))
    })

    output$download_plot2 <- downloadHandler(
        filename = glue("cov-ind-19_figure2a_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2_input(use_title = TRUE)
            dev.off()
        }
    )

    output$plot3 <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot3.RDS")))
    })

    output$download_plot3 <- downloadHandler(
        filename = glue("cov-ind-19_figure2b_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot3_input(use_title = TRUE)
            dev.off()
        }
    )


    output$plot4a_full <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot4a.RDS")))
    })

    output$download_plot4a <- downloadHandler(
        filename = glue("cov-ind-19_figure4_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure4.Rds"))))
            dev.off()
        }
    )

    output$plot4b_full <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot4b.RDS")))
    })

    output$download_plot4b <- downloadHandler(
        filename = glue("cov-ind-19_figure4_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure4.Rds"))))
            dev.off()
        }
    )

    output$plot5a <- renderPlotly({
            readRDS(url(paste0(github.path, latest, "/plot5a.RDS")))
    })

    output$download_plot5a <- downloadHandler(
        filename = glue("cov-ind-19_figure5a_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure5.Rds"))))
            dev.off()
        }
    )

    output$plot5b <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot5b.RDS")))
    })

    output$download_plot5b <- downloadHandler(
        filename = glue("cov-ind-19_figure5b_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure5_inc.Rds"))))
            dev.off()
        }
    )

    output$plot6a <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot6a.RDS")))
    })

    output$download_plot6a <- downloadHandler(
        filename = glue("cov-ind-19_figure6a_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure5.Rds"))))
            dev.off()
        }
    )


    output$plot6b <- renderPlotly({
        readRDS(url(paste0(github.path, latest, "/plot6b.RDS")))
    })

    output$download_plot6b <- downloadHandler(
        filename = glue("cov-ind-19_figure6b_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure5_inc.Rds"))))
            dev.off()
        }
    )

     output$map <- renderImage({
         file <- tempfile(fileext = ".gif")
         download.file(paste0(github.path, latest, "/day_sp_animation.gif"), file)
         list(src = file, contentType = "image/gif", alt = "Map not available",
              width = 500)
     }, deleteFile = FALSE)
})

library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)
library(jsonlite)
library(scales)

github.path <- "https://api.github.com/repos/umich-cphds/cov-ind-19-data/git/trees/master"


# authenticate as alexander rix and pull the latest data from the kaggle dataset.

httr_get <- function(file)
{
    github.auth <- read_json(".github.json")

    auth    <- authenticate(github.auth$user, github.auth$key)
    request <- GET(file, auth)

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

    content(request)
}


json <- httr_get(github.path)
trees <- keep(json$tree, ~.x$type == "tree")
tree <-  trees[[which.max(as.Date(map_chr(trees, ~ .x$path)))]]

latest <- as.Date(tree$path)

url <- url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/" ,
                  latest, "/India/plots.RData"))

load(url)
close(url)

url <- url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/" ,
                  latest, "/dl/plots.RData"))

load(url)
close(url)


url <- url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/" ,
                  latest, "/kl/plots.RData"))

load(url)
close(url)

url <- url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/" ,
                  latest, "/mh/plots.RData"))

load(url)
close(url)


# Define server logic required to draw a histogram
shinyServer(function(input, output)
{


    output$latest <- renderText(paste0("Data last updated ", format(latest, format = "%B %d")))

    output$plot1 <- renderPlotly({
        India_p1
    })

    # output$download_plot1 <- downloadHandler(
    #     filename = glue("cov-ind-19_figure1_{Sys.Date()}.pdf"),
    #     content = function(file) {
    #         orca(plot1_input(), file)
    #     }
    # )

    output$plot2 <- renderPlotly({
        India_p2
    })

    output$plot3 <- renderPlotly({
        India_p3
    })

    output$plot4a_full <- renderPlotly({
        India_p4a
    })

    output$plot4b_full <- renderPlotly({
        India_p4b
    })

    output$plot5a <- renderPlotly({
        India_p5a
    })


    output$plot5b <- renderPlotly({
        India_p5b
    })

    output$plot6a <- renderPlotly({
        India_p6a
    })

    output$plot6b <- renderPlotly({
        India_p6b
    })

     output$map <- renderImage({
         file <- tempfile(fileext = ".gif")
         download.file(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/",
                              latest, "/day_sp_animation.gif"), file)
         list(src = file, contentType = "image/gif", alt = "Map not available",
              width = 500)
     }, deleteFile = FALSE)
})

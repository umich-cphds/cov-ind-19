library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)
library(jsonlite)
library(scales)

data_repo <- "cov-ind-19-data"

github.path <- paste0("https://api.github.com/repos/umich-cphds/",
                      data_repo, "/git/trees/master")


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

url <- url(paste0("https://github.com/umich-cphds/", data_repo, "/raw/master/" ,
                  latest, "/India/plots.RData"))

load(url)
close(url)

url <- url(paste0("https://github.com/umich-cphds/", data_repo, "/raw/master/" ,
                  latest, "/dl/plots.RData"))

load(url)
close(url)


url <- url(paste0("https://github.com/umich-cphds/", data_repo, "/raw/master/" ,
                  latest, "/kl/plots.RData"))

load(url)
close(url)

url <- url(paste0("https://github.com/umich-cphds/", data_repo, "/raw/master/" ,
                  latest, "/mh/plots.RData"))

load(url)
close(url)


# Define server logic required to draw a histogram
shinyServer(function(input, output)
{


    output$latest <- renderText(paste0("Data last updated ",
                                       format(latest, format = "%B %d")))

    output$plot1 <- renderPlotly({
        India_p1
    })

    output$plot2 <- renderPlotly({
        India_p2
    })

    output$plot3a <- renderPlotly({
        India_p3a
    })

    output$plot3b <- renderPlotly({
        India_p3b
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

    output$plot4a_fulldl <- renderPlotly({
        dl_p4a
    })

    output$plot4b_fulldl <- renderPlotly({
        dl_p4b
    })

    output$plot5adl <- renderPlotly({
        dl_p5a
    })

    output$plot5bdl <- renderPlotly({
        dl_p5b
    })

    output$plot6adl <- renderPlotly({
        dl_p6a
    })

    output$plot6bdl <- renderPlotly({
        dl_p6b
    })

    output$plot4a_fullmh <- renderPlotly({
        mh_p4a
    })

    output$plot4b_fullmh <- renderPlotly({
        mh_p4b
    })

    output$plot5amh <- renderPlotly({
        mh_p5a
    })

    output$plot5bmh <- renderPlotly({
        mh_p5b
    })

    output$plot6amh <- renderPlotly({
        mh_p6a
    })

    output$plot6bmh <- renderPlotly({
        mh_p6b
    })

    output$plot4a_fullkl <- renderPlotly({
        kl_p4a
    })

    output$plot4b_fullkl <- renderPlotly({
        kl_p4b
    })

    output$plot5akl <- renderPlotly({
        kl_p5a
    })

    output$plot5bkl <- renderPlotly({
        kl_p5b
    })

    output$plot6akl <- renderPlotly({
        kl_p6a
    })

    output$plot6bkl <- renderPlotly({
        kl_p6b
    })

     output$map <- renderImage({
         file <- tempfile(fileext = ".gif")
         download.file(paste0("https://github.com/umich-cphds/", data_repo, "/raw/master/",
                              latest, "/day_sp_animation.gif"), file)
         list(src = file, contentType = "image/gif", alt = "Map not available",
              width = 500)
     }, deleteFile = FALSE)
})

library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)
library(jsonlite)
library(scales)


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

    plot1_input <- function(use_title = FALSE) {
        start.date <- as.Date("2020-03-01")

        jhu.path <- paste0("https://github.com/CSSEGISandData/COVID-19/raw/",
                           "master/csse_covid_19_data/csse_covid_19_time_series")

        jhu.files <- list(
            Case      = paste0(jhu.path, "/time_series_covid19_confirmed_global.csv"),
            Recovered = paste0(jhu.path, "/time_series_covid19_recovered_global.csv"),
            Death     = paste0(jhu.path, "/time_series_covid19_deaths_global.csv")
        )

        data <- reduce(imap(jhu.files,
            function(file, var)
            {
                vroom(file) %>%
                select(Country = matches("Country"), matches("[0-9]+")) %>%
                filter(Country == "India") %>% select(-Country) %>%
                gather(matches("[0-9]+"), key = "Date", value = !!var) %>%
                mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
                filter(Date >= start.date - 1)
            }
        ), ~ left_join(.x, .y)) %>%

        mutate_at(vars(Case, Recovered, Death), list(function(x) {
            y <- x - lag(x)
            ifelse(y < 0, 0, y)
        })) %>%
        filter(Date >= start.date) %>%
        gather(Case, Recovered, Death, key = Type, value = Count) %>%
        mutate(Date = as.factor(format(Date, format = "%b %d"))) %>%
        mutate(Type = factor(
        recode(Type,
            Case = "New Cases",
            Recovered = "Recovered",
            Death = "Fatalities"
        ), levels = c("New Cases", "Fatalities", "Recovered")))

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        title <- paste("Daily number of COVID-19 new cases, fatalities and",
                       "recovered cases in India since March 1")


        data$text <- paste0(data$Date, ": ", data$Count, " ", data$Type)

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date", titlefont = axis.title.font,
                      showticklabels = TRUE, tickangle = -30)

        yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                      tickfont = tickfont)

        plot_ly(data, x = ~Date, y = ~Count, color = ~Type, text = ~text,
                type = "bar", colors = c("orange", "red", "dark green"),
                hoverinfo = "text") %>%
        layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>")))
    }

    output$plot1 <- renderPlotly({
        plot1_input()
    })

    output$download_plot1 <- downloadHandler(
        filename = glue("cov-ind-19_figure1_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot1_input(use_title = TRUE)
            dev.off()
        }
    )

    plot2_input <- function(use_title = FALSE) {

        jhu.path <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series"

        countries <- c("France", "Germany", "India", "Iran", "Italy",
                       "Korea, South", "US", "China")

        file <- paste0(jhu.path, "/time_series_covid19_confirmed_global.csv")
        data <- vroom(file) %>%
        select(Country = matches("Country"), matches("[0-9].*")) %>%
        filter(Country %in% countries) %>%
        mutate(Country = ifelse(Country == 'Korea, South', 'South Korea', Country) %>% as.factor()) %>%
        group_by(Country) %>%

        # Since we don't care about counts in each state we collapse into a
        # single count per country of interest.
        summarise_all(sum, na.rm = T) %>%
        gather(matches("[0-9].+"), key = Date, value = Cases) %>%
        mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
        group_by(Country) %>% filter(Cases >= 100) %>%
        arrange(Date) %>%
        mutate(Day = seq(n()))

        Day.max <- 30 # nrow(data %>% filter(Country == "India"))
        data <- filter(data, Day <= Day.max) %>%
        mutate(Date = format(Date, format = "%b %d")) %>%
        ungroup()

        title <- paste("Cumulative number of COVID-19 cases in India compared",
                       "to other countries affected by the pandemic")

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Days since total cases passed 100",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of reported cases", titlefont =
                      axis.title.font, tickfont = tickfont)

        data$text <- paste0(data$Date, ": ", data$Cases, " cases")
        plot_ly(data, x = ~ Day, y = ~Cases, text = ~text, color = ~Country,
                type = "scatter", mode = "lines+markers", hoverinfo = "text",
                line = list(width = 3)) %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot2 <- renderPlotly({
        plot2_input()
    })

    output$download_plot2 <- downloadHandler(
        filename = glue("cov-ind-19_figure2a_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2_input(use_title = TRUE)
            dev.off()
        }
    )

    plot3_input <- function(use_title = FALSE)
    {

        jhu.path <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series"

        countries <- c("India")

        file <- paste0(jhu.path, "/time_series_covid19_confirmed_global.csv")
        data <- vroom(file) %>%
        select(Country = matches("Country"), matches("[0-9].*")) %>%
        filter(Country %in% countries) %>%
        group_by(Country) %>%

        # Since we don't care about counts in each state we collapse into a
        # single count per country of interest.
        summarise_all(sum, na.rm = T) %>%
        gather(matches("[0-9].+"), key = Date, value = Cases) %>%
        mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
        group_by(Country) %>% filter(Cases >= 100) %>%
        arrange(Date) %>%
        mutate(Day = seq(n()))

        Day.max <- nrow(data %>% filter(Country == "India"))
        data <- filter(data, Day <= Day.max) %>%
        mutate(Day = Day,
               Date = format(Date, format = "%b %d")) %>%
        ungroup()

        title <- "Cumulative number of COVID-19 cases in India"

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Days since total cases passed 100",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of reported cases", titlefont =
                      axis.title.font, tickfont = tickfont)

        data$text <- paste0(data$Date, ": ", data$Cases, " cases")
        plot_ly(data, x = ~ Day, y = ~Cases, text = ~text,
                type = "scatter", mode = "lines+markers", hoverinfo = "text",
                line = list(width = 3, color = "dark green")) %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot3 <- renderPlotly({
        plot3_input()
    })

    output$download_plot3 <- downloadHandler(
        filename = glue("cov-ind-19_figure2b_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot3_input(use_title = TRUE)
            dev.off()
        }
    )

    github.path <- "https://github.com/umich-cphds/cov-ind-19-data/raw/master/"

    plot4a_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/1wk/figure_4_data.csv")) %>%
        mutate(variable = factor(variable, levels = c("True", "mod_3",
               "mod_2", "mod_4", "mod_4_up")
        )) %>%
        mutate(variable = recode(variable,
            "True" = "Observed",
            "mod_3" = "No intervention",
            "mod_2" = "Social distancing",
            "mod_4" = "Lockdown with moderate release",
            "mod_4_up" = "Lockdown upper credible interval")
        ) %>%
        mutate(text = paste0(format(Dates, format("%b %d")), ": ", value,
                            ifelse(variable == "Observed", " observed cases",
                                                           " projected cases")),
               i = variable != "Lockdown upper credible interval"
        )

        title <- paste("Cumulative number of COVID-19 cases in India compared",
                       "to other countries affected by the pandemic")

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Cumulative number of cases", type = "log",
                      dtick = 1, titlefont = axis.title.font)

        plot_ly(data %>% filter(i),
                x = ~ Dates, y = ~ value, text = ~text, color = ~variable,
                colors = c("gray", "red", "orange", "navy", "navy"),
                type = "bar", hoverinfo = "text"
        ) %>%
        add_trace(data = data %>% filter(!i), x = ~Dates, y = ~value,
                  type = "scatter", mode = "line"
        ) %>%
        layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>")
        ))
    }

    output$plot4a_full <- renderPlotly({
        plot4a_input()
    })

    output$download_plot4a <- downloadHandler(
        filename = glue("cov-ind-19_figure4_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure4.Rds"))))
            dev.off()
        }
    )



    plot4b_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/2wk/figure_4_data.csv")) %>%
        mutate(variable = factor(variable, levels = c("True", "mod_3",
               "mod_2", "mod_4", "mod_4_up")
        )) %>%
        mutate(variable = recode(variable,
            "True" = "Observed",
            "mod_3" = "No intervention",
            "mod_2" = "Social distancing",
            "mod_4" = "Lockdown with moderate release",
            "mod_4_up" = "Lockdown upper credible interval")
        ) %>%
        mutate(text = paste0(format(Dates, format("%b %d")), ": ", value,
                            ifelse(variable == "Observed", " observed cases",
                                                           " projected cases")),
               i = variable != "Lockdown upper credible interval"
        )

        title <- ""

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Cumulative number of cases", type = "log",
                      dtick = 1, titlefont = axis.title.font)

        plot_ly(data %>% filter(i),
                x = ~ Dates, y = ~ value, text = ~text, color = ~variable,
                colors = c("gray", "red", "orange", "navy", "navy"),
                type = "bar", hoverinfo = "text"
        ) %>%
        add_trace(data = data %>% filter(!i), x = ~Dates, y = ~value,
                  type = "scatter", mode = "line"
        ) %>%
        layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>")
        ))
    }

    output$plot4b_full <- renderPlotly({
        plot4b_input()
    })

    output$download_plot4b <- downloadHandler(
        filename = glue("cov-ind-19_figure4_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure4.Rds"))))
            dev.off()
        }
    )


    plot5a_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/1wk/figure_5_data.csv")) %>%
        mutate(text = paste0(format(Dates, "%b %d"),": ",
                             round(value),
                             " projected total cases")
        )


        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of infected cases per 100,000",
                      titlefont = axis.title.font)


        plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
                color = ~ color, type = "scatter", mode = "line",
                hoverinfo = "text"
        ) %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot5a <- renderPlotly({
        plot5a_input()
    })

    output$download_plot5a <- downloadHandler(
        filename = glue("cov-ind-19_figure5a_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure5.Rds"))))
            dev.off()
        }
    )

    plot5b_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/1wk/figure_5_inc_data.csv")) %>%
        mutate(text = paste0(format(Dates, "%b %d"),": ",
                             round(value),
                             " projected cases per day")
        )


        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of new infected cases per 100,000 per day",
                      titlefont = axis.title.font)


        plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
                color = ~ color, type = "scatter", mode = "line",
                hoverinfo = "text"
        ) %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot5b <- renderPlotly({
        plot5b_input()
    })

    output$download_plot5b <- downloadHandler(
        filename = glue("cov-ind-19_figure5b_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure5_inc.Rds"))))
            dev.off()
        }
    )

    plot6a_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/2wk/figure_5_data.csv")) %>%
        mutate(text = paste0(format(Dates, "%b %d"),": ",
                             round(value),
                             " projected total cases")
        )

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of infected cases per 100,000",
                      titlefont = axis.title.font)


        plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
                color = ~ color, type = "scatter", mode = "line",
                hoverinfo = "text"
        ) %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot6a <- renderPlotly({
        plot6a_input()
    })

    output$download_plot6a <- downloadHandler(
        filename = glue("cov-ind-19_figure6a_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure5.Rds"))))
            dev.off()
        }
    )

    plot6b_input <- function()
    {
        data <- vroom(paste0(github.path, latest, "/2wk/figure_5_inc_data.csv"))%>%
        mutate(text = paste0(format(Dates, "%b %d"),": ",
                             round(value),
                             " projected cases per day")
        )

        cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                      format(latest, format = "%b %d"), sep = ' ')

        axis.title.font <- list(size = 16)
        tickfont        <- list(size = 16)

        xaxis <- list(title = "Date",
                      titlefont = axis.title.font, showticklabels = TRUE,
                      tickangle = -30)

        yaxis <- list(title = "Total number of new infected cases per 100,000 per day",
                      titlefont = axis.title.font)


        plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
                color = ~ color, type = "scatter", mode = "line",
                hoverinfo = "text") %>%
        layout(xaxis = xaxis, yaxis = yaxis,
               title = list(text = paste0("", "<br> ", cap, "<br>"))
        )
    }

    output$plot6b <- renderPlotly({
        plot6b_input()
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

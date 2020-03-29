library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
    get_latest <- function()
    {
        auth <- authenticate("tzimiskes", "9660dd499101885031bce23958ea067882d51f15")
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
        mutate(Type = factor(Type, levels = c("Recovered", "Death", "Case"))) %>%
        mutate(Type = recode(Type, Recovered = 'Recovered', Death = 'Fatalities', Case = 'New Cases') %>% as_factor)
        
        if(use_title == TRUE) {
            title_matter = paste('Daily number of COVID-19 new cases, fatalities\nand recovered cases in India from March 1 to', 
                                 as.character(data$Date[nrow(data)]), sep = ' ')
        } else {
            title_matter = ''
        }
        
        p <- ggplot(data, aes(Date, Count))
        p <- p + geom_bar(stat = "identity", aes(fill = Type), position = "stack") +
        xlab('Date') + ylab('Daily Counts') +
            labs(subtitle = paste("Data source: Johns Hopkins University CSSE. Last updated", format(latest, format = "%b %d"), sep = ' '),
                 caption = "© COV-IND-19 Study Group") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 40, vjust = 0.2, size=10),
                  axis.text.y = element_text(size = 15),
                  axis.title.x = element_text(vjust=-0.5),
                  legend.position = "bottom", legend.title = element_blank(),
                  legend.text = element_text(size = 17), plot.title =
                  element_text(size = 18),
                  plot.caption = element_text(color = "blue", face = "bold")
            ) + guides(shape = guide_legend(override.aes = list(size = 2)),
                       color = guide_legend(override.aes = list(size = 2))) +
            theme(legend.title = element_text(size = 12), 
                  legend.text  = element_text(size = 12),
                  legend.key.size = unit(0.4, "lines")) + 
            scale_fill_manual("", values = c("New Cases" = "orange", "Recovered" =
                              "dark green", "Fatalities" = "red")) + 
            ggtitle(title_matter)
        print(p)
    }

    output$plot1 <- renderPlotly({
        plotly::ggplotly(plot1_input()) %>% 
            layout(yaxis = list(title = 'Daily Counts'))
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
        mutate(Day = Day) %>%
        ungroup()
        
        if(use_title == TRUE) {
            title_matter = 'Cumulative number of COVID-19 cases in India compared to\nother countries affected by the pandemic'
        } else {
            title_matter = ''
        }
        
        subtext = paste('The x-axis starts on the day when each country exceeded 100 cases in order to allow comparison of case counts\nat similar stages of the outbreak. Last updated',
                        format(latest, format = "%b %d"), sep = ' ')
        
        p <- ggplot(data, aes(Day, Cases, col = Country, group = Country)) +
            geom_point(size = 1.5, na.rm = TRUE, alpha = 1) +
            geom_path(size = 1, na.rm = TRUE, alpha = 1) +
            geom_point(data = data %>% filter(Country == "India"), size = 1.5,
                       na.rm = TRUE, alpha = 1) +
            geom_path(data = data %>% filter(Country == "India"), size = 1,
                      na.rm = TRUE, alpha = 1) +
            xlab("Days since infected cases reached 100") +
            ylab("Cumulative number of reported cases") +
            theme_bw() + labs(subtitle = subtext,
                              caption = "\uA9 COV-IND-19 Study Group")+
            theme(axis.text.x = element_text(
                vjust = 0.5, size = 15),
                legend.position = "bottom",
                axis.text.y = element_text(size = 15),
                plot.title = element_text(size = 18),
                plot.caption = element_text(color = "blue", face = "bold"),
                #legend.position = c(0.1,0.6),
                legend.title = element_blank(),
                legend.box = "horizontal",
                legend.text = element_text(size = 17)) + 
            guides(shape = guide_legend(override.aes = list(size = 2)),
                   color = guide_legend(override.aes = list(size = 2))) +
            theme(legend.title = element_text(size = 12), 
                  legend.text  = element_text(size = 12),
                  legend.key.size = unit(0.4, "lines")) + 
            ggtitle(title_matter) + xlim(0, 30)
        print(p)
    }

    output$plot2 <- renderPlotly({
        plotly::ggplotly(plot2_input(), tooltip = c('Cases', 'Day', 'Date'))
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
        
        if(use_title == TRUE) {
            title_matter = 'Cumulative number of COVID-19 cases in India alone'
        } else {
            title_matter = ''
        }

        p <- ggplot(data, aes(Date, Cases, col = Country, group = Country)) +
            geom_point(size = 1.5, na.rm = TRUE, color = "#00BE67") +
            geom_path(size = 1, na.rm = TRUE, color = "#00BE67") +
            xlab("\nDays since infected cases reached 100")+
            ylab("Cumulative number of reported cases") +
            theme_bw() + 
            labs(subtitle = paste("This figure displays the cumulative number of COVID-19 cases in India\nsince the country reached 100 total cases. Last updated", format(latest, format = "%b %d")),
                 caption = "\uA9 COV-IND-19 Study Group") + 
            theme(axis.text.x = element_text(angle = 40, vjust = 0.15, size=10),
                legend.position = "bottom",
                axis.text.y = element_text(size = 15),
                plot.title = element_text(size = 18),
                plot.caption = element_text(color = "blue", face = "bold"),
                #legend.position = c(0.1,0.6),
                legend.title = element_blank(),
                legend.box = "horizontal",
                legend.text = element_text(size = 17),
                axis.title.x = element_text()) + 
            ggtitle(title_matter)
        print(p)
    }

    output$plot3 <- renderPlotly({
        plotly::ggplotly(plot3_input(), tooltip = c('Cases', 'Day', 'Date'))
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
    
    output$plot4a_full <- renderPlot({
            readRDS(url(paste0(github.path, latest, "/1wk/Figure4.Rds"))) + 
            theme(plot.title = element_blank(),
                  plot.caption = element_blank(),
                  plot.subtitle = element_blank())
    })
    
    output$download_plot4a <- downloadHandler(
        filename = glue("cov-ind-19_figure4_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure4.Rds"))))
            dev.off()
        }
    )
    
    output$plot4b_full <- renderPlot({
        readRDS(url(paste0(github.path, latest, "/2wk/Figure4.Rds"))) + 
            theme(plot.title = element_blank(),
                  plot.caption = element_blank(),
                  plot.subtitle = element_blank())
    })
    
    output$download_plot4b <- downloadHandler(
        filename = glue("cov-ind-19_figure4_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure4.Rds"))))
            dev.off()
        }
    )

    output$plot5a_full <- renderPlotly({
        gplot = readRDS(url(paste0(github.path, latest, "/1wk/Figure5.Rds"))) + 
            theme(plot.title = element_blank(),
                  plot.caption = element_blank(),
                  plot.subtitle = element_blank())
        gplot$labels$title = ""
        plotly::ggplotly(gplot, layerData = 1)
    })
    
    output$download_plot5a <- downloadHandler(
        filename = glue("cov-ind-19_figure5_1week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/1wk/Figure5.Rds"))))
            dev.off()
        }
    )
    
    output$plot5b_full <- renderPlotly({
        gplot = readRDS(url(paste0(github.path, latest, "/2wk/Figure5.Rds"))) + 
            theme(plot.title = element_blank(),
                  plot.caption = element_blank(),
                  plot.subtitle = element_blank())
        gplot$labels$title = ""
        plotly::ggplotly(gplot, layerData = 1)
    })
    
    output$download_plot5b <- downloadHandler(
        filename = glue("cov-ind-19_figure5_2week_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 11, height = 7)
            print(readRDS(url(paste0(github.path, latest, "/2wk/Figure5.Rds"))))
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

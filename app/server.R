library(shiny)
library(tidyverse)
library(vroom)
library(httr)
library(plotly)
library(glue)

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
    request <- GET("https://api.github.com/repos/umich-cphds/cov-ind-19-data/git/trees/master")
    dates <- as.Date(map_chr(
                         keep(content(request)$tree, ~.x$type == "tree"),
                         ~.x$path)
    )
    latest <- max(dates)

    plot1_input <- function() {
        start.date <- as.Date("2020-03-01")

        jhu.path <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series"
        jhu.files <- list(
            Case      = paste0(jhu.path, "/time_series_covid19_confirmed_global.csv"),
            Recovered = paste0(jhu.path, "/time_series_covid19_recovered_global.csv"),
            Death    = paste0(jhu.path, "/time_series_covid19_deaths_global.csv")
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
        mutate(Date = as.factor(format(Date, format = "%b.%d"))) %>%
        mutate(Type = factor(Type, levels = c("Recovered", "Death", "Case")))

        p <- ggplot(data, aes(Date, Count))
        p <- p + geom_bar(stat = "identity", aes(fill = Type), position = "stack") +
        xlab("") + ylab("Number of new cases/recovered/deaths") +
            labs(subtitle = "Data source: Johns Hopkins University CSSE",
                 caption = "© COV-IND-19 Study Group") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size=15),
                  axis.text.y = element_text(size = 15),
                  legend.position = "bottom", legend.title = element_blank(),
                  legend.text = element_text(size = 17), plot.title =
                  element_text(size = 18), axis.title.y = element_blank(),
                  plot.caption = element_text(color = "blue", face = "bold",
                  size = 10)
            ) +
            scale_fill_manual("", values = c("Case" = "orange", "Recovered" =
                              "dark green", "Death" = "red"))
        print(p)
    }

    output$plot1 <- renderPlotly({
        plotly::ggplotly(plot1_input())
    })

    output$download_plot1 <- downloadHandler(
        filename = glue("cov-ind-19_figure1_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot1_input()
            dev.off()
        }
    )

    plot2a_input <- function() {

        jhu.path <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series"

        countries <- c("France", "Germany", "India", "Iran", "Italy",
                       "Korea, South", "US", "China")

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

        Day.max <- 22 # nrow(data %>% filter(Country == "India"))
        data <- filter(data, Day <= Day.max) %>%
        mutate(Day = Day) %>%
        ungroup()

        p <- ggplot(data, aes(Day, Cases, col = Country, group = Country)) +
            geom_point(size = 1.5, na.rm = TRUE, alpha = .3) +
            geom_path(size = 1, na.rm = TRUE, alpha = .3) +
            geom_point(data = data %>% filter(Country == "India"), size = 1.5,
                       na.rm = TRUE, alpha = 1) +
            geom_path(data = data %>% filter(Country == "India"), size = 1,
                      na.rm = TRUE, alpha = 1) +
            xlab("Days since infected cases reached 100")+
            ylab("Number of infected cases") +
            theme_bw() + labs(caption = "\uA9 COV-IND-19 Study Group")+
            theme(axis.text.x = element_text(
                vjust = 0.5, size = 15),
                legend.position = "bottom",
                axis.text.y = element_text(size = 15),
                plot.title = element_text(size = 18),
                plot.caption = element_text(color = "blue", face = "bold"),
                #legend.position = c(0.1,0.6),
                legend.title = element_blank(),
                legend.box = "horizontal",
                legend.text = element_text(size = 17))
        print(p)
    }

    output$plot2a <- renderPlotly({
        plotly::ggplotly(plot2a_input())
    })

    output$download_plot2a <- downloadHandler(
        filename = glue("cov-ind-19_figure2a_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2a_input()
            dev.off()
        }
    )

    plot2b_input <- function()
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
        mutate(Day = Day) %>%
        ungroup()

        p <- ggplot(data, aes(Day, Cases, col = Country, group = Country)) +
            geom_point(size = 1.5, na.rm = TRUE, color = "dark green") +
            geom_path(size = 1, na.rm = TRUE, color = "dark green") +
            xlab("Days since infected cases reached 100")+
            ylab("Number of infected cases") +
            theme_bw() + ggtitle(paste("\uA9 COV-IND-19 Study Group; Updated", latest))
            theme(axis.text.x = element_text(
                vjust = 0.5, size = 15),
                legend.position = "bottom",
                axis.text.y = element_text(size = 15),
                plot.title = element_text(size = 18),
                plot.caption = element_text(color = "blue", face = "bold"),
                #legend.position = c(0.1,0.6),
                legend.title = element_blank(),
                legend.box = "horizontal",
                legend.text = element_text(size = 17))
        print(p)
    }

    output$plot2b <- renderPlotly({
        plotly::ggplotly(plot2b_input())
    })

    output$download_plot2b <- downloadHandler(
        filename = glue("cov-ind-19_figure2b_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2b_input()
            dev.off()
        }
    )


    # output$plot3a_full = renderImage({
    #     fls = files()
    #     fls = fls[str_detect(string = fls, pattern = fixed('forecast2.'))]
    #     fls = fls[str_detect(string = fls, pattern = fixed('2_Figure3_adj'))]
    #     # calibrate = ifelse(input$calib == 'wout_calib', 'without_calibration', 'with_calibration')
    #     middle_path = paste(paste(input$day, calibrate, sep = '/'), '', sep = '/')
    #     fls = paste0('ftp://xfer1.bio.sph.umich.edu/ncov2019/', middle_path, fls)
    #     outfile = tempfile(fileext='.png')
    #     download.file(url = fls, destfile = outfile, mode = 'wb')
    #     print(outfile)
    #     print(fls)
    #     list(src = outfile,
    #          alt = "Plot not found",
    #          width = 900)
    # }, deleteFile = TRUE)



    output$plot3a_full <- renderImage({
        fl <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/", latest, "/", latest, "_India_totalN_prior%232_Figure3_adj.png")
        outfile = tempfile(fileext='.png')
        download.file(url = fl, destfile = outfile, mode = 'wb')
        print(outfile)
        print(fl)
        list(src = outfile,
             alt = "Plot not available",
             width = 900)
    }, deleteFile = TRUE)

    output$plot3b_full <- renderImage({
        fl <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/", latest, "/", latest, "_India_totalN_prior%232_Figure3_adj.png")
        outfile = tempfile(fileext='.png')
        download.file(url = fl, destfile = outfile, mode = 'wb')
        print(outfile)
        print(fl)
        list(src = outfile,
             alt = "Plot not available",
             width = 900)
    }, deleteFile = TRUE)

    output$map <- renderImage({
        list(src = "./www/day_sp_animation.gif",
             contentType = "image/gif",
             alt = "Map not available")
    }, deleteFile = FALSE)

    # output$plot3c_metro <- renderImage({
    #     fl <- "ftp://xfer1.bio.sph.umich.edu/ncov2019/India/2020-03-24/2020-03-24_India_metroN_prior%232_Figure3_adj.png"
    #     outfile = tempfile(fileext='.png')
    #     download.file(url = fl, destfile = outfile, mode = 'wb')
    #     print(outfile)
    #     print(fl)
    #     list(src = outfile,
    #          alt = "Plot not available",
    #          width = 900)
    # }, deleteFile = TRUE)
    #
    # output$plot3d_metro <- renderImage({
    #     fl <- "ftp://xfer1.bio.sph.umich.edu/ncov2019/India/2020-03-24/2020-03-24_India_metroN_prior%233_Figure3_adj.png"
    #     outfile = tempfile(fileext='.png')
    #     download.file(url = fl, destfile = outfile, mode = 'wb')
    #     print(outfile)
    #     print(fl)
    #     list(src = outfile,
    #          alt = "Plot not available",
    #          width = 900)
    # }, deleteFile = TRUE)

    # output$plot3b_full <- renderImage({
    #     fl <- "2020-03-24_India_totalIN_prior#3_Figure3_adj.png"
    # })
    # output$plot3c_metro <- renderImage({
    #     fl <- "2020-03-24_India_metroN_prior#2_Figure3_adj.png"
    # })
    #
    # output$plot3d_metro <- renderImage({
    #     fl <- "2020-03-24_India_metroN_prior#3_Figure3_adj.png"
    # })


})

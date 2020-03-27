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

    plot2_input <- function() {

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

    output$plot2 <- renderPlotly({
        plotly::ggplotly(plot2_input())
    })

    output$download_plot2 <- downloadHandler(
        filename = glue("cov-ind-19_figure2a_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2_input()
            dev.off()
        }
    )

    plot3_input <- function()
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

    output$plot3 <- renderPlotly({
        plotly::ggplotly(plot3_input())
    })

    output$download_plot3 <- downloadHandler(
        filename = glue("cov-ind-19_figure2b_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot3_input()
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



    output$plot4_full <- renderPlotly({
            plotly::ggplotly(readRDS(url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/", latest, "/Figure4.Rds"))))
    })

    output$plot5_full <- renderPlotly({
        plotly::ggplotly(readRDS(url(paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/master/", latest, "/Figure5.Rds"))))
    })

    # output$map <- renderImage({
    #     list(src = "./www/day_sp_animation.gif",
    #          contentType = "image/gif",
    #          alt = "Map not available")
    # }, deleteFile = FALSE)

    # output$map = renderLeaflet({
    #     # load('./map/map_obj.RData')
    #     ndays=1 # no of previous days to plot, we start from the last reported day by default
    #     plot.height=11 # gif parameters
    #     plot.width=8.5 # gif parameters
    #     plot.delay=200 # gif parameters
    #     plot.dpi=60
    #     covid.india = read.csv("./map/IndividualDetails.csv") # assuming filenames in Kaggle are same, use IndividualDetails.csv
    #     covid.india$day.num = match(covid.india$Diagnosed.date,
    #                                 as.character(unique(covid.india$Diagnosed.date)))
    #     covid.ind = tapply(rep(1, nrow(covid.india)),
    #                        list(covid.india$day.num, covid.india$Detected.state),
    #                        sum)
    #     rownames(covid.ind) = as.character(unique(covid.india$Diagnosed.date))
    #     covid.ind[is.na(covid.ind)] = 0
    #     covid.ind = t(apply(covid.ind, 2, cumsum))
    #     covid.ind <- as.data.frame(covid.ind)
    #     rownames(covid.ind)[which(rownames(covid.ind)=="Delhi")] <- "NCT of Delhi"
    #     rownames(covid.ind)[which(rownames(covid.ind)=="Jammu and Kashmir")] <- "Jammu & Kashmir"
    #
    #
    #     india_shp <- st_read("./map/Indian_States.shp") # put the shape file in the data path
    #     list_try <- list()
    #
    #     states_np <- as.character(india_shp$st_nm[is.na(match(india_shp$st_nm,rownames(covid.ind)))])
    #     states_np_data <- matrix(0,nrow=length(states_np),ncol = ncol(covid.ind))
    #     rownames(states_np_data) <- states_np; colnames(states_np_data) <- cnames <- colnames(covid.ind)
    #     covid.ind <- rbind(covid.ind,states_np_data)
    #     covid.ind["Jammu & Kashmir",] <- covid.ind["Jammu & Kashmir",] + covid.ind["Ladakh",]
    #     covid.ind <- covid.ind[-which(rownames(covid.ind)=="Ladakh"),]
    #
    #     for(i in (ncol(covid.ind)-ndays+1):ncol(covid.ind)){
    #         temp_shp <- india_shp
    #         temp_shp$cases <- covid.ind[match(india_shp$st_nm,rownames(covid.ind)),i]
    #         temp_shp$day <- cnames[i]
    #
    #         list_try[[i]] <- temp_shp
    #     }
    #     final_data <- do.call(rbind, list_try)
    #
    #     anim_day <- tm_shape(india_shp) + tm_borders() + tm_shape(final_data) +
    #         tm_fill(col="cases", palette="Reds") + tm_text(text="cases")+
    #         tm_facets(along="day", free.coords=F)  +
    #         tm_compass(type = "8star", position = c("left", "top")) +
    #         tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
    #
    #     tmap_leaflet(anim_day)
    # })

    output$map <- renderImage({

        # ndays=5 # no of previous days to plot, we start from the last reported day by default
        # plot.height=11 # gif parameters
        # plot.width=8.5 # gif parameters
        # plot.delay=200 # gif parameters
        # plot.dpi=60
        # covid.india = read.csv("./map/IndividualDetails.csv") # assuming filenames in Kaggle are same, use IndividualDetails.csv
        # covid.india$day.num = match(covid.india$Diagnosed.date,
        #                             as.character(unique(covid.india$Diagnosed.date)))
        # covid.ind = tapply(rep(1, nrow(covid.india)),
        #                    list(covid.india$day.num, covid.india$Detected.state),
        #                    sum)
        # rownames(covid.ind) = as.character(unique(covid.india$Diagnosed.date))
        # covid.ind[is.na(covid.ind)] = 0
        # covid.ind = t(apply(covid.ind, 2, cumsum))
        # covid.ind <- as.data.frame(covid.ind)
        # rownames(covid.ind)[which(rownames(covid.ind)=="Delhi")] <- "NCT of Delhi"
        # rownames(covid.ind)[which(rownames(covid.ind)=="Jammu and Kashmir")] <- "Jammu & Kashmir"
        # 
        # 
        # india_shp <- st_read("./map/Indian_States.shp") # put the shape file in the data path
        # list_try <- list()
        # 
        # states_np <- as.character(india_shp$st_nm[is.na(match(india_shp$st_nm,rownames(covid.ind)))])
        # states_np_data <- matrix(0,nrow=length(states_np),ncol = ncol(covid.ind))
        # rownames(states_np_data) <- states_np; colnames(states_np_data) <- cnames <- colnames(covid.ind)
        # covid.ind <- rbind(covid.ind,states_np_data)
        # covid.ind["Jammu & Kashmir",] <- covid.ind["Jammu & Kashmir",] + covid.ind["Ladakh",]
        # covid.ind <- covid.ind[-which(rownames(covid.ind)=="Ladakh"),]
        # 
        # for(i in (ncol(covid.ind)-ndays+1):ncol(covid.ind)){
        #     temp_shp <- india_shp
        #     temp_shp$cases <- covid.ind[match(india_shp$st_nm,rownames(covid.ind)),i]
        #     temp_shp$day <- cnames[i]
        # 
        #     list_try[[i]] <- temp_shp
        # }
        # final_data <- do.call(rbind, list_try)
        # 
        # anim_day <- tm_shape(india_shp) + tm_borders() + tm_shape(final_data) +
        #     tm_fill(col="cases", palette="Reds") + tm_text(text="cases")+
        #     tm_facets(along="day", free.coords=F)  +
        #     tm_compass(type = "8star", position = c("left", "top")) +
        #     tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
        # 
        # # write animation to file
        # outfile = tempfile(fileext='.gif')
        # tmap_animation(anim_day,filename=outfile,
        #                width=plot.width, height=plot.height, delay=plot.delay, dpi=plot.dpi)


        list(src = './map/day_sp_animation.gif',
             contentType = "image/gif",
             alt = "Map not available", width = 500)
    }, deleteFile = FALSE)



})

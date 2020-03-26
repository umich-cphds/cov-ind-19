library(shiny)
library(tidyverse)
library(vroom)
library(plotly)
library(glue)

# Define server logic required to draw a histogram
shinyServer(function(input, output)
{
    plot1_input <- function() {
        start.date <- as.Date("2020-01-30")

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
        mutate_at(vars(Case, Recovered, Death), list( ~ .x - lag(.x))) %>%
        filter(Date >= start.date) %>%
        gather(Case, Recovered, Death, key = Type, value = Count) %>%
        mutate(Date = as.factor(format(Date, format = "%b.%d")))

        # dataf$Date <- format(as.Date(dataf$Date, format="%m-%d-%y"), format="%b.%d")
        # dataf$Date <- factor(dataf$Date, levels=dataf$Date)

        myylab <- "Number of new cases/recovered/deaths"

        p <- ggplot(data, aes(Date, Count))
        p <- p + geom_bar(stat = "identity", aes(fill = Type), position = "stack") +
            xlab("") + ylab(myylab) +
            labs(subtitle = "Data source: Johns Hopkins University CSSE",
                 caption = "© COV-IND-19 Study Group") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size=15),
                  axis.text.y = element_text(size = 15),
                  legend.position="bottom",
                  legend.title = element_blank(), #legend.box = "horizontal",
                  legend.text=element_text(size = 17), plot.title =
                  element_text(size = 18), axis.title.y = element_blank(),
                  plot.caption = element_text(color = "blue", face = "bold", size = 10)) +
            scale_fill_manual("", values = c("Case" = "orange", "Recovered" = "dark green", "Death" = "red"))
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
        options(stringsAsFactors=F)
        Data=read.csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv&filename=time_series_2019-ncov-Confirmed.csv")
        Data=Data[,-c(1,3,4)]

        Data.Country=aggregate(.~Country.Region,data=Data,sum,na.rm=TRUE)

        countries <- c("France","Germany","India","Iran","Italy","Japan","Korea, South","Singapore","US","China")
        # countries <- c("India")

        Data.Subset=Data.Country[Data.Country$Country.Region %in% countries,]
        HundredPlus=unlist(lapply(1:nrow(Data.Subset),function(x){return(min(which(Data.Subset[x,-1]>100)))}))
        MaxCols=max(HundredPlus)+((ncol(Data.Subset)-1)-min(HundredPlus))

        Data.New=matrix(0,nrow(Data.Subset),MaxCols)
        for(i in 1:nrow(Data.New))
        {
            Range=(1:(ncol(Data.Subset)-1))+(max(HundredPlus)-HundredPlus[i])
            Replace=c(rep(NA,min(Range)-1),as.vector(Data.Subset[i,-1]),rep(NA,MaxCols-max(Range)))
            Data.New[i,]=unlist(Replace)
        }
        Data.New=data.frame(Country=as.character(Data.Subset$Country.Region),Data.New)
        colnames(Data.New)[-1]=(1:MaxCols)-max(HundredPlus)
        Data.New[8,1]="South Korea"

        Data.Plot=melt(Data.New,id="Country")
        colnames(Data.Plot)[-1]=c("Days","Cases")


        p <- ggplot(Data.Plot[Data.Plot$Days%in%(-1:7),],
                    mapping=aes(x=Days,y=Cases,colour=Country,group=Country))+
            geom_point(size=3.6,na.rm=TRUE)+
            geom_path(size=1.8,na.rm=TRUE)+
            xlab("Days since infected cases reach 100")+
            ylab("Number of infected cases")+
            ggtitle("Cumulative number of COVID-19 infections", "Updated on March 22")+
            theme_bw()+labs(caption="\uA9 COV-IND-19 Study Group")+
            theme(axis.text.x=element_text(#angle=90,
                vjust=0.5,size=15),
                legend.position = "bottom",
                axis.text.y=element_text(size=15),
                plot.title=element_text(size=18),
                plot.caption=element_text(color="blue", face="bold"),
                #legend.position=c(0.1,0.6),
                legend.title=element_blank(),
                legend.box="horizontal",
                legend.text=element_text(size=17))
        print(p)
    }

    output$plot2 <- renderPlotly({
        plotly::ggplotly(plot2_input())
    })

    output$download_plot2 <- downloadHandler(
        filename = glue("cov-ind-19_figure2_{Sys.Date()}.pdf"),
        content = function(file) {
            pdf(file, width = 9, height = 6)
            plot2_input()
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
        fl <- "ftp://xfer1.bio.sph.umich.edu/ncov2019/India/2020-03-24/2020-03-24_India_totalN_prior%232_Figure3_adj.png"
        outfile = tempfile(fileext='.png')
        download.file(url = fl, destfile = outfile, mode = 'wb')
        print(outfile)
        print(fl)
        list(src = outfile,
             alt = "Plot not available",
             width = 900)
    }, deleteFile = TRUE)

    output$plot3b_full <- renderImage({
        fl <- "ftp://xfer1.bio.sph.umich.edu/ncov2019/India/2020-03-24/2020-03-24_India_totalN_prior%233_Figure3_adj.png"
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

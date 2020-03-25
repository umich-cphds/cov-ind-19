library(shiny)
library(tidyverse)
#library(reshape2)
library(plotly)
library(glue)

.posdates <- function(start.date, end.date){
    if(is.null(end.date)){
        end.date <- format(Sys.Date()-1, "%m-%d-%Y")
        message(paste("Input 'end.date' not provided. Using end.date =",end.date))
    }
    months <- as.numeric(unlist(strsplit(start.date,"-"))[1]) : as.numeric(unlist(strsplit(end.date,"-"))[1])
    months <- as.character(months)
    months[which(nchar(months)==1)] <- paste("0",months[which(nchar(months)==1)],sep="")
    days <- as.character(1:31)
    days[which(nchar(days)==1)] <- paste("0",days[which(nchar(days)==1)],sep="")
    
    dates <- NULL
    for(mm in months){
        if(mm %in% c("01","03","05","07","08","10","12")) days1 <- days
        if(mm %in% c("04","06","09","11")) days1 <- days[1:30]
        if(mm=="02") days1 <- days[1:29]
        if(mm==months[1]) days1 <- days1[as.numeric(unlist(strsplit(start.date,"-"))[2]):length(days1)]
        if(mm==months[length(months)]) days1 <- days[1:as.numeric(unlist(strsplit(end.date,"-"))[2])]
        if(mm=="01" & mm!=months[1]) days1 <- days1[22:length(days1)]
        
        for(dd in days1){
            dates <- c(dates, paste0(mm,"-",dd,"-2020"))
        }
        rm(days1)
    }
    return(dates)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    plot1_input <- function() {
        start.date="01-30-2020"
        end.date=NULL
        dates <- .posdates(start.date, end.date)
        # Define countries to look at (only 'India' now)
        countries <- "India"
        # Variables to be included in barplot
        plot.variables <- c("Confirmed","Deaths","Recovered")
        
        # Read the JHU CSSE data on confirmed cases/deaths
        datafull <- list()
        for(i in 1:length(dates)){
            message(paste("Reading date",dates[i]))
            data <- as.data.frame(read_csv(url(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dates[i],".csv"))))
            colnames(data) <- sapply(1:ncol(data), function(i) strsplit(colnames(data),"/")[[i]][1])
            # subset data for the chosen countries
            if(length(countries)==1){
                data1 <- data[grep(countries,data$Country),]
            }else{
                data1 <- data[grep(paste(countries,collapse="|"),data$Country),]
            }
            # aggregate data for each chosen country
            data2 <- aggregate(data1[,which(colnames(data1) %in% plot.variables)], by=list(Country=data1$Country), FUN=sum)
            data2$Date <- dates[i]
            datafull[[i]] <- data2[,c(which(colnames(data2)=='Date'),which(colnames(data2)!='Date'))]
            rm(data1, data2)
        }
        
        # Transform cumulative data to incident data
        dataf <- do.call(rbind, datafull)
        dataf1 <- dataf[!complete.cases(dataf), ]
        if(nrow(dataf1)>0){
            message("Some dates with missing values...")
            print(dataf1)
            message(paste0("NAs seem to be in the last days of Jan for Deaths and Recovered. Can be safely assumed to be 0 (based on COVID-19 timeline in ",countries,"). If they are not in Jan, please check if anything is wrong with the data."))
            dataf[!complete.cases(dataf), 'Deaths'] <- 0
            dataf[!complete.cases(dataf), 'Recovered'] <- 0
        }
        dataf$Newcase <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Confirmed[i], dataf$Confirmed[i]-dataf$Confirmed[i-1] )} )
        dataf$Newrecovered <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Recovered[i], dataf$Recovered[i]-dataf$Recovered[i-1] )} )
        dataf$Newdeath <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Deaths[i], dataf$Deaths[i]-dataf$Deaths[i-1] )} )
        
        # Long format of the incident data
        dataf <- dataf[,c('Date','Newcase','Newrecovered','Newdeath')]
        dataf <- dataf[-which(dataf$Newcase==0 & dataf$Newrecovered==0 & dataf$Newdeath==0),]
        colnames(dataf) <- c('Date','Case','Recovered','Death')
        dataf$Date <- format(as.Date(dataf$Date, format="%m-%d-%y"), format="%b.%d")
        dataf$Date <- factor(dataf$Date, levels=dataf$Date)
        dataflong <- gather(dataf, key="Count.type", value="Count", Case, Recovered, Death)
        dataflong$Count.type <- factor(dataflong$Count.type, levels=c('Death','Recovered','Case'))
        # Barplot using ggplot
        ymax <- max(dataf$Case + dataf$Recovered + dataf$Death, na.rm=T)
        mytitle <- paste("COVID-19 Confirmed New Cases/Recovered/Deaths by Day in",countries)
        myylab <- "Number of new cases/recovered/deaths"
        
        start.date="01-30-2020"
        end.date=NULL
        dates <- .posdates(start.date, end.date)
        # Define countries to look at (only 'India' now)
        countries <- "India"
        # Variables to be included in barplot
        plot.variables <- c("Confirmed","Deaths","Recovered")
        # Transform cumulative data to incident data
        dataf <- do.call(rbind, datafull)
        dataf1 <- dataf[!complete.cases(dataf), ]
        if(nrow(dataf1)>0){
            message("Some dates with missing values...")
            print(dataf1)
            message(paste0("NAs seem to be in the last days of Jan for Deaths and Recovered. Can be safely assumed to be 0 (based on COVID-19 timeline in ",countries,"). If they are not in Jan, please check if anything is wrong with the data."))
            dataf[!complete.cases(dataf), 'Deaths'] <- 0
            dataf[!complete.cases(dataf), 'Recovered'] <- 0
        }
        dataf$Newcase <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Confirmed[i], dataf$Confirmed[i]-dataf$Confirmed[i-1] )} )
        dataf$Newrecovered <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Recovered[i], dataf$Recovered[i]-dataf$Recovered[i-1] )} )
        dataf$Newdeath <- sapply(1:length(dates), function(i) { ifelse(i==1, dataf$Deaths[i], dataf$Deaths[i]-dataf$Deaths[i-1] )} )
        
        # Long format of the incident data
        dataf <- dataf[,c('Date','Newcase','Newrecovered','Newdeath')]
        dataf <- dataf[-which(dataf$Newcase==0 & dataf$Newrecovered==0 & dataf$Newdeath==0),]
        colnames(dataf) <- c('Date','Case','Recovered','Death')
        dataf$Date <- format(as.Date(dataf$Date, format="%m-%d-%y"), format="%b.%d")
        dataf$Date <- factor(dataf$Date, levels=dataf$Date)
        dataflong <- gather(dataf, key="Count.type", value="Count", Case, Recovered, Death)
        dataflong$Count.type <- factor(dataflong$Count.type, levels=c('Death','Recovered','Case'))
        # Barplot using ggplot
        ymax <- max(dataf$Case + dataf$Recovered + dataf$Death, na.rm=T)
        mytitle <- paste("COVID-19 Confirmed New Cases/Recovered/Deaths \nby Day in",countries)
        myylab <- "Number of new cases/recovered/deaths"
        dataflong$Count <- abs(dataflong$Count)
        p <- ggplot(dataflong, aes(Date, Count))
        p <- p + geom_bar(stat = "identity", aes(fill = Count.type), position = "stack") +
            xlab("") + ylab(myylab) +  
            labs(title=mytitle, subtitle="Data source: Johns Hopkins University CSSE",
                 caption="© COV-IND-19 Study Group") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size=15), 
                  axis.text.y = element_text(size=15),
                  legend.position="bottom", 
                  legend.title = element_blank(), #legend.box = "horizontal",
                  legend.text=element_text(size=17), plot.title = element_text(size=18),
                  axis.title.y = element_blank(),
                  plot.caption = element_text(color="blue", face="bold", size=10)) + 
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

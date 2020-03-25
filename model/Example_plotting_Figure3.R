# The function to prepare the plot
predplot <- function(start.date, full.rdata="bar_plot_adj.RData", forecast.ci.rdata="Forecast_CI.RData", data.folder=NULL, plot.folder, plotfilename="Figure3India.png",adj=F)
{
  # start.date: date from which to start the plot (format: mm-dd-yyyy)
  # end.date: date after which forecast data is available (format: mm-dd-yyyy)
  
  # read the predicted data
  load(paste0(data.folder,full.rdata))
  dataflong <- p$data
  dataflong <- dataflong[which(dataflong$Country=="India"),]
  dates <- as.character(dataflong$Dates)
  # wide format
  datafwide <- spread(dataflong[,1:3], Country, Confirmed)
  head(datafwide)
  rm(p)
  
  # read the forecast CI data and create a single df
  load(paste0(data.folder,forecast.ci.rdata))
  dates2 <- forecast_dt
  rm(forecast_dt)
  datafwide$India.up <- NA
  datafwide$India.up[which(as.Date(datafwide$Dates, format="%m-%d-%y") >= as.Date(dates2[1], format="%m-%d-%y"))] <- if(adj){India_confirm_up_adj}else{India_confirm_up}
  dataflong2 <- gather(datafwide, "Country", "Confirmed", all_of(c("India","India.up")))
  
  # Barplot using ggplot
  ymax <- max(log(dataflong2$Confirmed), na.rm=T)
  
  mytitle <- "COVID-19 Cumulative Cases by Day for India"
  mybreaks <- seq(0, ymax, length.out=10)
  datebreaks <- dates[which(dates %in% c(start.date,"03-01-2020","03-15-2020","03-31-2020","04-15-2020","04-30-2020","05-15-2020","05-31-2020","06-15-2020",dates2[1]))]
  
  #dev.new()
  p <- ggplot(dataflong, aes(Dates, logCount))
  p <- p + geom_bar(stat = "identity", aes(fill = Country), position = "dodge") +  
    labs(title=mytitle, caption="© COV-IND-19 Study Group") + 
    theme_bw() + 
    scale_y_continuous(breaks=mybreaks, limits=c(0,ymax), labels=round(exp(mybreaks),0),sec.axis = sec_axis(~., name = "",breaks=mybreaks,labels=round(exp(mybreaks),0))) +
    scale_x_discrete(breaks=datebreaks, labels=as.character(format(as.Date(datebreaks, format="%m-%d-%y"), format="%b.%d")))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size=15), 
          axis.text.y = element_text(size=15), 
          legend.position="none",
          plot.title = element_text(size=22),
          plot.caption = element_text(size=12, face="bold", color="blue"),
          axis.title.x = element_blank(), axis.title.y = element_blank()) +
    geom_line(data = datafwide, aes(x=Dates, y=log(India.up), group=1), stat="identity", linetype=2, alpha=1,size=2, color="#F8766D",na.rm = T) +
    geom_segment(aes(x=dates[5], xend=dates[10], y=ymax-0.5, yend=ymax-0.5),color="#F8766D",size=1.5,linetype=2) +
    annotate(geom="text", label=paste0("Upper credible limit of predicted cases for India"), x=dates[28], y=ymax-0.5,color="black",size=5.5) +
    geom_vline(aes(xintercept=which(factor(Dates) %in% dates2[1])), linetype=2,alpha=0.6,size=1)
  
  ggsave(paste0(plot.folder,plotfilename),width =20, height =8)
}



# Do the plotting for all the files saved in file_add
date<-"2020-03-23" #Sys.Date() # change to the date of the data you would like to extract
setwd(paste0("/mnt/biostat/ftp/ncov2019/India/",date,"/")) # set the place I save the files for plotting
file_add <- paste0("/mnt/biostat/ftp/ncov2019/India/",date,"/") # Please change to where you would like to save the file
# Plotting codes
for(server_num in 1:10){
  N_case <- ceiling(server_num/5)
  N_case_name <- c("totalN","metroN")[N_case]
  prior_case <- server_num%%5+1
  casename <- paste0(date,"_India_",N_case_name,"_prior#",prior_case)
  predplot(start.date="03-01-2020", full.rdata=paste0(casename,"barplot_obj.RData"), forecast.ci.rdata=paste0(casename,"plot_data.RData"), data.folder=file_add, plot.folder=file_add, plotfilename=paste0(casename,"_Figure3.png"),adj=F)
  predplot(start.date="03-01-2020", full.rdata=paste0(casename,"barplot_obj_adj.RData"), forecast.ci.rdata=paste0(casename,"plot_data.RData"), data.folder=file_add, plot.folder=file_add, plotfilename=paste0(casename,"_Figure3_adj.png"),adj=T)
}

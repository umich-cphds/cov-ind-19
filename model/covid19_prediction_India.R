rm(list=ls())
require(devtools)
source_url("https://github.com/RayDebashree/COVID19/blob/master/covid19plot_v1.R?raw=TRUE")

#### Data preparation
require(readr)
require(tidyr)
require(ggplot2)
library(dplyr)

start.date="03-01-2020"
end.date=NULL#"03-16-2020"
countries=c('India')

# Define possible dates
dates <- .posdates(start.date, end.date)

# Read the JHU CSSE data on confirmed cases/deaths
plot.variable=c("Confirmed","Deaths","Recovered")
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
  data2 <- aggregate(data1[,which(colnames(data1)%in%plot.variable)], by=list(Country=data1$Country), FUN=sum) %>% mutate(Removed=Deaths+Recovered)
  colnames(data2)[2:5] <- c(plot.variable,"Removed")
  
  datafull[[i]] <- data2
  rm(data1, data2)
}

#datafull


# Create a longformat data for plotting
dataf <- as.data.frame(matrix(NA, nrow=length(dates), ncol=length(countries)+1))
colnames(dataf) <- c("Dates", countries)
dataf$Dates <- dates
for(j in 1:length(dates)){
  dat1 <- datafull[[j]]
  for(k in 1:length(countries)){
    dat2 <- dat1[which(dat1$Country==countries[k]),2]
    if(length(dat2)>0) dataf[j,1+k] <- dat2
  }
}

dataf_removed <- as.data.frame(matrix(NA, nrow=length(dates), ncol=length(countries)+1))
colnames(dataf_removed) <- c("Dates", countries)
dataf_removed$Dates <- dates
for(j in 1:length(dates)){
  dat1 <- datafull[[j]]
  for(k in 1:length(countries)){
    dat2 <- dat1[which(dat1$Country==countries[k]),5]
    if(length(dat2)>0) dataf_removed[j,1+k] <- dat2
  }
}

library(chron)
library(dplyr)
library(reshape2)
library(readr)
library(rjags)
library(gtools) #rdirichlet(n, alpha)
library(scales) #alpha　function
library(ggplot2)
library(nCov2019)
library(chron)
library(data.table)
wd<-"/mnt/biostat/ftp/ncov2019/India/"
hd<-"~/ncov2019/report/International/"
#wd<-hd<-"/Users/liliwang/Box/Wuhan_Virus/codes/20200316_indian_group/"
file_add <-wd 
setwd(file_add)
today <- Sys.Date()
if(!file.exists(as.character(today))){dir.create(as.character(today))}
file_add <-paste0(file_add,today,"/")
setwd(file_add)
# In stall the R package or directly use the source file
# install.packages("devtools")
# library(devtools)
# install_github("lilywang1988/eSIR")
# library(eSIR) 
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/tvt.eSIR.R?raw=TRUE")
source_url("https://github.com/lilywang1988/eSIR/blob/master/R/qh.eSIR.R?raw=TRUE")
## Lili: I will add the imputed predicted data here
N_vec<-c(1369.56e6,32e6)  #N_total and N_metro
server_num=as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))# 5*2=10 three countries, Inida, Italy and US
#server_num <-1 #1-10
N_case <- ceiling(server_num/5)
N_case_name <- c("totalN","metroN")[N_case]
prior_case <- server_num%%5+1
save_mcmc <- F
save_plot_data <-T

R <- as.vector(dataf_removed[,2])/N_vec[N_case] # or use N_metro
Y <- as.vector(dataf[,2])/N_vec[N_case]-R # or use N_metro

casename <- paste0(today,"_India_",N_case_name,"_prior#",prior_case)
lambda0 <- 0

res <-tvt.eSIR(Y,R,begin_str=as.character(chron(chron(start.date,format="m-d-Y"),format="m/d/y")),T_fin=200,M=5e5,nburnin=2e5,exponential=T,lambda0=lambda0,casename=casename,save_files=T,file_add = file_add,save_mcmc=save_mcmc,save_plot_data=save_plot_data,gamma0 = 0.0117,R0=c(1.5,2,2.5,3,3.5)[prior_case])
# Prior of gamma0 is from SARS, its std is 0.1

end.date <-as.character(chron("03-01-2020",format="m-d-Y")+length(Y)-1) 
len=as.numeric(chron("06/15/20")-chron(end.date,format="m-d-Y"))

forecast_dt<-format(as.Date(end.date,"%m-%d-%y")+1:len,"%m-%d-%Y")
N<-N_vec[N_case]
#N_p<-c(1369.56e6,327.2e6,60.4e6);N<-c(32e6,327.2e6,60.4e6) 
#N_p<-N<-c(32e6,327.2e6,60.4e6) 
#adj2=T
adj_len=2 #use the last three observations to adjust the low prevalence
#setwd(dt)
load(file=paste0(file_add,casename,"_plot_data.RData"))
(casename<-plot_data_ls$casename)
other_plot<-plot_data_ls[[2]]

T_prime<-other_plot[[1]]
T_fin<-other_plot[[2]]
chron_ls=other_plot[[3]]
dthetaI_tp1=other_plot[[4]]
dthetaI_tp2=other_plot[[5]]
dthetaI_tp1_date=other_plot[[6]]
dthetaI_tp2_date=other_plot[[7]]
beta_p_mean=other_plot[[8]]
gamma_p_mean=other_plot[[9]]
R0_p_mean=other_plot[[10]]


spaghetti_plot_ls <-plot_data_ls[[3]]
spaghetti_ht<-spaghetti_plot_ls[[1]]
dthetaI_mean_data<-spaghetti_plot_ls[[2]]
sample_dthetaI_mat_long<-spaghetti_plot_ls[[3]]
first_tp_date_ci<-spaghetti_plot_ls[[4]]
second_tp_date_ci<-spaghetti_plot_ls[[5]]

infection_plot_ls <-plot_data_ls[[4]]
y_text_ht=infection_plot_ls[[1]]
data_poly=infection_plot_ls[[2]]
data_comp=infection_plot_ls[[3]]
data_pre=infection_plot_ls[[4]]

removed_plot_ls <-plot_data_ls[[5]]
r_text_ht=removed_plot_ls[[1]]
data_poly_R=removed_plot_ls[[2]]
data_comp_R=removed_plot_ls[[3]]
data_pre_R=removed_plot_ls[[4]]

India_confirm <- round(N[1]*(data_comp[(T_prime+1):(T_prime+len),"mean"]+
                                 data_comp_R[(T_prime+1):(T_prime+len),"mean"]))
India_confirm_up <- round(N[1]*(data_comp[(T_prime+1):(T_prime+len),"upper"]+
                                    data_comp_R[(T_prime+1):(T_prime+len),"upper"]))
India_confirm_low <- round(N[1]*(data_comp[(T_prime+1):(T_prime+len),"lower"]+
                                     data_comp_R[(T_prime+1):(T_prime+len),"lower"]))

adj_v <- mean(as.vector(dataf[(T_prime-adj_len):T_prime,1+1])/N[1]/(data_comp[(T_prime-adj_len):T_prime,"mean"]+data_comp_R[(T_prime-adj_len):T_prime,"mean"]))
India_confirm_adj<- round(India_confirm*adj_v)
India_confirm_up_adj<- round(India_confirm_up*adj_v)
India_confirm_low_adj<- round(India_confirm_low*adj_v)



# Create a longformat data for plotting
#fore_len<-length(dataf$Dates)
dataf_forecast<- tibble(Dates=forecast_dt,India=India_confirm,India_low=India_confirm_low,India_up=India_confirm_up,India_adj=India_confirm_adj,India_low_adj=India_confirm_low_adj,India_up_adj=India_confirm_up_adj)
dataf2 <- dataf%>%mutate(India_adj=India)%>%bind_rows(dataf_forecast[,])
dataf2$India<-cummax(dataf2$India)
dataf2$India_adj<-cummax(dataf2$India_adj)

# dataf2 <- dataf%>%bind_rows(dataf_forecast)
#dataf2<- tibble(Dates=c(dataf$Dates,forecast_dt),India=cummax(c(dataf$India,India_confirm)))

#dataf2 <- dataf%>%bind_rows(dataf_forecast)
# Not adjusted
dataflong <- dataf2%>% select(Dates,India)%>%gather("Country", "Confirmed", all_of(countries[1]))

dataflong$Country <- factor(dataflong$Country, levels=c(countries[1]))
dataflong$logCount <- log(dataflong[,3])
dataflong$logCount[which(dataflong$logCount==-Inf)] <- NA

plot.variable = "Confirmed"
# Barplot using ggplot
ymax <- max(dataflong$logCount, na.rm=T)

mytitle <- "COVID-19 Cumulative Confirmed Cases by Day"
myylab <- "Number of confirmed cases"
mybreaks <- seq(0,ymax,length.out = 10)
#dev.new()
p <- ggplot(dataflong, aes(Dates, logCount))
p<-p + geom_bar(stat = "identity", aes(fill = Country), position = "dodge") +
  xlab("Dates") + ylab(myylab) +  
  labs(title=mytitle, subtitle="Data source: JHU CSSE (https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)",
       caption=paste0("https://github.com/RayDebashree/COVID19 (",as.POSIXlt(Sys.time(), "GMT")," GMT)")) +
  theme_bw() + 
  scale_y_continuous(breaks=mybreaks, limits=c(0,ymax), labels=round(exp(mybreaks),0),sec.axis = sec_axis(~., name = "",breaks=mybreaks,labels=round(exp(mybreaks),0))) #+   coord_flip()
p=p+ scale_x_discrete(breaks=dataf2$Dates,labels=as.character(format(as.Date(dataf2$Dates, format="%m-%d-%y"), format="%b.%d")))+theme(axis.text.x = element_text(angle = 90, hjust = 1))#+
#p+geom_vline(xintercept=T_prime+0.5,linetype=2,alpha=0.5)+
#  annotate(geom="text", label=paste0("Forecast after \n",as.character(format(as.Date(unlist(dataf2$Dates[T_prime]), format="%m-%d-%y"), format="%b.%d"))), x=dataf2$Dates[T_prime-3], y=ymax-2,color="black",size=4.5)+
#  geom_segment(aes(x=dataf2$Dates[T_prime+0.5], xend=dataf2$Dates[T_prime+9], y=ymax-2, yend=ymax-2),arrow = arrow(length = unit(0.3, "cm"),type = "closed"),color="black")

#ggsave(paste0(casename,"barplot.png"),width =20, height =8)
save(p,file=paste0(casename,"barplot_obj.RData"))




### Adjusted 
dataflong_adj <- dataf2%>% select(Dates,India=India_adj)%>%gather("Country", "Confirmed",all_of(countries[1]))

dataflong_adj$Country <- factor(dataflong_adj$Country, levels=c(countries[1]))
dataflong_adj$logCount <- log(dataflong_adj[,3])
dataflong_adj$logCount[which(dataflong_adj$logCount==-Inf)] <- NA

plot.variable = "Confirmed"
# Barplot using ggplot
ymax <- max(dataflong_adj$logCount, na.rm=T)

mytitle <- "COVID-19 Cumulative Confirmed Cases by Day"
myylab <- "Number of confirmed cases"
mybreaks <- seq(0,ymax,length.out = 10)
#dev.new()
p <- ggplot(dataflong_adj, aes(Dates, logCount))
p<-p + geom_bar(stat = "identity", aes(fill = Country), position = "dodge") +
  xlab("Dates") + ylab(myylab) +  
  labs(title=mytitle, subtitle="Data source: JHU CSSE (https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)",
       caption=paste0("https://github.com/RayDebashree/COVID19 (",as.POSIXlt(Sys.time(), "GMT")," GMT)")) +
  theme_bw() + 
  scale_y_continuous(breaks=mybreaks, limits=c(0,ymax), labels=round(exp(mybreaks),0),sec.axis = sec_axis(~., name = "",breaks=mybreaks,labels=round(exp(mybreaks),0))) #+   coord_flip()
p=p+ scale_x_discrete(breaks=dataf2$Dates,labels=as.character(format(as.Date(dataf2$Dates, format="%m-%d-%y"), format="%b.%d")))+theme(axis.text.x = element_text(angle = 90, hjust = 1))#+
#p+geom_vline(xintercept=T_prime+0.5,linetype=2,alpha=0.5)+
#  annotate(geom="text", label=paste0("Forecast after \n",as.character(format(as.Date(unlist(dataf2$Dates[T_prime]), format="%m-%d-%y"), format="%b.%d"))), x=dataf2$Dates[T_prime-3], y=ymax-2,color="black",size=4.5)+
#  geom_segment(aes(x=dataf2$Dates[T_prime+0.5], xend=dataf2$Dates[T_prime+9], y=ymax-2, yend=ymax-2),arrow = arrow(length = unit(0.3, "cm"),type = "closed"),color="black")

#ggsave(paste0(casename,"barplot_adj.png"),width =20, height =8)
save(p,file=paste0(casename,"barplot_obj_adj.RData"))

save(dataf2,forecast_dt,India_confirm,India_confirm_up,India_confirm_adj,India_confirm_up_adj,file=paste0(casename,"plot_data.RData"))



## Define the plotting code
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


predplot(start.date="03-01-2020", full.rdata=paste0(casename,"barplot_obj.RData"), forecast.ci.rdata=paste0(casename,"plot_data.RData"), data.folder=file_add, plot.folder=file_add, plotfilename=paste0(casename,"_Figure3.png"),adj=F)

predplot(start.date="03-01-2020", full.rdata=paste0(casename,"barplot_obj_adj.RData"), forecast.ci.rdata=paste0(casename,"plot_data.RData"), data.folder=file_add, plot.folder=file_add, plotfilename=paste0(casename,"_Figure3_adj.png"),adj=T)


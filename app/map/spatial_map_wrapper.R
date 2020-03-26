# Wrapper for making spatial state level plots
## Step 1: Set the path to working directory
## Step 2: Save the data (IndividualDetails.csv) and shape files (contents of lgismap.zip)  in the working directory
# install.packages("tmap"); install.packages("sf")
#### Note: Adjust gif parameters as required to reduce compilation time
spatial_map_wrapper <- function(path=NULL, # path to data file
                                ndays=5, # no of previous days to plot, we start from the last reported day by default
                                plot.height=11, # gif parameters
                                plot.width=8.5, # gif parameters
                                plot.delay=200, # gif parameters
                                plot.dpi=60){ # gif parameters
  options(warn=-1)
  # for static and interactive maps
  library(tmap)   
  library(sf)
  setwd(path)
  covid.india = read.csv("./IndividualDetails.csv") # assuming filenames in Kaggle are same, use IndividualDetails.csv
  covid.india$day.num = match(covid.india$Diagnosed.date,
                              as.character(unique(covid.india$Diagnosed.date)))
  covid.ind = tapply(rep(1, nrow(covid.india)),
                     list(covid.india$day.num, covid.india$Detected.state),
                     sum)
  rownames(covid.ind) = as.character(unique(covid.india$Diagnosed.date))
  covid.ind[is.na(covid.ind)] = 0
  covid.ind = t(apply(covid.ind, 2, cumsum))
  covid.ind <- as.data.frame(covid.ind)
  rownames(covid.ind)[which(rownames(covid.ind)=="Delhi")] <- "NCT of Delhi"
  rownames(covid.ind)[which(rownames(covid.ind)=="Jammu and Kashmir")] <- "Jammu & Kashmir"
  
  
  india_shp <- st_read("./Indian_States.shp") # put the shape file in the data path
  list_try <- list()
  
  states_np <- as.character(india_shp$st_nm[is.na(match(india_shp$st_nm,rownames(covid.ind)))])
  states_np_data <- matrix(0,nrow=length(states_np),ncol = ncol(covid.ind))
  rownames(states_np_data) <- states_np; colnames(states_np_data) <- cnames <- colnames(covid.ind)
  covid.ind <- rbind(covid.ind,states_np_data)
  covid.ind["Jammu & Kashmir",] <- covid.ind["Jammu & Kashmir",] + covid.ind["Ladakh",] 
  covid.ind <- covid.ind[-which(rownames(covid.ind)=="Ladakh"),]
  
  for(i in (ncol(covid.ind)-ndays+1):ncol(covid.ind)){
    temp_shp <- india_shp
    temp_shp$cases <- covid.ind[match(india_shp$st_nm,rownames(covid.ind)),i]
    temp_shp$day <- cnames[i]
    
    list_try[[i]] <- temp_shp
  }
  final_data <- do.call(rbind, list_try)
  
  anim_day <- tm_shape(india_shp) + tm_borders() + tm_shape(final_data) +
    tm_fill(col="cases", palette="Blues") + tm_text(text="cases")+
    tm_facets(along="day", free.coords=F)  +
    tm_compass(type = "8star", position = c("left", "top")) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  
  tmap_animation(anim_day,filename="./day_sp_animation.gif",
                 width=plot.width, height=plot.height, delay=plot.delay, dpi=plot.dpi)
}

## Not Run
# Test:: spatial_map_wrapper(path = "/Users/aritrah/Downloads/covid19-in-india")


library(httr)
library(tidyverse)
library(sf)
library(tmap)

wd <- "~/cov-ind-19/app/map"
setwd(wd)

ndays=5
plot.height=11
plot.width=8.5
plot.delay=200
plot.dpi=200

days <- 7

auth <- authenticate("alexanderrix","73bc32161b1b3d4a98ddb89ed1110413")
url <- "https://www.kaggle.com/api/v1/datasets/download/sudalairajkumar/covid19-in-india/covid_19_india.csv"
request <- GET(url, auth)

stop_for_status(request)

data <- content(request) %>%
transmute(
        State = `State/UnionTerritory`,
        Cases = ConfirmedIndianNational + ConfirmedForeignNational,
        Date = as.Date(Date, format = "%d/%m/%y")
) %>%
arrange(State, Date) %>%
mutate(State = as.factor(case_when(
    State == "Delhi" ~ "NCT of Delhi",
    State == "Telengana" ~ "Telangana",
    State == "Chattisgarh" ~ "Chandigarh",
    State == "Pondicherry" ~ "Puducherry",
    State == "Andaman and Nicobar Islands" ~ "Andaman & Nicobar Island",
    State == "Jammu and Kashmir" ~ "Jammu & Kashmir",
    State == "Ladakh" ~ "Jammu & Kashmir",
    TRUE ~ State))
) %>% group_by(State, Date) %>%
summarise(Cases = sum(Cases)) %>%
ungroup() %>%
spread(Date, Cases, fill = 0) %>%
gather(matches("[0-9].+"), key = Date, value = Cases) %>%
mutate(Date = as.Date(Date)) %>%
filter(Date >= max(Date) - days)

india_shp <- st_read("Indian_States.shp")
i <- match(data$State, india_shp$st_nm)

final_data <- india_shp[i, ]
final_data$Cases <- data$Cases
final_data$Date <- data$Date

anim_day <- tm_shape(india_shp) + tm_borders() + tm_shape(final_data) +
  tm_fill(col="Cases", palette="Reds") + tm_text(text="Cases")+
  tm_facets(along="Date", free.coords=F)  +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)

tmap_animation(anim_day,filename="day_sp_animation.gif",
               width=plot.width, height=plot.height, delay=plot.delay, dpi=plot.dpi)

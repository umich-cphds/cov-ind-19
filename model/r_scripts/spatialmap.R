library(httr)
library(tidyverse)
library(sf)
library(tmap)
library(jsonlite)

wd <- "~/cov-ind-19/model/map"
setwd(wd)

plot.height <- 11
plot.width <- 8.5
plot.delay <- 200
plot.dpi <- 200

days.back  <- 7
start.date <- as.Date("2020-02-15")

kalends.ides <- as.Date(paste0(rep(paste0("2020-", 1:12), 2),
                       c(rep(-1, 12), rep(-15,12))
))

kaggle.auth <- read_json(".kaggle.json")

auth <- authenticate(kaggle.auth$user, kaggle.auth$key)
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
filter(Date %in% c(kalends.ides, seq(max(Date) - days.back, max(Date), 1)) &
       Date >= start.date
) %>% mutate(Date = format(Date, "%e %B %Y"))

india_shp <- st_read("Indian_States.shp")
i <- match(data$State, india_shp$st_nm)

final_data <- india_shp[i, ]
final_data$Cases <- data$Cases
final_data$Date <- data$Date

anim_day <- tm_shape(india_shp) + tm_borders() + tm_shape(final_data) +
  tm_fill(col = "Cases", palette = "Reds") + tm_text(text = "Cases") +
  tm_facets(along = "Date", free.coords = F) + tm_legend(scale = 2)

today <- Sys.Date()
wd <- paste0("~/cov-ind-19-data/", today)
setwd(wd)

tmap_animation(anim_day,filename="day_sp_animation.gif",
               width=plot.width, height=plot.height, delay=plot.delay, dpi=plot.dpi)

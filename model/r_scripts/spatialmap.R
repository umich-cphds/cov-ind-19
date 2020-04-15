library(httr)
library(tidyverse)
library(sf)
library(tmap)
library(jsonlite)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")
plot.height <- 11
plot.width  <- 8.5
plot.delay  <- 200
plot.dpi    <- 100

days.back  <- 7
start.date <- as.Date("2020-02-15")

# first and fifteenth of each month over the next two years
kalends.ides <- as.Date(paste0(c(paste0("2020-", 1:12), paste0("2020-", 1:12)),
                        c(rep(-1, 12), rep(-15,12))
))

# shape file for spatial plot
india_shp   <- st_read("~/cov-ind-19/model/map/Indian_States.shp")

# authenticate as alexander rix and pull the latest data from the kaggle dataset.
kaggle.auth <- read_json("~/cov-ind-19/model/map/.kaggle.json")

auth    <- authenticate(kaggle.auth$user, kaggle.auth$key)
url     <- "https://www.kaggle.com/api/v1/datasets/download/sudalairajkumar/covid19-in-india/covid_19_india.csv"
request <- GET(url, auth)

# Check for errors
stop_for_status(request)
if (http_type(request) != "text/csv")
    stop(header$date, ": GET did not result in the correct content type.")


col_types <- cols(
    Sno  = col_double(),
    Date = col_date(format = "%d/%m/%y"),
    Time = col_time(),
    `State/UnionTerritory`   = col_character(),
    ConfirmedIndianNational  = col_character(),
    ConfirmedForeignNational = col_character(),
    Cured     = col_double(),
    Deaths    = col_double(),
    Confirmed = col_double()
)

data <- content(request, col_types = col_types) %>%
    select(State = `State/UnionTerritory`, Cases = Confirmed, Date) %>%
    arrange(State, Date) %>%
    mutate(State = case_when(
        State == "Delhi" ~ "NCT of Delhi",
        State == "Telengana" ~ "Telangana",
        State == "Chattisgarh" ~ "Chandigarh",
        State == "Pondicherry" ~ "Puducherry",
        State == "Andaman and Nicobar Islands" ~ "Andaman & Nicobar Island",
        State == "Jammu and Kashmir" ~ "Jammu & Kashmir",
        State == "Ladakh" ~ "Jammu & Kashmir",
        TRUE ~ State)

    # Black Magick
    ) %>% (function (x)
        add_row(x, State = setdiff(india_shp$st_nm, x$State), Date = start.date,
                Cases = NA)
    ) %>%
    mutate(State = as.factor(State)) %>%
    group_by(State, Date) %>%
    summarise(Cases = sum(Cases)) %>%
    ungroup() %>%
    spread(Date, Cases, fill = NA) %>%
    gather(matches("[0-9].+"), key = Date, value = Cases) %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date %in% c(kalends.ides, seq(max(Date) - days.back, max(Date), 1)) &
           Date >= start.date)

i <- match(data$State, india_shp$st_nm)

final_data       <- india_shp[i, ]
final_data$Cases <- data$Cases
final_data$Text <- as.character(ifelse(is.na(data$Cases), 0, data$Cases))
final_data$Date  <- data$Date
# final_data$Date <- as.factor(format.Date(data$Date, format("%e %b %Y")))

# spatial plot...
anim_day <- tm_shape(final_data) +
            tm_fill(col = "Cases", palette = "Reds", colorNA = "white",
                    showNA = F
            ) +
            tm_text(text = "Text", showNA = TRUE) + tm_facets(along = "Date", free.coords = F) +
            tm_legend(scale = 1, legend.title.size = 2, legend.text.size = 1) +
            tm_borders()

path  <- path.expand(paste0(data_repo, today))
if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

# render gif
tmap_animation(anim_day, filename = paste0(path, "/day_sp_animation.gif"),
               width = plot.width, height = plot.height, delay = plot.delay,
               dpi = plot.dpi)

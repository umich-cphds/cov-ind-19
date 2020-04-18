library(vroom)
library(tidyverse)
library(sf)
library(tmap)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today <- Sys.getenv("today")
plot.height <- 9
plot.width  <- 7
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


data <- vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
arrange(Name, Date) %>%
mutate(State = case_when(
    Name == "Ladakh" ~ "Jammu & Kashmir",
    TRUE ~ State)
) %>%
filter(Date %in% c(kalends.ides, seq(max(Date) - days.back, max(Date), 1)) &
       Date >= start.date
)

i <- match(data$Name, india_shp$st_nm)

final_data       <- india_shp[i, ]
final_data$Cases <- data$Cases
final_data$Cases <- ifelse(final_data$Cases == 0, NA, final_data$Cases)
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

path <- path.expand(paste0(data_repo, today))
if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

# render gif
tmap_animation(anim_day, filename = paste0(path, "/day_sp_animation.gif"),
               width = plot.width, height = plot.height, delay = plot.delay,
               dpi = plot.dpi)

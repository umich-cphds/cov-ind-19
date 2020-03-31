library(tidyverse)
library(vroom)
library(plotly)


start.date <- as.Date("2020-03-01")
latest <- Sys.Date()

data <- vroom(paste0("~/cov-ind-19-data/", latest, "/jhu_data.csv")) %>%
group_by(Country) %>% filter(Case >= 100) %>%
arrange(Date) %>%
mutate(Day = seq(n()))

Day.max <- 30 # nrow(data %>% filter(Country == "India"))
data <- filter(data, Day <= Day.max) %>%
mutate(Date = format(Date, format = "%b %d")) %>%
ungroup()

title <- paste("Cumulative number of COVID-19 cases in India compared",
               "to other countries affected by the pandemic")

cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
              format(latest, format = "%b %d"), sep = ' ')

axis.title.font <- list(size = 16)
tickfont        <- list(size = 16)

xaxis <- list(title = "Days since cumulative cases passed 100",
              titlefont = axis.title.font, showticklabels = TRUE,
              tickangle = 0, showline = T, zeroline = F)

yaxis <- list(title = "Cumulative number of reported cases", titlefont =
              axis.title.font, tickfont = tickfont, zeroline = F,
              showline = F)

data$text <- paste0(data$Date, ": ",
                    format(data$Case, big.mark = ",", scientific = FALSE, trim = T),
                    " cases")

colors <- c(
    "China"       = "#ED553B",
    "South Korea" = "#56738A",
    "Italy"       = "#0472CF",
    "Iran"        = "#173F5F",
    "France"      = "#3CAEA3",
    "Germany"     = "#f2c82e",
    "US"          = "#822F21",
    "India"       = "#138808"
)

p <- plot_ly(data %>% filter(Country == "India"), x = ~ Day, y = ~Case,
        text = ~text, color = ~Country, colors = colors, type = "scatter",
        mode = "lines+markers", hoverinfo = "text",
        line = list(width = 4)) %>%
layout(xaxis = xaxis, yaxis = yaxis,
       title = list(text = cap, xanchor = "left", x = 0),
       legend = list(orientation = "h", font = list(size = 16), y = -0.2),
       margin = list(b = 200)
)


p2 <- p %>%
add_trace(data = data %>% filter(Country != "India"), x = ~ Day,
          y = ~Case, text = ~text, color = ~Country,
          type = "scatter", mode = "lines+markers",
          hoverinfo = "text", line = list(width = 4))

saveRDS(p2, paste0("~/cov-ind-19-data/", latest, "/plot2.RDS"))

p3 <- p %>%
add_trace(data = data %>% filter(Country != "India"), x = ~ Day,
          y = ~Case, text = ~text, color = ~Country,
          type = "scatter", mode = "lines+markers",
          hoverinfo = "text", line = list(width = 4),
          visible = "legendonly")

saveRDS(p3, paste0("~/cov-ind-19-data/", latest, "/plot3.RDS"))

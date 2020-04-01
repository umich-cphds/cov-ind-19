library(tidyverse)
library(vroom)
library(plotly)

start.date <- as.Date("2020-03-01")
latest <- Sys.Date()

data <- vroom(paste0("~/cov-ind-19-data/", latest, "/jhu_data.csv")) %>%
filter(Country == "India") %>%
mutate_at(vars(Case, Recovered, Death), list(function(x) {
    y <- x - lag(x)
    ifelse(y < 0, 0, y)
})) %>%
filter(Date >= start.date) %>%
gather(Case, Recovered, Death, key = Type, value = Count) %>%
mutate(Date = as.factor(format(Date, format = "%b %e"))) %>%
mutate(Type = factor(
recode(Type,
    Case = "New Cases",
    Recovered = "Recovered",
    Death = "Fatalities"
), levels = c("New Cases", "Fatalities", "Recovered")))

cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
              format(latest, format = "%b %e"), sep = ' ')

title <- paste("Daily number of COVID-19 new cases, fatalities and",
               "recovered cases in India since March 1")

data$text <- paste0(data$Date, ": ",
                    format(data$Count, big.mark = ",",
                    scientific = F, trim = T), " ", data$Type)

axis.title.font <- list(size = 16)
tickfont        <- list(size = 16)

xaxis <- list(title = "", titlefont = axis.title.font,
              showticklabels = TRUE, tickangle = -30, zeroline = F)

yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
              tickfont = tickfont, zeroline = T)
colors <- c(
  "Fatalities" = "#ED553B",
  "New Cases"  = "#f2c82e",
  "Recovered"  = "#138808"
)

p <- plot_ly(data, x = ~Date, y = ~Count, color = ~Type, text = ~text,
        type = "bar", colors = colors,
        hoverinfo = "text") %>%
layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis,
       title = list(text = cap, xanchor = "left", x = 0),
       legend = list(orientation = "h", font = list(size = 16))
) %>%
plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))


saveRDS(p, file = paste0("~/cov-ind-19-data/", latest, "/plot1.RDS"))

suppressPackageStartupMessages({
  library(data.table)
  library(scales)
  library(covid19india)
  library(plotly)
})

plot_fig_1 <- function(start.date = as.Date(Sys.getenv("today")) - 365) {
  
  data <- get_nat_counts()[date >= start.date]
  
  data <- melt(data,
       id.vars       = c("place", "date"),
       measure.vars  = c("daily_cases", "daily_deaths", "daily_recovered"),
       variable.name = "Type",
       value.name    = "Count")[
         , date.fmt := as.factor(format(date, format = "%b %e"))
       ][]
  
  data <- data[Type == "daily_cases", Type := "New Cases"][
    Type == "daily_recovered", Type := "Recovered"][
      Type == "daily_deaths", Type := "Fatalities"][
        , Type := factor(Type, levels = c("New Cases", "Fatalities", "Recovered"))][
    , count.fmt := format(Count, big.mark = ",", scientific = F, trim = T)
  ][
    , text := paste0(date.fmt, ": ", count.fmt, " ", Type)
  ][]
  
  cap <- paste0("\uA9 COV-IND-19 Study Group. Data through ",
                trimws(format(max(data[, date]), format = "%B")), " ",
                trimws(format(max(data[, date]), format = "%e")))

    title <- paste("Daily number of COVID-19 new cases, fatalities and",
                   "recovered cases in India since", format(start.date, "%b %e"))

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = -30, zeroline = F)

    yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                  tickfont = tickfont, zeroline = T)
    colors <- c(
        "Fatalities" = "#ED553B",
        "New Cases"  = "#f2c82e",
        "Recovered"  = "#138808"
    )

    p <- plot_ly(data, x = ~date, y = ~Count, color = ~Type, text = ~text,
                 type = "bar", colors = colors, hoverinfo = "text"
    ) %>%
    layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    p
}

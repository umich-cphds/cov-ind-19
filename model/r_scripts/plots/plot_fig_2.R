suppressPackageStartupMessages({
  library(data.table)
  library(scales)
  library(covid19india)
  library(plotly)
})

plot_fig_2 <- function(start.date = as.Date(Sys.getenv("today")) - 365)
{
    Day.max          <- 100
    cases.threshold  <- 100
    deaths.threshold <- 3

    fmt  <- function(x) format(x, big.mark = ",", scientific = F, trim = T)
    
    data <- fread(paste0(data_repo, "/", today, "/jhu_data_mod.csv"))[
      , Date := as.Date(Date)
      ]
    
    min_date <- unique(data[, .SD[Cases >= cases.threshold], by = "Country"][
      , min(Date)])


    # cases data -----------
    cases_data <- data[Cases >= cases.threshold][order(Date)][
      , Day := 1:.N, by = "Country"
      ][Day > 30][
        order(Date),
        Incident_Cases := Cases - data.table::shift(Cases),
        by = "Country"][
          , Cases_fmt := fmt(Incident_Cases)
        ][
          , text := paste0(Country, "<br>", Date, ": ", Cases_fmt,
                          " incident cases<br>")
        ][,
          loess_cases := c(0, predict(loess(formula = Incident_Cases ~ Day, span = 0.15))), by = "Country"][
            Country != "India",
            .(Country, Date, Incident_Cases, Cases_fmt, text, loess_cases, Day)][]
    
    
    # deaths data ---------- 
    deaths_data <- data[Deaths >= deaths.threshold][order(Date)][
      , Day := 1:.N, by = "Country"
    ][Day > 30][
      order(Date),
      Incident_Deaths := Deaths - data.table::shift(Deaths),
      by = "Country"][
        , Deaths_fmt := fmt(Incident_Deaths)
      ][
        , text := paste0(Country, "<br>", Date, ": ", Deaths_fmt,
                         " incident deaths<br>")
      ][,
        loess_deaths := c(0, predict(loess(formula = Incident_Deaths ~ Day, span = 0.15))), by = "Country"][
          Country != "India",
          .(Country, Date, Incident_Deaths, Deaths_fmt, text, loess_deaths, Day)][]
    
    # india data -----------
    india_data <- covid19india::get_nat_counts()
    
    india_cases <- india_data[total_cases >= cases.threshold][
      order(date), Day := 1:.N][Day > 30][]
    setnames(india_cases,
             old = c("place", "date", "daily_cases"),
             new = c("Country", "Date", "Incident_Cases"))
    india_cases <- india_cases[
      , Cases_fmt := fmt(Incident_Cases)
    ][
      , text := paste0(Country, "<br>", Date, ": ", Cases_fmt,
                       " incident cases<br>")
    ][
      , loess_cases := c(predict(loess(formula = Incident_Cases ~ Day, span = 0.15)))
    ][, .(Country, Date, Incident_Cases, Cases_fmt, text, loess_cases, Day)]
    
    india_deaths <- india_data[total_deaths >= deaths.threshold][
      order(date), Day := 1:.N][Day > 30][]
    setnames(india_deaths,
             old = c("place", "date", "daily_deaths"),
             new = c("Country", "Date", "Incident_Deaths"))
    india_deaths <- india_deaths[
      , Deaths_fmt := fmt(Incident_Deaths)
    ][
      , text := paste0(Country, "<br>", Date, ": ", Deaths_fmt,
                       " incident deaths<br>")
    ][
      , loess_deaths := c(predict(loess(formula = Incident_Deaths ~ Day, span = 0.15)))
    ][, .(Country, Date, Incident_Deaths, Deaths_fmt, text, loess_deaths, Day)]
    
    # combine ----------
    cases_data  <- rbindlist(list(cases_data, india_cases), fill = TRUE)
    deaths_data <- rbindlist(list(deaths_data, india_deaths), fill = TRUE)

    cases.title <- paste("COVID-19 cases in India compared",
                         "to other countries")

    deaths.title <- paste("COVID-19 deaths in India compared",
                          "to other countries")

    cap <- paste0("\uA9 COV-IND-19 Study Group. Data through ",
                  trimws(format(max(data[, Date]), format = "%B")), " ",
                  trimws(format(max(data[, Date]), format = "%e")))

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)
    
    x.date.span = as.numeric(as.Date(today) - as.Date(min_date))

    cases.xaxis <- list(title = paste0("Days since cumulative cases passed ",
                                       cases.threshold),
                        titlefont = axis.title.font, showticklabels = TRUE,
                        tickangle = 0, showline = T, zeroline = F,
                        range = list(30, x.date.span + 14)
    )

    deaths.xaxis <- list(title = paste0("Days since cumulative deaths passed ",
                                        deaths.threshold),
                         titlefont = axis.title.font, showticklabels = TRUE,
                         tickangle = 0, showline = T, zeroline = F,
                         range = list(30, x.date.span + 14)
    )


    cases.yaxis <- list(title = "Incident number of reported cases", titlefont =
                  axis.title.font, tickfont = tickfont, zeroline = F,
                  showline = F)

    deaths.yaxis <- list(title = "Incident number of reported deaths",
                        titlefont = axis.title.font, tickfont = tickfont,
                        zeroline = F, showline = F)

    else_color  <- "#999999"
    india_color <- "#138808"
    
    colors <- c(viridis::plasma(length(unique(data$Country)) - 1, begin = 0, end = 0.8),
                india_color)

    names(colors) <- c(setdiff(unique(data$Country), "India"), "India")

    case_plot <- plot_ly(cases_data, x = ~ Day, y = ~loess_cases, text = ~text,
                         color = ~Country, colors = colors,
                         legendgroup = ~Country, hoverinfo = "text",
                         mode = "markers+lines", hoverlabel = list(align = "left"),
                         showlegend = F, line = list(width = 3)
    ) %>%
    add_trace(
        data = cases_data[Country != "India"], type = "scatter",
        mode = "lines", visible = "legendonly", line = list(width = 2)
    ) %>%
    add_trace(data = cases_data[Country == "India"],
              type = "scatter", mode = "lines", line = list(width = 3)
    )  %>%
    layout(xaxis = cases.xaxis, yaxis = cases.yaxis,
           annotations = list(text = cases.title, xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22))
    )

    death_plot <- plot_ly(deaths_data, x = ~ Day, y = ~loess_deaths, text = ~text,
                          color = ~Country, colors = colors,legendgroup = ~Country,
                          hoverinfo = "text", mode = "markers+lines",
                          hoverlabel = list(align = "left"), showlegend = T
    ) %>%
    add_trace(
        data = deaths_data[Country != "India"], type = "scatter",
        mode = "lines", visible = "legendonly", line = list(width = 2)
    ) %>%
    add_trace(data = deaths_data[Country == "India"],
              type = "scatter", mode = "lines", line = list(width = 3)
    ) %>%
    layout(xaxis = deaths.xaxis, yaxis = deaths.yaxis,
           annotations = list(text = deaths.title, xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22))
    )

    subplot(case_plot, death_plot, titleX = T, titleY = T, margin = .08,
            nrows = 2, shareX = F) %>%
            plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
}

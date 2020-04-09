plot_fig_23 <- function(start.date = as.Date("2020-03-01"),
                       latest = Sys.Date())
{
    data <- vroom(paste0("~/cov-ind-19-data/", latest, "/jhu_data.csv")) %>%
    group_by(Country) %>% filter(Cases >= 100) %>%
    arrange(Date) %>%
    mutate(Day = seq(n()))

    Day.max <- 30 # nrow(data %>% filter(Country == "India"))
    data <- filter(data, Day <= Day.max) %>%
    mutate(Date = format(Date, format = "%b %e")) %>%
    ungroup() %>%
    mutate(num.fmt = format(Cases, big.mark = ",", scientific = F, trim = T)) %>%
    mutate(text = paste0(Date, ": ", num.fmt, " cumulative cases"))


    title <- paste("Cumulative number of COVID-19 cases in India compared",
                   "to other countries affected by the pandemic")

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                  format(latest, format = "%b %e"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "Days since cumulative cases passed 100",
                  titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = 0, showline = T, zeroline = F)

    yaxis <- list(title = "Cumulative number of reported cases", titlefont =
                  axis.title.font, tickfont = tickfont, zeroline = F,
                  showline = F)


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

    p <- plot_ly(data %>% filter(Country == "India"), x = ~ Day, y = ~Cases,
                 text = ~text, color = ~Country, colors = colors,
                 type = "scatter", mode = "lines+markers", hoverinfo = "text",
                 line = list(width = 4), hoverlabel = list(align = "left")
    ) %>%
    layout(xaxis = xaxis, yaxis = yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16), y = -0.2),
           margin = list(b = 100)
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))


    p2 <- p %>%
    add_trace(data = data %>% filter(Country != "India"), x = ~ Day,
              y = ~Cases, text = ~text, color = ~Country,
              type = "scatter", mode = "lines+markers",
              hoverinfo = "text", line = list(width = 4))

    p3 <- p %>%
    add_trace(data = data %>% filter(Country != "India"), x = ~ Day,
                y = ~Cases, text = ~text, color = ~Country,
                type = "scatter", mode = "lines+markers",
                hoverinfo = "text", line = list(width = 4),
                visible = "legendonly"
    )

    vroom_write(data, path = paste0("~/cov-ind-19-data/", latest, "/plot23.csv"),
                delim = ","
    )
    list(p2 = p2, p3 = p3)
}

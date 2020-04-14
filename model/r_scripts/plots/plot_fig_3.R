plot_fig_3 <- function(start.date = as.Date("2020-03-01"))
{
    Day.max <- 60

    data <- vroom(paste0(data_repo, today, "/jhu_data_mod.csv"))
    cases.data <- data %>%
    group_by(Country) %>% filter(Cases >= 100) %>%
    arrange(Date) %>%
    mutate(Day = seq(n())) %>%
    filter(Day <= Day.max) %>%
    mutate(Date = format(Date, format = "%b %e")) %>%
    ungroup() %>%
    mutate(num.fmt = format(Cases, big.mark = ",", scientific = F, trim = T)) %>%
    mutate(text = paste0(Country, "<br>", Date, ": ", num.fmt, " cumulative cases<br>"))

    deaths.data <- data %>%
    group_by(Country) %>% filter(Deaths >= 3) %>%
    arrange(Date) %>%
    mutate(Day = seq(n())) %>%
    filter(Day <= Day.max) %>%
    mutate(Date = format(Date, format = "%b %e")) %>%
    ungroup() %>%
    mutate(num.fmt = format(Deaths, big.mark = ",", scientific = F, trim = T)) %>%
    mutate(text = paste0(Country, "<br>", Date, ": ", num.fmt, " cumulative deaths<br>"))

    title <- paste("Cumulative number of COVID-19 cases in India compared",
                   "to other countries affected by the pandemic")

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    cases.xaxis <- list(title = "Days since cumulative cases passed 100",
                  titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = 0, showline = T, zeroline = F,
                  range = list(1, 70)
    )
    deaths.xaxis <- list(title = "Days since cumulative deaths passed 3",
                  titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = 0, showline = T, zeroline = F,
                  range = list(1, 70)
    )

    cases.yaxis <- list(title = "Cumulative number of reported cases",
                        titlefont = axis.title.font, tickfont = tickfont,
                        zeroline = F, showline = F, dtick = 1, type = "log",
                        range = list(2, 6))

    deaths.yaxis <- list(title = "Cumulative number of reported deaths",
                         titlefont = axis.title.font, tickfont = tickfont,
                         zeroline = F, showline = F, dtick = 1, type = "log",
                         range = list(0, 5))

    cases.doubles <- tibble(Day = 1:60, `3 Days` = 100 * 2 ^ (1 / 3 * 1:60),
                      `1 Week` = 100 * 2 ^ (1 / 7 * 1:60),
                      `2 Weeks` = 100 * 2 ^ (1 / 14 * 1:60)
    ) %>% gather(`3 Days`, `1 Week`, `2 Weeks`, key = Country, value = Cases)

    deaths.doubles <- tibble(Day = 1:60, `3 Days` = 3 * 2 ^ (1 / 3 * 1:60),
                      `1 Week` = 3 * 2 ^ (1 / 7 * 1:60),
                      `2 Weeks` = 3 * 2 ^ (1 / 14 * 1:60)
    ) %>% gather(`3 Days`, `1 Week`, `2 Weeks`, key = Country, value = Deaths)

    cases.data <- add_row(cases.data, !!!cases.doubles)
    deaths.data <- add_row(deaths.data, !!!deaths.doubles)

    else_color  <- "#999999"
    india_color <- "#138808"
    line_color  <- "#aaaaaa"
    # viridis::plasma(length(unique(data$Country)) - 4
    # rep(else_color, length(unique(data$Country)) - 4)
    colors <- c(viridis::plasma(length(unique(cases.data$Country)) - 4),
                india_color, rep(line_color, 3))

    names(colors) <- c(
        setdiff(unique(cases.data$Country), c("3 Days", "1 Week", "2 Weeks", "India")),
        c("India", "3 Days", "1 Week", "2 Weeks")
    )

    p1 <- plot_ly(cases.data, type = "scatter", color = ~Country, colors = colors,
                 mode = "line", hoverinfo = "text", hoverlabel = list(align = "left")
    ) %>%
    add_trace(data = filter(cases.data, Country %in% c("3 Days", "1 Week", "2 Weeks")),
              x = ~Day, y = ~Cases, type = "scatter",
              showlegend = F, line = list(dash = "dash", width = 2)
    ) %>%
    add_trace(data = filter(cases.data, !(Country %in% c("India", "3 Days", "1 Week", "2 Weeks"))),
              x = ~Day, y = ~Cases, text = ~text, type = "scatter",
              showlegend = T, line = list(width = 2), visible = "legendonly"
    ) %>%
    add_trace(data = filter(cases.data, Country == "India"), type = "scatter",
              x = ~Day, y = ~Cases, text = ~text,
              showlegend = T, line = list(width = 3)
    ) %>%
    layout(xaxis = cases.xaxis, yaxis = cases.yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16), y = -0.2),
           margin = list(b = 100)
    ) %>%
    add_annotations(
        x = c(40, 60, 60),
        y = 2 + c(log10(2 ^ (1 / 3 * 39.863)),
                    log10(2 ^ (1 / 7 * 60)),
                    log10(2 ^ (1 / 14 * 60))
        ),
        text = paste0("Doubles every ", c("3 days", "week", "2 weeks")),
        font = list(size = 18),
        xref = "x",
        xanchor = "left",
        yref = "y",
        showarrow = F
    ) %>%
        plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    p2 <- plot_ly(deaths.data, type = "scatter", color = ~Country, colors = colors,
                 mode = "line", hoverinfo = "text", hoverlabel = list(align = "left"),
                 legendgroup = ~Country
    ) %>%
    add_trace(data = filter(deaths.data, Country %in% c("3 Days", "1 Week", "2 Weeks")),
              x = ~Day, y = ~Deaths, type = "scatter",
              showlegend = F, line = list(dash = "dash", width = 2)
    ) %>%
    add_trace(data = filter(deaths.data, !(Country %in% c("India", "3 Days", "1 Week", "2 Weeks"))),
              x = ~Day, y = ~Deaths, text = ~text, type = "scatter",
              showlegend = T, line = list(width = 2), visible = "legendonly"
    ) %>%
    add_trace(data = filter(deaths.data, Country == "India"), type = "scatter",
              x = ~Day, y = ~Deaths, text = ~text,
              showlegend = T, line = list(width = 3)
    ) %>%
    layout(xaxis = deaths.xaxis, yaxis = deaths.yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16), y = -0.2),
           margin = list(b = 100)
    ) %>%
    add_annotations(
        x = c(45, 60, 60),
        y = log10(3) + c(log10(2 ^ (1 / 3 * 45)),
                    log10(2 ^ (1 / 7 * 60)),
                    log10(2 ^ (1 / 14 * 60))
        ),
        text = paste0("Doubles every ", c("3 days", "week", "2 weeks")),
        font = list(size = 18),
        xref = "x",
        xanchor = "left",
        yref = "y",
        showarrow = F
    ) %>%
        plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    list(p3a = p1, p3b = p2)
}

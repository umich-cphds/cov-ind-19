data <- vroom(paste0("~/cov-ind-19-data/", latest, "/jhu_data.csv")) %>%
group_by(Country) %>% filter(Case >= 100) %>%
arrange(Date) %>%
mutate(Day = seq(n()))

Day.max <- 60 # nrow(data %>% filter(Country == "India"))
data <- filter(data, Day <= Day.max) %>%
mutate(Date = format(Date, format = "%b %e")) %>%
ungroup() %>%
mutate(num.fmt = format(Case, big.mark = ",", scientific = F, trim = T)) %>%
mutate(text = paste0(Date, ": ", num.fmt, " cumulative cases"))

title <- paste("Cumulative number of COVID-19 cases in India compared",
               "to other countries affected by the pandemic")

cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
              format(latest, format = "%b %e"), sep = ' ')

axis.title.font <- list(size = 16)
tickfont        <- list(size = 16)

xaxis <- list(title = "Days since cumulative cases passed 100",
              titlefont = axis.title.font, showticklabels = TRUE,
              tickangle = 0, showline = T, zeroline = F,
              range = list(1, 60)
)

yaxis <- list(title = "Cumulative number of reported cases", titlefont =
              axis.title.font, tickfont = tickfont, zeroline = F,
              showline = F, dtick = 1, type = "log", range = list(2, 6))

colors <- c(
    "China"       = "#ED553B",
    "South Korea" = "#56738A",
    "Italy"       = "#0472CF",
    "Iran"        = "#173F5F",
    "France"      = "#3CAEA3",
    "Germany"     = "#f2c82e",
    "US"          = "#822F21",
    "India"       = "#138808",
    "3 Days"      = "#cccccc",
    "1 Week"      = "#cccccc",
    "2 Weeks"     = "#cccccc"
)

doubles <- tibble(Day = 1:60, `3 Days` = 100 * 2 ^ (1 / 3 * 1:60),
                  `1 Week` = 100 * 2 ^ (1 / 7 * 1:60),
                  `2 Weeks` = 100 * 2 ^ (1 / 14 * 1:60)
) %>% gather(`3 Days`, `1 Week`, `2 Weeks`, key = Country, value = Case)


data <- add_row(data, !!!doubles)

colors <- c(
    "3 Days"      = "#aaaaaa",
    "1 Week"      = "#aaaaaa",
    "2 Weeks"     = "#aaaaaa",
    "India"       = "#138808",
    "China"       = "#ED553B",
    "South Korea" = "#56738A",
    "Italy"       = "#0472CF",
    "Iran"        = "#173F5F",
    "France"      = "#3CAEA3",
    "Germany"     = "#f2c82e",
    "US"          = "#822F21"
)

p <- plot_ly(type = "scatter", mode = "line", hoverinfo = "text",
             hoverlabel = list(align = "left")
) %>%
add_trace(data = d2, x = ~Day, y = ~ `3 Days`, showlegend = F,
          line = list(dash = "dash", width = 2, color = colors["3 Days"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `1 Week`, showlegend = F,
          line = list(dash = "dash", width = 2, color = colors["1 Week"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `2 Weeks`, showlegend = F,
          line = list(dash = "dash", width = 2, color = colors["2 Weeks"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ India, name = "India", text = ~text,
          line = list(width = 3, color = colors["India"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ China, name = "China", text = ~text,
          line = list(width = 3, color = colors["China"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `South Korea`, name = "South Korea", text = ~text,
          line = list(width = 3, color = colors["South Korea"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `Italy`, name = "Italy", text = ~text,
          line = list(width = 3, color = colors["Italy"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `Iran`, name = "Iran",text = ~text,
          line = list(width = 3, color = colors["Iran"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `France`, name = "France", text = ~text,
          line = list(width = 3, color = colors["France"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `Germany`, name = "Germany", text = ~text,
          line = list(width = 3, color = colors["Germany"])
) %>%
add_trace(data = d2, x = ~Day, y = ~ `US`, name = "US", text = ~text,
          line = list(width = 3, color = colors["US"])
) %>%
layout(xaxis = xaxis, yaxis = yaxis, title =
       list(text = cap, xanchor = "left", x = 0), legend =
       list(orientation = "h", font = list(size = 16), y = -0.2),
       margin = list(b = 100)
) %>%
add_annotations(
    x = c(37, 57, 57),
    y = 1.9 + c(log10(2 ^ (1 / 3 * 37)), log10(2 ^ (1 / 7 * 57)), log10(2 ^ (1 / 14 * 57))),
    text = paste0("Doubles every ", c("3 days", "week", "2 weeks")),
    font = list(size = 18),
    textangle = c(
        -atan(2 / 3) * 180 / pi,
        -atan(2 / 7) * 180 / pi,
        -atan(2 / 14) * 180 / pi),
    xref = "x",
    yref = "y",
    showarrow = F
) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

plot_fig_5b <- function(forecast, start.date = as.Date(today),
                        end.date = end.date <- as.Date("2020-09-15"))
{
    data <- vroom(paste0(data_repo, today, "/1wk/", forecast,
                            "_figure_5_inc_data.csv")
    ) %>%
    mutate(Dates = as.Date(Dates)) %>%
    filter(Dates >= start.date & Dates <= end.date & variable != "mod_3") %>%
    drop_na() %>%
    mutate(date.fmt = paste0(format(Dates, "%b %d")),
           val.fmt = format(round(value), big.mark = ",", scientific = FALSE,
                            trim = T)
    ) %>%
    mutate(
        text = paste0(paste0(date.fmt, ": ", val.fmt,
                      " projected cases per day")
        )
    ) %>%
    group_by(color) %>%
    mutate(value = predict(loess(value ~ as.numeric(Dates), span = .2))) %>%
    filter(color != 'Normal (pre-intervention)')


    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
    format(today, format = "%b %d"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
    tickangle = -30, zeroline = F)

    yaxis <- list(title = "Number of new infected cases per 100,000 per day",
    titlefont = axis.title.font, zeroline = T)

    colors <- c("#173F5F", "#0472CF", "#3CAEA3", "#f2c82e")
    p <- plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
        color = ~ color, colors = colors, type = "scatter",
        mode = "lines", hoverinfo = "text", line = list(width = 4),
        hoverlabel = list(align = "left")
    ) %>%
    layout(xaxis = xaxis, yaxis = yaxis,
        title = list(text = cap, xanchor = "left", x = 0),
        legend = list(orientation = "h", font = list(size = 16))
        # shapes = lines
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    p
}

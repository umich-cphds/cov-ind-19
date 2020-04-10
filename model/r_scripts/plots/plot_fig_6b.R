plot_fig_6b <- function(forecast, start.date = as.Date("2020-04-30"),
                        end.date = end.date <- as.Date("2020-08-31"),
                        latest = Sys.getenv("today"))
{
    data <- vroom(paste0("~/cov-ind-19-data/", latest, "/1wk/", forecast,
                            "_figure_5_data.csv")
    ) %>%
    mutate(Dates = as.Date(Dates)) %>%
    filter(Dates >= start.date & Dates <= end.date & variable != "mod_3") %>%
    mutate(date.fmt = paste0(format(Dates, "%b %d")),
           val.fmt = format(round(value), big.mark = ",", scientific = FALSE,
                            trim = T)
    ) %>%
    mutate(
        text = paste0(paste0(date.fmt, ": ", val.fmt,
                      " projected cases per day")
        )
    )

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
    format(latest, format = "%b %d"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
    tickangle = -30, zeroline = F)

    yaxis <- list(title = "Number of new infected cases per 100,000 per day",
    titlefont = axis.title.font, zeroline = T)


    colors <- c("#173F5F", "#0472CF", "#3CAEA3", "#f2c82e")
    p <- plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
        color = ~ color, colors = colors, type = "scatter",
        mode = "line", hoverinfo = "text", line = list(width = 4),
        hoverlabel = list(align = "left")
    ) %>%
    layout(xaxis = xaxis, yaxis = yaxis,
        title = list(text = cap, xanchor = "left", x = 0),
        legend = list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    # vroom_write(data, path = paste0("~/cov-ind-19-data/", latest, "/plot6b.csv"),
    #             delim = ","
    # )
    p
}

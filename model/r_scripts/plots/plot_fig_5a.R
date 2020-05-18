plot_fig_5a <- function(forecast, start.date = as.Date("2020-05-15"),
                        end.date = end.date <- as.Date("2020-07-15"))
{
    data <- vroom(paste0(data_repo, today, "/1wk/", forecast,
                            "_figure_5_data.csv")
    ) %>%
    mutate(Dates = as.Date(Dates)) %>%
    filter(Dates >= start.date & Dates <= end.date & variable != "mod_3") %>%
    mutate(date.fmt = paste0(format(Dates, "%b %d")),
           val.fmt = format(round(value), big.mark = ",", scientific = FALSE,
                            trim = T),
            ci.fmt = format(round(upper_ci), big.mark = ",", scientific = FALSE,
                            trim = T),
    ) %>%
    mutate(
        text = paste0(paste0(date.fmt, ": ", val.fmt, " projected total cases"),
                      paste0("<br>Projection upper CI: ", ci.fmt, " cases<br>")
        )
    ) %>% 
    filter(color != 'Normal (pre-intervention)')

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
    format(today, format = "%b %d"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
    tickangle = -30, zeroline = F)

    yaxis <- list(title = "Cumulative number of infected cases per 100,000",
    titlefont = axis.title.font, zeroline = T)

    anno.data <- filter(data, as.character(Dates) %in% c("2020-05-15", "2020-06-15",
                                           "2020-07-15", "2020-08-15")
    ) %>%
    group_by(Dates) %>% summarise(diff = (max(value) - min(value)),
                                  value = max(value) * 1e5 / 1.34e9
    ) %>%
    mutate(y = ifelse(50 + value < 1.2 * value, 1.2 * value, 50 + value))

    line <- list(
        type = "line",
        xref = "x",
        yref = "y",
        y0 = 0,
        layer = "below",
        line = list(color = "#aaa", width = 3, dash = "dot")
    )

    lines <- list()
    for (i in seq(nrow(anno.data))) {
        line$x0 <- anno.data$Dates[i]
        line$x1 <- anno.data$Dates[i]
        line$y1 <- anno.data$y[i] - 10
        lines[[i]] <- line
    }


    colors <- c("#173F5F", "#0472CF", "#3CAEA3", "#f2c82e")
    p <- plot_ly(data, x = ~Dates, y = ~ value * 1e5 / 1.34e9, text = ~text,
        color = ~ color, colors = colors, type = "scatter", mode = "lines",
        hoverinfo = "text", hoverlabel = list(align = "left"),
        line = list(width = 4)
    ) %>%
    layout(xaxis = xaxis, yaxis = yaxis,
        title = list(text = cap, xanchor = "left", x = 0),
        legend = list(orientation = "h", font = list(size = 16))
        #shapes = lines
    ) %>%
    # add_annotations(
    #     x = anno.data$Dates,
    #     y = anno.data$y,
    #     text = paste0("Difference between social distancing and <br>cautious return on ",
    #                   format(anno.data$Dates, "%B %e"), ": ",
    #                   format(anno.data$diff, big.mark = ",", trim = T, sci = F),
    #                   " cases<br>"
    #     ),
    #     align = "left",
    #     font = list(size = 18),
    #     xref = "x",
    #     yref = "y",
    #     showarrow = F
    # ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    # vroom_write(data, path = paste0(data_repo, todau, "/plot5a.csv"),
    #             delim = ","
    # )
    p
}

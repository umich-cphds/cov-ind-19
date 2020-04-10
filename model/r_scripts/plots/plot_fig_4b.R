plot_fig_4b <- function(forecast, start.date = as.Date("2020-03-01"),
                        end.date = as.Date("2020-04-30"),
                        latest = Sys.getenv("today"))
{
    data <- vroom(paste0("~/cov-ind-19-data/", latest, "/2wk/", forecast,
                            "_figure_4_data.csv")
    ) %>%
    mutate(
        color = factor(color, levels= c("Observed", "No intervention",
            "Social distancing", "Lockdown with moderate release")),
        date.fmt = format(Dates, format("%b %e")),
        val.fmt = format(value, big.mark = ",", scientific = F, trim = T),
        ci.fmt = format(upper_ci, big.mark = ",", scientific = F, trim = T)
    ) %>%
    mutate(
        color = recode(color, "Lockdown with moderate release" = "Lockdown with moderate return"),
        j = color == "Lockdown with moderate return"
    ) %>%
    filter(Dates <= end.date) %>%
    mutate(text = case_when(
        color == "Observed" ~ paste0(date.fmt, ": ", val.fmt, " observed cases"),
        TRUE ~ paste0(paste0(date.fmt, ": ", val.fmt, " projected cases"),
                      paste0("<br>Projection upper CI: ", ci.fmt, " cases<br>")
        )
    ))

    title <- paste("Cumulative number of COVID-19 cases in India compared",
                   "to other countries affected by the pandemic")

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                  format(latest, format = "%b %e"), sep = ' ')

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = -30, showline = T, zeroline = T, x0 =
                  format(latest, format = "%b %e"))

    yaxis <- list(title = "Cumulative number of cases", type = "log",
                  dtick = 1, titlefont = axis.title.font, zeroline = T,
                  showline =T)

    colors <- c("#979799", "#ED553B", "#f2c82e", "#173F5F")

    p <- plot_ly(data, x = ~ Dates, y = ~ value, text = ~text,
                 color = ~color, colors = colors, type = "bar",
                 hoverinfo = "text", hoverlabel = list(align = "left")
    ) %>%
    layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
           title = list(text = cap, xanchor = "left", x = 0),
           legend = list(orientation = "h", font = list(size = 16)),
           shapes = list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = latest,
                         x1 = latest, layer = "below",
                         line = list(color = "#eee", size = 3)
                     )
    ) %>%
    add_trace(data = filter(data, j), x = ~Dates, y = ~upper_ci,
        name = paste(filter(data, j)$color, "upper CI"), type = "scatter",
        mode = "line", line = list(width = 3, dash = "dash")
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))


    # vroom_write(data, path = paste0("~/cov-ind-19-data/", latest, "/", forecast,
    #                                 "/plot4a.csv"), delim = ","
    # )
    p
}

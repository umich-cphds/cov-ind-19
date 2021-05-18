suppressPackageStartupMessages(library(plotly))

plot_fig_4 <- function(forecast)
{
    data <- read_tsv(paste0(data_repo, "/", today, "/1wk/", forecast,
                            "_plot_data.txt"), col_types = cols()
    ) %>%
    mutate(
        scenario = as.factor(scenario),
        date_fmt = format(date, format("%b %e")),
        val_fmt  = format(value, big.mark = ",", scientific = F, trim = T),
        ci_fmt   = format(upper_ci, big.mark = ",", scientific = F, trim = T)
    ) %>%
    mutate(
        j = scenario == "No intervention"
    ) %>%
    #filter(Dates <= end.date) %>%
    mutate(text = case_when(
        scenario == "Observed" ~ paste0(date_fmt, ": ", val_fmt, " observed cases"),
        scenario != "Observed" ~ paste0(paste0(date_fmt, ": ", val_fmt, " projected cases"),
                      paste0("<br>Projection upper CI: ", ci_fmt, " cases<br>")
        )
    )) %>%
    filter(date <= Sys.Date() + 30 & date >= Sys.Date() - 14) %>%
    filter(scenario %in% c("Observed", "No intervention")) %>%
    mutate(scenario = factor(scenario, levels = c("Observed", "No intervention")))

    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"), sep = ' ')

    axis_title_font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis_title_font, showticklabels = TRUE,
                  tickangle = -30, showline = T, zeroline = T, x0 =
                  format(today, format = "%b %e"))

    yaxis <- list(title = "Cumulative number of cases", type = "log",
                  dtick = 1, titlefont = axis_title_font, zeroline = T,
                  showline = T)

    #colors <- c("#979799", "#f2c82e", "#173F5F")
    
    colors <- c(
        "Observed" = "#979799",
        "No intervention" = "#173F5F"
    )
    
    p <- plot_ly(data, x = ~ date, y = ~ value, text = ~text,
                 color = ~scenario, colors = colors, name = ~scenario, type = "bar",
                 hoverinfo = "text", hoverlabel = list(align = "left")
    ) %>%
    layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
           title = list(text = cap, xanchor = "left", x = 0),
           legend = list(orientation = "h", font = list(size = 16)),
           shapes = list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = today,
                         x1 = today, layer = "below",
                         line = list(color = "#eee", size = 3)
                     )
    ) %>%
    add_trace(data = filter(data, j), x = ~date, y = ~upper_ci,
              name = paste(filter(data, j)$scenario, "upper CI"), type = "scatter",
              mode = "lines", line = list(width = 3, dash = "dash")
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    # vroom_write(data, path = paste0(data_repo, today, "/", forecast,
    #                                 "/plot4a.csv"), delim = ","
    # )
    p
}

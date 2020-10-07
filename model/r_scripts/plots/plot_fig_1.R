plot_fig_1 <- function(start.date = as.Date("2020-07-01"))
{
    data <- read_tsv(paste0(data_repo, today, "/jhu_data_mod.csv"), col_types = cols()) %>%
    filter(Country == "India") %>%
    mutate_at(vars(Cases, Recovered, Deaths), list(function(x) {
        y <- x - lag(x)
        ifelse(y < 0, 0, y)
    })) %>%
    filter(Date >= start.date) %>%
    gather(Cases, Recovered, Deaths, key = Type, value = Count) %>%
    mutate(
        date.fmt = as.factor(format(Date, format = "%b %e")),
        Type = factor(
            recode(Type,
                Cases     = "New Cases",
                Recovered = "Recovered",
                Deaths    = "Fatalities"
            ), levels = c("New Cases", "Fatalities", "Recovered"))
    ) %>%
    mutate(count.fmt = format(Count, big.mark = ",",
           scientific = F, trim = T)
    ) %>%
    mutate(text = paste0(date.fmt, ": ", count.fmt, " ", Type))


    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"), sep = ' ')

    title <- paste("Daily number of COVID-19 new cases, fatalities and",
                   "recovered cases in India since March 1")

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

    p <- plot_ly(data, x = ~Date, y = ~Count, color = ~Type, text = ~text,
                 type = "bar", colors = colors, hoverinfo = "text"
    ) %>%
    layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    vroom_write(data, path = paste0(data_repo, today, "/plot1.csv"),
                delim = ","
    )
    p
}

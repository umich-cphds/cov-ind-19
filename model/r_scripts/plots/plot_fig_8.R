plot_fig_8 <- function(start.date = as.Date("2020-04-01"))
{
    data <- vroom(paste0(data_repo, "/", today, "/testing.csv"), col_types = cols()) %>%
    select(-Country) %>%

    # Since we are reporting day by day, take the highest reported values if
    # there are multiple entries for a day
    group_by(Date) %>%
    summarise(Cases = max(Cases), Tests = max(Tests)) %>%
    ungroup() %>%
    mutate(
        Cases = Cases - lag(Cases),
        Tests = Tests - lag(Tests),
        Percent = Cases / Tests * 100
    ) %>%
    gather(Cases, Tests, key = Type, value = Counts) %>%
    mutate(
        Type = recode(Type, "Cases" = "Positive tests"),
        Date.fmt    = format(Date, format = "%b %e"),
        Counts.fmt  = format(Counts, big.mark = ",", sci = F, trim = T),
        Percent.fmt = paste0(format(Percent, digits = 3, trim = T))
    ) %>%
    mutate(
        Text = paste0(Date.fmt, ": ", Counts.fmt, " ", Type,
            "<br>Percent tests positive: ", Percent.fmt
        )
    ) %>%
    filter(Date >= start.date)

    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"))

    title <- paste("Daily confirmed positive cases and total tests in India")

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
                  tickangle = -30, zeroline = F)

    yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                  tickfont = tickfont, zeroline = T)
    colors <- c(
        "Tests" = "#B7F1A0",
        "Positive tests" = "#ED553B"
    )

    p <- plot_ly(data, x = ~Date, y = ~Counts, color = ~Type, text = ~Text,
                 type = "bar", colors = colors, hoverinfo = "text",
                 hoverlabel = list(align = "left")
    ) %>%
    layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
           list(text = cap, xanchor = "left", x = 0), legend =
           list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

    # vroom_write(data, path = paste0(data_repo, today, "/plot8.csv"),
    #             delim = ","
    # )
    p
}

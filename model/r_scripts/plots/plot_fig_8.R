# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
    data_repo <- "~/cov-ind-19-data/"
    today     <- Sys.getenv("today")
} else {
    data_repo <- "~/cov-ind-19-test/"
    today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_8 <- function(start.date = as.Date("2020-04-01"))
{
    data <- vroom(paste0(data_repo, today, "/testing.csv")) %>%
    filter(Country == "India") %>%
    filter(Date >= start.date) %>%
    mutate(Date = as.factor(format(Date, format = "%b %e")),
           Cases = c(NA, diff(Cases)),
           Tests = c(NA, diff(Tests)),
           Text_cases = paste0(Date, ": ", format(Cases, big.mark = ",",
                               scientific = F, trim = T), " positive tests"),
           Text_tests = paste0(Date, ": ", format(Tests, big.mark = ",",
                               scientific = F, trim = T), " tests")
    ) %>%
    drop_na() %>%
    mutate(Percent = (Cases/Tests)*100,
           Text_cases = paste0(Text_cases, '. % positive tests: ', format(Percent, digits = 3)),
           Text_tests = paste0(Text_tests, '. % positive tests: ', format(Percent, digits = 3))
    )

  data <- bind_rows(
      data %>%
      select(-Tests, -Text_tests) %>%
      mutate(
          Count = Cases,
          Text = Text_cases,
          Type = "Positive tests"),
          data %>%
          select(-Cases, -Text_cases) %>%
          mutate(Count = Tests, Text = Text_tests, Type = "Tests")
    ) %>%
    mutate(Type = factor(Type, levels = c("Positive tests", "Tests"))) %>%
    select(Date, Count, Text, Type, Country)

    cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
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

    p <- plot_ly(data, x = ~Date, y = ~Count, color = ~Type, text = ~Text,
                 type = "bar", colors = colors, hoverinfo = "text"
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

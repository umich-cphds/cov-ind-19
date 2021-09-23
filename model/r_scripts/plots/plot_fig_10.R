suppressPackageStartupMessages({
  library(covid19india)
  library(tidyverse)
  library(data.table)
  library(plotly)
})

plot_fig_10 <- function(start.date = as.Date("2020-05-01"))
{
  
  data <- fread(paste0(data_repo, "/", today, "/global_testing.csv"), showProgress = FALSE)[, .(location, date, total_cases, total_tests)][!is.na(total_cases) & !is.na(total_tests)] |>
    {\(x) x[x[, .I[date == max(date)], by = "location"]$V1]}() |>
    na.omit() |>
    DT(, Percent := (total_cases / total_tests) * 100)
  
  popsize <- fread(paste0(code_repo, "/model/country_population.csv"))

  data <- merge(data, popsize, by.x = "location", by.y = "Country")[, Percent_of_pop := (total_tests/Population) * 100]

  data <- rbindlist(list(
    data[location != "China", !c("total_cases")] |>
      DT(, Count := total_tests) |>
      # {\(x) setnames(x, old = "total_tests", new = "Count")}() |>
      DT(, `:=` (
        Text = paste0("Total tests: ", format(Count, big.mark = ",", sci = F, trim = T)),
        Type = "Tests")),
    data[, !c("total_tests")] |>
      DT(, Count := total_cases) |>
      # {\(x) setnames(x, old = "total_cases", new = "Count")}() |>
      DT(, `:=` (
        Text = paste0("Total positive tests: ", format(Count, big.mark = ",", sci = F, trim = T)),
        Type = "Positive tests"))
  ), fill = TRUE)[, Type := factor(Type, levels = c("Positive tests", "Tests"))][, !c("total_cases", "total_tests")][
    , Text := paste0(Text, "<br>Percent positive tests: ",
                    format(Percent, digits = 3),
                    "<br>Percent of population tested: ", 
                    format(Percent_of_pop, digits = 3))][]

  cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"))

  title <- paste("Total tests across countries and percentage of tested population")

  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)

  xaxis <- list(title = "Country", titlefont = axis.title.font,
                showticklabels = TRUE)

  yaxis <- list(title = "Counts", titlefont = axis.title.font,
                tickfont = tickfont, zeroline = T)

  # colors <- c(
  #   "Tests" = "#b3b3b3",
  #   "Cases" = "#138808"
  # )

  colors <- c(
    "Tests" = "#B7F1A0",
    "Positive tests" = "#ED553B"
  )

  p <-
  plot_ly(data, x = ~location, y = ~Count, color = ~Type, text = ~Text,
          type = "bar", colors = colors, hoverinfo = "text"
  ) %>% layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
                 list(text = cap, xanchor = "left", x = 0), legend =
                 list(orientation = "h", font = list(size = 16))
  ) %>%
  plotly::config(toImageButtonOptions = list(width = NULL, height = NULL)) %>%
    layout(legend = list(x = 0, y = 1))

  p
}

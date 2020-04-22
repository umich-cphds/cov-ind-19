# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_11 <- function(start.date = as.Date("2020-04-01"))
{
  cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"))
  
  title <- paste("Daily confirmed positive cases and total tests in India")
  
  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)
  
  yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                tickfont = tickfont, zeroline = T)
  
  
  data <- vroom(paste0(data_repo, today, "/global_testing.csv")) %>%
    select(location, date, total_cases, total_tests) %>%
    group_by(location) %>%
    filter(!is.na(total_cases) & !is.na(total_tests)) %>%
    drop_na() %>%
    filter(date > as.Date('2020-04-01')) %>%
    mutate(Percent = (total_cases/total_tests)*100)
  
  data <- bind_rows(
    data %>%
      select(-total_cases) %>%
      mutate(
        Count = total_tests,
        Text = paste0('Total tests: ', total_tests, '. '),
        Type = 'Tests'),
    data %>%
      select(-total_tests) %>%
      mutate(
        Count = total_cases,
        Text = paste0('Total cases: ', total_cases, '. '),
        Type = 'Cases')
  ) %>%
    mutate(Type = factor(Type, levels = c("Cases", "Tests"))) %>%
    select(-total_cases, - total_tests) %>%
    mutate(Text = paste0(Text, 'Percent cases: ', format(Percent, digits = 3)))
  
  colors <- c(
    "Tests" = "#b3b3b3",
    "Cases" = "#138808"
  )
  
  nationbarplot = function(name) {
    plot_ly(data %>% filter(location == name), x = ~date, y = ~Count, color = ~Type, text = ~Text,
            type = "bar", colors = colors, hoverinfo = "text"
    ) %>%
      layout(barmode = "stack", xaxis = list(title = name, titlefont = axis.title.font, showticklabels = TRUE,
                                             tickangle = -30, zeroline = F), yaxis = yaxis, title =
               list(text = cap, xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16))
      ) %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  }
  
  plotList <- function(names) {
    lapply(names, nationbarplot)
  }
  
  names = c('United States', 'United Kingdom', 'Canada', 'South Korea', 'Turkey', 'India')
  
  p = subplot(plotList(names), nrows = 3, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
  p
}

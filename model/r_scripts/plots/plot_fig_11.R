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
    filter(total_cases >= 100) %>%
    mutate(Percent = (total_cases/total_tests)*100)
  
  popsize = 
    matrix(data = c('Argentina',44494502,
                    'Australia',24982688,
                    'Austria',8840521,
                    'Belgium',11433256,
                    'Bangladesh',161356039,
                    'Bulgaria',7025037,
                    'Bahrain',1569439,
                    'Bolivia',11353142,
                    'Canada',37057765,
                    'Switzerland', 8513227,
                    'Chile', 18729160,
                    'Colombia', 49648685,
                    'Costa Rica', 4999441,
                    'Cuba', 11338138,
                    'Czech Republic', 10629928,
                    'Germany', 82905782,
                    'Denmark', 5793636,
                    'Ecuador', 17084357,
                    'Spain', 46796540,
                    'Estonia', 1321977,
                    'Ethiopia', 109224559,
                    'Finland', 5515525,
                    'France', 66977107,
                    'United Kingdom', 66460344,
                    'Ghana', 29767108,
                    'Greece', 10731726,
                    'Croatia', 4087843,
                    'Hungary', 9775564,
                    'Indonesia', 267663435,
                    'India', 1352617328,
                    'Ireland', 4867309,
                    'Iran', 81800269,
                    'Iceland', 352721,
                    'Israel', 8882800,
                    'Italy', 60421760,
                    'Japan', 126529100,
                    'Kazakhstan', 18272430,
                    'Kenya', 51393010,
                    'South Korea', 51606633,
                    'Lithuania', 2801543,
                    'Luxembourg', 607950,
                    'Latvia', 1927174,
                    'Morocco', 36029138,
                    'Mexico', 126190788,
                    'Myanmar', 53708395,
                    'Malaysia', 31528585,
                    'Nigeria', 195874740,
                    'Netherlands', 17231624,
                    'Norway', 5311916,
                    'New Zealand', 4841000,
                    'Pakistan', 212215030,
                    'Panama', 4176873,
                    'Peru', 31989256,
                    'Philippines', 106651922,
                    'Poland', 37974750,
                    'Portugal', 10283822,
                    'Paraguay', 6956071,
                    'Romania', 19466145,
                    'Russia', 144478050,
                    'Rwanda', 12301939,
                    'Senegal', 15854360,
                    'Singapore', 5638676,
                    'El Salvador', 6420744,
                    'Serbia', 6982604,
                    'Slovakia', 5446771,
                    'Slovenia', 2073894,
                    'Sweden', 10175214,
                    'Thailand', 69428524,
                    'Tunisia', 11565204,
                    'Turkey', 82319724,
                    'Taiwan', 23780000,
                    'Uruguay', 3449299,
                    'United States', 326687501,
                    'Vietnam', 95540395,
                    'South Africa', 57779622), 
           ncol = 2, byrow = TRUE) 
  colnames(popsize) <- c('Country','Population')
  popsize = as.data.frame(popsize)
  popsize$Population = as.numeric(as.character(popsize$Population))
  
  data = left_join(data, popsize, by = c('location' = 'Country')) %>%
    mutate(Percent_of_pop = (total_tests/Population)*100)
  
  data <- bind_rows(
    data %>%
      select(-total_cases) %>%
      mutate(
        Count = total_tests,
        Text = paste0('Total tests: ', format(total_tests, big.mark = ",",
                      sci = F, trim = T)),
        Type = 'Tests'),
    data %>%
      select(-total_tests) %>%
      mutate(
        Count = total_cases,
        Text = paste0("Total positive tests: ", format(total_cases,
                      big.mark = ",", sci = F, trim = T)),
        Type = 'Positive tests')
  ) %>%
    mutate(Type = factor(Type, levels = c("Positive tests", "Tests"))) %>%
    select(-total_cases, - total_tests) %>%
    mutate(Text = paste0(Text, "<br>Percent positive tests: ", format(Percent, digits = 3), 
                               "<br>Percent of population tested: ", format(Percent_of_pop, digits = 3)))
  

  colors <- c(
    "Tests" = "#B7F1A0",
    "Positive tests" = "#ED553B"
  )

  nationbarplot = function(name) {
    plot_ly(data %>% filter(location == name), x = ~date, y = ~Count, color = ~Type, text = ~Text,
            type = "bar", colors = colors, hoverinfo = "text", showlegend = FALSE
    ) %>%
      layout(barmode = "stack", xaxis = list(title = name, titlefont = axis.title.font, showticklabels = TRUE,
                                             tickangle = -30, zeroline = F), yaxis = yaxis, title =
               list(text = cap, xanchor = "left", x = 0), legend =
               list(orientation = "h", font = list(size = 16))
      )
      # ) %>%
      # plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  }

  plotList <- function(names) {
    lapply(names, nationbarplot)
  }

  names = c('United States', 'United Kingdom', 'Canada', 'South Korea', 'Turkey', 'India')

  p = subplot(plotList(names), nrows = 6, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  p
}

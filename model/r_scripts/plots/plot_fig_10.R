plot_fig_10 <- function(start.date = as.Date("2020-04-01"))
{
  
  coltype = cols(
    iso_code = col_character(),
    continent = col_character(),
    location = col_character(),
    date = col_date(),
    total_cases = col_double(),
    new_cases = col_double(),
    total_deaths = col_double(),
    new_deaths = col_double(),
    total_cases_per_million = col_double(),
    new_cases_per_million = col_double(),
    total_deaths_per_million = col_double(),
    new_deaths_per_million = col_double(),
    total_tests = col_double(),
    new_tests = col_double(),
    total_tests_per_thousand = col_double(),
    new_tests_per_thousand = col_double(),
    new_tests_smoothed = col_double(),
    new_tests_smoothed_per_thousand = col_double(),
    tests_units = col_character(),
    stringency_index = col_double(),
    population = col_double(),
    population_density = col_double(),
    median_age = col_double(),
    aged_65_older = col_double(),
    aged_70_older = col_double(),
    gdp_per_capita = col_double(),
    extreme_poverty = col_double(),
    cvd_death_rate = col_double(),
    diabetes_prevalence = col_double(),
    female_smokers = col_double(),
    male_smokers = col_double(),
    handwashing_facilities = col_double(),
    hospital_beds_per_thousand = col_double(),
    life_expectancy = col_double()
  )
  
  data <- read_csv(paste0(data_repo, today, "/global_testing.csv"), col_types = coltype) %>%
    select(location, date, total_cases, total_tests) %>%
    group_by(location) %>%
    filter(!is.na(total_cases) & !is.na(total_tests)) %>%
    filter(date == max(date)) %>%
    drop_na() %>%
    mutate(Percent = (total_cases / total_tests) * 100)
  
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
        Text = paste0("Total tests: ", format(Count, big.mark = ",", sci = F,
                         trim = T)),
        Type = 'Tests'),
    data %>%
      select(-total_tests) %>%
      mutate(
        Count = total_cases,
        Text = paste0("Total positive tests: ", format(Count, big.mark = ",",
                      sci = F, trim = T)),
        Type = "Positive tests")
    ) %>%
    mutate(Type = factor(Type, levels = c("Positive tests", "Tests"))) %>%
    select(-total_cases, - total_tests) %>%
    mutate(Text = paste0(Text, "<br>Percent positive tests: ",
                         format(Percent, digits = 3),
                         "<br>Percent of population tested: ", 
                         format(Percent_of_pop, digits = 3))
    )

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

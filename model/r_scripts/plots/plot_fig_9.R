
plot_fig_9 = function(start.date = as.Date('2020-04-01')) {
  
  data <- vroom(paste0(data_repo, today, "/global_testing.csv"))
  #today = '2020-04-17' # delete this later
  data = vroom('C:/Users/mkleinsa/Box/Projects/covid-india/IRIS5/cov-ind-19-data/today/global_testing.csv') %>%
    select(location, date, total_cases_per_million, total_tests_per_thousand) %>%
    group_by(location) %>%
    filter(!is.na(total_tests_per_thousand)) %>%
    filter(date == max(date)) %>%
    drop_na() %>% 
    mutate(Text = paste0('Location: ', location))
  
  cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"), sep = ' ')
  
  title <- paste("Testing pattern across countries")
  
  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)
  
  xaxis <- list(title = "Total cases per million", titlefont = axis.title.font, showticklabels = TRUE)
  
  yaxis <- list(title = "Total tests per thousand", titlefont = axis.title.font,
                tickfont = tickfont, zeroline = T)
  line_fmt = list(dash="solid", width = 1.5, color='black')
  
  p <- plot_ly(data, x = ~total_cases_per_million, y = ~total_tests_per_thousand, color = ~location, text = ~Text,
               hoverinfo = "text"
  ) %>%
    layout(xaxis = xaxis, yaxis = yaxis, title = list(text = cap, xanchor = "left", x = 0)
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  
  vroom_write(data, path = paste0(data_repo, today, "/plot9.csv"),
              delim = ","
  )
  
  #m1 = lm(total_tests_per_thousand ~ total_cases_per_million, data = data)
  #p = add_lines(p, x = 0:max(data$total_cases_per_million), y = predict(m1), line = line_fmt, name = "Linear trend")
  p
}

plot_fig_8 = function(start.date = as.Date('2020-04-01')) {
  
  data <- vroom(paste0(data_repo, today, "/testing.csv"))
  #today = '2020-04-17' # delete this later
  data = vroom('C:/Users/mkleinsa/Box/Projects/covid-india/IRIS5/cov-ind-19-data/today/testing.csv') %>%
    filter(Country == "India") %>%
    filter(Date >= start.date) %>%
    mutate(Date = as.factor(format(Date, format = "%b %e")),
           Cases = c(NA, diff(Cases)), #format(Cases, big.mark = ",", scientific = F, trim = T),
           Tests = c(NA, diff(Tests)), #format(Tests, big.mark = ",", scientific = F, trim = T),
           Text_cases = paste0(Date, ": ", Cases, ' cases'),
           Text_tests = paste0(Date, ": ", Tests, ' tests')) %>%
    drop_na()
  
  data = 
  bind_rows(data %>% select(-Tests, -Text_tests) %>% mutate(Count = Cases, Text = Text_cases, Type = 'Cases'),
            data %>% select(-Cases, -Text_cases) %>% mutate(Count = Tests, Text = Text_tests, Type = 'Tests')) %>%
    select(Date, Count, Text, Type, Country)
  
  cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"), sep = ' ')
  
  title <- paste("Daily confirmed positive cases and total tests in India")
  
  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)
  
  xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
                tickangle = -30, zeroline = F)
  
  yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                tickfont = tickfont, zeroline = T)
  colors <- c(
    "Cases" = "#ED553B",
    "Tests"  = "#f2c82e"
  )
  
  p <- plot_ly(data, x = ~Date, y = ~Count, color = ~Type, text = ~Text,
               type = "bar", colors = colors, hoverinfo = "text"
  ) %>%
    layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
             list(text = cap, xanchor = "left", x = 0), legend =
             list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  
  vroom_write(data, path = paste0(data_repo, today, "/plot8.csv"),
              delim = ","
  )
  return(p)
  
}
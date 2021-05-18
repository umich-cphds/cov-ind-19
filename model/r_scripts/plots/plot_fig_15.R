
suppressPackageStartupMessages({
  library(zoo)
})

plot_fig_15 <- function(state = 'India', start.date = "2020-07-01") {
  
  tsing = read.csv(paste0(data_repo, "/", today, "/statewise_tested_numbers_data.csv"), header = TRUE)
  
  everything = read.csv(paste0(data_repo, "/", today, "/everything.csv"))
  
  subtitle <- paste0('\uA9 COV-IND-19 Study Group. Last updated ',
                     format(as.Date(today), format = '%b %e'), ', 2020', sep = '')
  
  daily = function(x) { c(x[1], diff(x)) }
  
  caption <- 'Source: https://www.covid19india.org'
  
  statenames =
    matrix(data = c('Andhra Pradesh', 'AP',
                    'Arunachal Pradesh', 'AR',
                    'Assam', 'AS',
                    'Bihar', 'BR',
                    'Chhattisgarh', 'CG',
                    'Goa', 'GA',
                    'Gujarat', 'GJ',
                    'Haryana', 'HR',
                    'Himachal Pradesh', 'HP',
                    'Jammu and Kashmir', 'JK',
                    'Jharkhand', 'JH',
                    'Karnataka', 'KA',
                    'Kerala', 'KL',
                    'Madhya Pradesh', 'MP',
                    'Maharashtra', 'MH',
                    'Manipur', 'MN',
                    'Meghalaya', 'ML',
                    'Mizoram', 'MZ',
                    'Nagaland', 'NL',
                    'Odisha', 'OR',
                    'Punjab', 'PB',
                    'Rajasthan', 'RJ',
                    'Sikkim', 'SK',
                    'Tamil Nadu', 'TN',
                    'Tripura', 'TR',
                    'Uttarakhand', 'UK',
                    'Uttar Pradesh', 'UP',
                    'West Bengal', 'WB',
                    'Tamil Nadu', 'TN',
                    'Tripura', 'TR',
                    'Andaman and Nicobar Islands', 'AN',
                    'Chandigarh', 'CH',
                    'Dadra and Nagar Haveli', 'DH',
                    'Daman and Diu', 'DD',
                    'Delhi', 'DL',
                    'Lakshadweep', 'LD',
                    'Pondicherry', 'PY',
                    'Telangana', 'TG',
                    'Dadra and Nagar Haveli', 'DN',
                    'Chhattisgarh', 'CT',
                    'Ladakh', 'LA',
                    'Uttarakhand', 'UT'), ncol = 2, byrow = TRUE) %>%
    as.data.frame()
  colnames(statenames) <- c('full', 'abbrev')
  statenames$abbrev <- tolower(statenames$abbrev)
  
  tsing = left_join(tsing, statenames, by = c('State' = 'full'))
  
  p = NULL
  
  if(state == 'India') {
    
    everything = 
      everything %>%
      filter(abbrev == state) %>%
      mutate(Dates = as.Date(date)) %>%
      filter(Dates >= as.Date(start.date) & Dates <= as.Date(today)) %>%
      select(Dates, daily_cases, daily_tests, place) %>% 
      #group_by(Dates) %>%
      mutate(
        daily_tpr = daily_cases / daily_tests,
        text = paste('Date: ', format(Dates, format = '%b %d'), '\nDaily percent positive: ', 
            round(daily_tpr * 100, digits = 2), '%', sep = ''))
    
    everything$dailyTPR7d = c(rep(NA, 6), rollmean(everything$daily_tpr, k = 7))
    
    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)
    
    title <- "Proportion of positive COVID-19 tests in India"
    xaxis = list(title = 'Date', titlefont = axis.title.font, zeroline = F,
                 showline = F)
    yaxis = list(title = 'Proportion of positive tests', titlefont = axis.title.font, zeroline = F,
                 showline = F)
    
    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"), sep = ' ')
    
    p = plot_ly(everything, x = ~Dates, y = ~dailyTPR7d, text = ~text, type = "scatter",
                  hoverinfo = "text", mode = "markers+lines", hoverlabel = list(align = "left"),
                  showlegend = F, line = list(width = 3, color = '#36A30B'), marker = list(color = '#36A30B')
    ) %>%
      layout(xaxis = xaxis, yaxis = yaxis, title =
               list(text = cap, xanchor = "left", x = 0)
    )
    
  } else {
    
    everything = 
      everything %>%
      filter(abbrev == state) %>%
      mutate(Dates = as.Date(date)) %>%
      filter(Dates >= as.Date(start.date) & Dates <= as.Date(today)) %>%
      select(Dates, daily_cases, daily_tests, place) %>% 
      #group_by(Dates) %>%
      mutate(
        daily_tpr = daily_cases / daily_tests,
        text = paste('Date: ', format(Dates, format = '%b %d'), '\nDaily percent positive: ', 
                     round(daily_tpr * 100, digits = 2), '%', sep = ''))
    
    everything$dailyTPR7d = c(rep(NA, 6), rollmean(everything$daily_tpr, k = 7))
    
    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)
    
    title <- "Proportion of positive COVID-19 tests in India"
    xaxis = list(title = 'Date', titlefont = axis.title.font, zeroline = F,
                 showline = F)
    yaxis = list(title = 'Proportion of positive tests', titlefont = axis.title.font, zeroline = F,
                 showline = F)
    
    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                  format(today, format = "%b %e"), sep = ' ')
    
    p = plot_ly(everything, x = ~Dates, y = ~dailyTPR7d, text = ~text, type = "scatter",
                hoverinfo = "text", mode = "markers+lines", hoverlabel = list(align = "left"),
                showlegend = F, line = list(width = 3, color = '#36A30B'), marker = list(color = '#36A30B')
    ) %>%
      layout(xaxis = xaxis, yaxis = yaxis, title =
               list(text = cap, xanchor = "left", x = 0)
    )
    
  }

  p
}

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_15<- function(start.date = "2020-04-01") {
  
  tsing = read.csv(paste0(data_repo, today, "/statewise_tested_numbers_data.csv"), header = TRUE)
  
  subtitle <- paste0('© COV-IND-19 Study Group. Last updated ',
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
  
  tsing =
    tsing %>%
    mutate(Dates = as.Date(Updated.On, format = '%d/%m/%Y')) %>%
    filter(Dates >= as.Date(start.date)) %>%
    select(Dates, Positive, Total.Tested, State) %>% 
    mutate(Dates = format(Dates, format = '%b %d'),
           Dates = as.Date(Dates, format = '%b %d')) %>% 
    group_by(Dates) %>%
    summarise(total_positive = sum(Positive, na.rm = TRUE),
              total_tests = sum(Total.Tested, na.rm = TRUE)) %>%
    mutate(text = paste('Proportion tested: ', round(total_positive/total_tests, digits = 3), sep = ''))
  
  
  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)
  
  title <- "Proportion of positive COVID-19 tests in India"
  xaxis = list(title = 'Date', titlefont = axis.title.font, zeroline = F,
               showline = F)
  yaxis = list(title = 'Proportion of positive tests', titlefont = axis.title.font, zeroline = F,
               showline = F)
  
  cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"), sep = ' ')
  
  p15 = plot_ly(tsing, x = ~Dates, y = ~total_positive/total_tests, text = ~text,
          hoverinfo = "text", mode = "markers+lines", hoverlabel = list(align = "left"),
          showlegend = F, line = list(width = 3, color = '#36A30B'), marker = list(color = '#36A30B')
  ) %>%
  layout(xaxis = xaxis, yaxis = yaxis, title =
           list(text = cap, xanchor = "left", x = 0)
  )
  
  list(p15 = p15)
}

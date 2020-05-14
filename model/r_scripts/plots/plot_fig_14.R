# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_14 <- function(start.date = "2020-04-01") {
  
  tsing_by_state = read.csv(paste0(data_repo, today, "/statewise_tested_numbers_data.csv"), header = TRUE)
  
  subtitle <- paste0('© COV-IND-19 Study Group. Last updated ',
                     format(as.Date(today), format = '%b %e'), ', 2020', sep = '')
  
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
                    'Orissa', 'OR',
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
  
  tsing_by_state =
    tsing_by_state %>%
    mutate(Dates = as.Date(Updated.On, format = '%d/%m/%Y')) %>%
    filter(Dates >= as.Date(start.date)) %>%
    select(Dates, Positive, Total.Tested, State) %>% 
    mutate(Dates = format(Dates, format = '%b %d'))
  
  # by state positive and total tested facet plot
  
  ggplot(data = tsing_by_state, aes(x = Dates, y = Total.Tested, group = State)) +
    facet_wrap(~State) + geom_line(aes(color = 'Total tested'), size = 1.2) + 
    geom_line(aes(y = Positive, color = 'Positive tests'), size = 1.2) + 
    theme_bw() + 
    xlab('\nDate') + ylab('Cumulative number of daily cases\n') +
    theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
          strip.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 20)) +
    scale_y_continuous(sec.axis = sec_axis(~ .)) + 
    scale_color_manual(values = c('Total tested' = "#36A30B", 'Positive tests' = "#e63c30"),
                       name = 'Testing')
  
  
  
  ggplot(data = data_caseplot, aes(x = Date, y = Cases, group = full)) +
    geom_line(data = data_caseplot %>% ungroup() %>% select(-full), aes(x = Date, y = Cases, group = full2), color = 'gray', alpha = 0.6) +
    theme_bw() +
    geom_line(aes(x = Date, y = Cases, group = full), color = '#36A30B', size = 1.5) + facet_wrap(~full) +
    geom_point(shape = 3, size = 0.5) +
    xlab('\nDate') + ylab('Cumulative number of daily cases\n') +
    theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
          strip.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 20)) +
    scale_y_continuous(sec.axis = sec_axis(~ .)) +
    geom_point(data = distinct(data_caseplot %>% select(full, totalcases, State, Datemax)),
               aes(x = Datemax, y = totalcases, group = full), size = 2) +
    geom_text(data = distinct(data_caseplot %>% select(full, totalcases, State, Datemax)),
              aes(x = Datemax - 26, y = max(data_caseplot$totalcases) - 500, group = full, label = paste(totalcases, ' total cases', sep = ' '))) +
    labs(title = '',
         subtitle = subtitle,
         caption = caption)
  
  
  
  
  ggplot(data = fac_inc_data %>% filter(color == 'Moderate return'), aes(x = Dates, y = value, group = full)) +
    facet_wrap(~full, ncol = 2) + geom_line(aes(color = 'Moderate return'), size = 1.2) +
    geom_line(data = fac_inc_data %>% filter(color == 'Cautious return'), aes(color = 'Cautious return'), size = 1) +
    theme_bw() +
    xlab('\nDate') + ylab('Number of projected daily cases\n') +
    theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = 'black'),
          strip.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_text(size = 14), #, face = "bold"
          axis.title.y = element_text(size = 14), #, face = "bold"
          axis.text.x = element_text(angle = 20, size = 14),
          axis.text.y = element_text(size = 13),
          legend.position = 'bottom',
          legend.text = element_text(size=12)) +
    scale_x_date(sec.axis = ~ .) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NotFancy), labels = NotFancy) +
    # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    #               labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K")),
    #               sec.axis = sec_axis(~ .,
    #                   breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                   labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K"))
    # )) +
    labs(title = '',
         subtitle = subtitle,
         caption = caption) +
    scale_color_manual(values = c('Moderate return' = "#0472CF", 'Cautious return' = "#173F5F"),
                       name = 'Return strategy')
  
  
  
  
  
  
  
  
}

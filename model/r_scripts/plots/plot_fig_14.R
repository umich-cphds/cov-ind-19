plot_fig_14 <- function(start.date = "2020-04-01") {
  
  tsing_by_state = read.csv(paste0(data_repo, today, "/statewise_tested_numbers_data.csv"), header = TRUE)
  
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
  
  tsing_by_state =
    tsing_by_state %>%
    mutate(Dates = as.Date(Updated.On, format = '%d/%m/%Y')) %>%
    filter(Dates >= as.Date(start.date)) %>%
    select(Dates, Positive, Total.Tested, State) %>% 
    mutate(Dates = format(Dates, format = '%b %d'),
           Dates = as.Date(Dates, format = '%b %d'))
  
  # by state positive and total tested facet plot
  
  NotFancy <- function(l) {
    format(l, scientific = FALSE)
    #parse(text=l)
  }
  
  # top20test = 
  #   tsing_by_state %>% 
  #   group_by(State) %>% 
  #   summarise(maxtest = max(Total.Tested, na.rm = TRUE)) %>%
  #   arrange(desc(maxtest)) %>%
  #   top_n(20) %>%
  #   pull(State) %>% 
  #   as.character()
  
  data_caseplot =
    vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
    group_by(State) %>% #filter(Cases >= 50) %>%
    arrange(Date) %>%
    mutate(Day = seq(n()),
           dailyCases = daily(Cases),
           totalcases = max(Cases),
           Datemax = max(Date))
  
  data_caseplot =
    left_join(data_caseplot, statenames, by = c('State' = 'abbrev'))
  
  data_caseplot =
    data_caseplot %>%
    arrange(desc(totalcases))
  
  top20case = (data_caseplot %>% select(full) %>% unique() %>% pull(full))[1:20] # %>% top_n(20)
  
  tsing_by_state =
    tsing_by_state %>%
    filter(State %in% top20case) %>%
    drop_na()
  
  # tsing_by_state = 
  #   tsing_by_state %>% 
  #   filter(State %in% top20test) %>% 
  #   drop_na()
  
  p14 <- ggplot(data = tsing_by_state, aes(x = Dates, y = Positive/Total.Tested, group = State)) +
    facet_wrap(~State, ncol = 5) + geom_line(aes(color = 'Positive cases / Total tested'), size = 1.2) + 
    geom_point(shape = 3, size = 0.5) + 
    geom_point(data = tsing_by_state %>% group_by(State) %>% mutate(Datemax = max(Dates)) %>% filter(Dates == Datemax) %>% distinct(Datemax, .keep_all = TRUE),
               aes(x = Datemax, y = Positive/Total.Tested, group = State), size = 2) +
    geom_text(data = tsing_by_state %>% group_by(State) %>% mutate(Datemax = max(Dates)) %>% filter(Dates == Datemax) %>% distinct(Datemax, .keep_all = TRUE),
              aes(x = Datemax - 20, y = 0.14, group = State, label = paste(Total.Tested, ' total tested', sep = ' '))) +
    theme_bw() + 
    xlab('\nDate') + ylab('Positive cases / Total tested\n') +
    theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
          strip.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 20, size = 12),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = 'bottom',
          legend.text = element_text(size=12)) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NotFancy), labels = NotFancy) +  
    scale_color_manual(values = c('Positive cases / Total tested' = "#36A30B"),
                       name = 'Testing') + 
    scale_x_date(date_breaks = "2 week", date_labels = "%b %d") + 
    labs(title = '',
         subtitle = subtitle,
         caption = caption)
    
  list(p14 = p14)
}

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_12 <- function(start.date = "2020-06-01") {

  fac_inc_data = read.table(paste0(data_repo, today, "/incident_state_data.tsv"), sep = '\t', header = TRUE)
  fac_cumul_data = read.table(paste0(data_repo, today, "/cumulative_state_data.tsv"), sep = '\t', header = TRUE)

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

  fac_inc_data =
    fac_inc_data %>%
    mutate(Dates = as.Date(Dates)) %>%
    filter(Dates < as.Date('2020-09-16') & Dates >= as.Date(start.date),
           color == 'No Intervention') %>%
    left_join(statenames, by = c('State' = 'abbrev'))
  
  top20inc = 
    fac_inc_data %>% 
    filter(color == 'No Intervention') %>%
    group_by(full) %>% 
    summarise(maxproj = max(value)) %>%
    arrange(desc(maxproj)) %>%
    top_n(20) %>%
    pull(full) %>% 
    as.character()
  
  fac_inc_data = 
    fac_inc_data %>% 
    filter(full %in% top20inc)

  fac_cumul_data =
    fac_cumul_data %>%
    mutate(Dates = as.Date(Dates)) %>%
    filter(Dates < as.Date('2020-09-16') & Dates >= as.Date(start.date),
           color == 'No Intervention') %>%
    left_join(statenames, by = c('State' = 'abbrev'))
  
  top20cumul = 
    fac_cumul_data %>% 
    filter(color == 'No Intervention') %>%
    group_by(full) %>% 
    summarise(maxproj = max(value)) %>%
    arrange(desc(maxproj)) %>%
    top_n(20) %>%
    pull(full) %>% 
    as.character()
  
  fac_cumul_data = 
    fac_cumul_data %>% 
    filter(full %in% top20cumul)

  NotFancy <- function(l) {
    format(l, scientific = FALSE)
    #parse(text=l)
  }


  fmt1 <- function(x) paste(round_any(10 ^ x / 1000, 0.01) , "K", sep = "")
  
  p12a <- ggplot(data = fac_inc_data, aes(x = Dates, y = value, group = full)) +
    facet_wrap(~full, ncol = 4) + geom_line(color = '#36A30B', size = 1.8) +
    geom_point(shape = 3, size = 0.2) + 
    geom_point(data = fac_inc_data %>% group_by(full) %>% mutate(Datemax = max(Dates)) %>% filter(Dates == Datemax) %>% distinct(Datemax, .keep_all = TRUE),
               aes(x = Datemax, y = value, group = State), size = 2) +
    #geom_line(data = fac_inc_data %>% filter(color == 'Cautious return'), aes(color = 'Cautious return'), size = 1) +
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
    #scale_x_date(sec.axis = ~ .) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NotFancy), labels = NotFancy) +
    # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    #               labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K")),
    #               sec.axis = sec_axis(~ .,
    #                   breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                   labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K"))
    # )) +
    labs(title = '',
         subtitle = subtitle,
         caption = caption)# +
    #scale_color_manual(values = c('Moderate return' = "#0472CF", 'Cautious return' = "#173F5F"),
    #                   name = 'Return strategy')

  p12b <- ggplot(data = fac_cumul_data, aes(x = Dates, y = value, group = full)) +
    facet_wrap(~full, ncol = 4) + geom_line(color = '#36A30B', size = 1.8) +
    geom_point(shape = 3, size = 0.2) + 
    geom_point(data = fac_cumul_data %>% group_by(full) %>% mutate(Datemax = max(Dates)) %>% filter(Dates == Datemax) %>% distinct(Datemax, .keep_all = TRUE),
               aes(x = Datemax, y = value, group = State), size = 2) +
    #geom_line(data = fac_inc_data %>% filter(color == 'Cautious return'), aes(color = 'Cautious return'), size = 1) +
    theme_bw() +
    xlab('\nDate') + ylab('Cumulative number of projected daily cases\n') +
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
    #scale_x_date(sec.axis = ~ .) +
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NotFancy), labels = NotFancy) +
    # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    #               labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K")),
    #               sec.axis = sec_axis(~ .,
    #                   breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                   labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K"))
    # )) +
    labs(title = '',
         subtitle = subtitle,
         caption = caption)# +

  # p12b <- ggplot(data = fac_cumul_data %>% filter(color == 'Moderate return'), aes(x = Dates, y = value, group = full)) +
  #   facet_wrap(~full, ncol = 4) + geom_line(aes(color = "Moderate return"), size = 1.2) +
  #   geom_line(data = fac_cumul_data %>% filter(color == 'Cautious return'), aes(color = "Cautious return"), size = 1) +
  #   theme_bw() +
  #   xlab('\nDate') + ylab('Cumulative number of projected daily cases\n') +
  #   theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = 'black'),
  #         strip.background = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.border = element_blank(),
  #         axis.title.x = element_text(size = 14), #, face = "bold"
  #         axis.title.y = element_text(size = 14), #, face = "bold"
  #         axis.text.x = element_text(angle = 20, size = 14),
  #         axis.text.y = element_text(size = 13),
  #         legend.position = 'bottom',
  #         legend.text = element_text(size=12)) +
  #   scale_x_date(sec.axis = dup_axis()) +
  #   scale_y_continuous(sec.axis = sec_axis(~ ., labels = NotFancy), labels = NotFancy) +
  #   # scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   #               labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K")),
  #   #               sec.axis = sec_axis(~ .,
  #   #                   breaks = scales::trans_breaks("log10", function(x) 10^x),
  #   #                   labels = scales::trans_format("log10", function(x) paste0(format(10 ^ x /1000, sci = F), "K"))
  #   # )) +
  #   labs(title = '',
  #        subtitle = subtitle,
  #        caption = caption) +
  #   scale_color_manual(values = c('Moderate return' = "#0472CF", 'Cautious return' = "#173F5F"),
  #                      name = 'Return strategy')

  list(p12a = p12a, p12b = p12b)

}

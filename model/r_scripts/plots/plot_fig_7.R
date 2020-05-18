
# State-level static "rolling average" of deaths and cases per day

library(tidyverse)
library(vroom)
library(ggplot2)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

plot_fig_7 <- function()
{
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

    # subtitle
    subtitle <- paste0('© COV-IND-19 Study Group. Last updated ',
                  format(as.Date(today), format = '%b %e'), ', 2020', sep = '')
    caption <- 'Source: https://www.covid19india.org'

    # Compute daily counts from cumulative sums
    daily = function(x) { c(x[1], diff(x)) }

    #start.date = as.Date("2020-03-01")
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

    data_deathplot =
      vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
      group_by(State) %>% #filter(Deaths >= 1) %>%
      arrange(Date) %>%
      mutate(Day = seq(n()),
             dailyDeaths = daily(Deaths),
             totalDeaths = max(Deaths),
             Datemax = max(Date))

    data_deathplot =
      left_join(data_deathplot, statenames, by = c('State' = 'abbrev'))

    # sort data sets from max to min max cases, plot the first 20
    data_caseplot =
      data_caseplot %>%
      arrange(desc(totalcases))

    top20case = (data_caseplot %>% select(State) %>% unique() %>% pull(State))[1:20] # %>% top_n(20)

    data_caseplot =
      data_caseplot %>%
      filter(State %in% top20case)


    data_deathplot =
      data_deathplot %>%
      arrange(desc(totalDeaths))

    top20caseDeath = (data_deathplot %>% select(State) %>% unique() %>% pull(State))[1:20]

    data_deathplot =
      data_deathplot %>%
      filter(State %in% top20caseDeath)


    # rolling average plots

    # incidence
    data_caseplot =
      data_caseplot %>%
      mutate(full2 = full)
    p7a<-ggplot(data = data_caseplot, aes(x = Date, y = dailyCases, group = full)) +
      geom_smooth(data = data_caseplot %>% ungroup() %>% select(-full), aes(x = Date, y = dailyCases, group = full2), color = 'gray', span = 0.7, se = FALSE, alpha = 0.6, size = 1) +
      theme_bw() +
      geom_smooth(aes(x = Date, y = dailyCases, group = full), span = 0.7, se = FALSE, color = '#36A30B', size = 1.25) + facet_wrap(~full) +
      geom_point(size = 1) +
      xlab('\nDate') + ylab('Number of daily cases\n') +
      theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
            strip.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 20)) +
      scale_y_continuous(sec.axis = sec_axis(~ .)) +
      #annotate("text", x = 1, y = 200, label = totalcases)
      geom_text(data = distinct(data_caseplot %>% select(full, totalcases, State, Datemax)),
                aes(x = Datemax - 18, y = 200, group = full, label = paste(totalcases, ' total cases', sep = ' '))) +
      labs(title = '',
           subtitle = subtitle,
           caption = caption)

    # culumative cases
    p7b<-ggplot(data = data_caseplot, aes(x = Date, y = Cases, group = full)) +
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


    # rolling average plots (of deaths)

    # incidence
    data_deathplot =
      data_deathplot %>%
      mutate(full2 = full)
    p7c <- ggplot(data = data_deathplot, aes(x = Date, y = dailyDeaths, group = full)) +
      geom_smooth(data = data_deathplot %>% ungroup() %>% select(-full), aes(x = Date, y = dailyDeaths, group = full2), color = 'gray', span = 0.7, se = FALSE, alpha = 0.6, size = 1) +
      theme_bw() +
      geom_smooth(aes(x = Date, y = dailyDeaths, group = full), span = 0.7, se = FALSE, color = '#36A30B', size = 1.25) + facet_wrap(~full) +
      geom_point(size = 1) +
      xlab('\nDate') + ylab('Number of daily deaths\n') +
      theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
            strip.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 20)) +
      scale_y_continuous(sec.axis = sec_axis(~ .)) +
      #annotate("text", x = 1, y = 200, label = totalcases)
      geom_text(data = distinct(data_deathplot %>% select(full, totalDeaths, State, Datemax)),
                aes(x = Datemax - 22, y = 23, group = full, label = paste(totalDeaths, ' total deaths', sep = ' '))) +
      labs(title = '',
           subtitle = subtitle,
           caption = caption)

    # culumative
    p7d <- ggplot(data = data_deathplot, aes(x = Date, y = Deaths, group = full)) +
      geom_line(data = data_deathplot %>% ungroup() %>% select(-full), aes(x = Date, y = Deaths, group = full2), color = 'gray', alpha = 0.6) +
      theme_bw() +
      geom_line(aes(x = Date, y = Deaths, group = full), color = '#36A30B', size = 1.5) + facet_wrap(~full) +
      geom_point(shape = 3, size = 0.5) +
      xlab('\nDate') + ylab('Cumulative number of daily deaths\n') +
      theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
            strip.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 14), #, face = "bold"
            axis.title.y = element_text(size = 14), #, face = "bold"
            axis.text.x = element_text(angle = 20)) +
      scale_y_continuous(sec.axis = sec_axis(~ .)) +
      geom_point(data = distinct(data_deathplot %>% select(full, totalDeaths, State, Datemax)),
                 aes(x = Datemax, y = totalDeaths, group = full), size = 2) +
      geom_text(data = distinct(data_deathplot %>% select(full, totalDeaths, State, Datemax)),
                aes(x = Datemax - 26, y = max(data_deathplot$totalDeaths) - 50, group = full, label = paste(totalDeaths, ' total deaths', sep = ' '))) +
      labs(title = '',
           subtitle = subtitle,
           caption = caption)

    list(p7a = p7a, p7b = p7b, p7c = p7c, p7d = p7d)
}

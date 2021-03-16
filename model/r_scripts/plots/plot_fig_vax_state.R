
plot_fig_vax_state = function() {
  
  vax_dat <- suppressMessages(vroom("http://api.covid19india.org/csv/latest/vaccine_doses_statewise.csv")) %>%
    pivot_longer(
      names_to = "date",
      values_to = "vaccines",
      -State
    ) %>%
    mutate(
      date = as.Date(date, format = "%d/%m/%Y")
    ) %>%
    dplyr::rename(
      state = State
    ) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(
      daily_vaccines = vaccines - dplyr::lag(vaccines)
    ) %>%
    ungroup()
  
  top_20 <- vax_dat %>%
    filter(!state %in% c("Total", "Miscellaneous")) %>%
    group_by(state) %>%
    filter(vaccines == max(vaccines)) %>%
    ungroup() %>%
    arrange(desc(vaccines)) %>%
    slice(1:20) %>%
    pull(state)
  
  # subtitle
  subtitle <- paste0('\uA9 COV-IND-19 Study Group. Last updated ',
                     format(as.Date(today), format = '%b %e'), ', 2020', sep = '')
  caption <- 'Source: https://www.covid19india.org'
  
  vax_dat = vax_dat %>% dplyr::filter(state %in% top_20, state != "Total")
  
  pvax = vax_dat %>%
    drop_na() %>%
    ggplot(aes(x = date, y = vaccines, group = state)) +
    facet_wrap(~state) +
    geom_line(color = "#138808", size = 1.2) +
    geom_point(data = vax_dat %>% filter(date == max(date))) + 
    scale_y_continuous(labels = comma) +
    labs(
      title = "Cumulative COVID-19 vaccines delivered by state",
      x     = "Date",
      y     = "Number of vaccines",
      caption = "**Source:** covid19india.org<br>**\uA9 COVIND-19 Study Group**"
    ) +
    theme_bw() +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_markdown(hjust = 0),
      strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
      strip.background = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_text(size = 14), #, face = "bold"
      axis.title.y = element_text(size = 14), #, face = "bold"
      axis.text.x = element_text(angle = 20)
    )
  
  return(pvax)
}

suppressPackageStartupMessages({
library(janitor)
library(EpiEstim)
})

plot_fig_tvr <- function(forecast)
{
  tickfont        <- list(size = 16)
  set_seed <- 46342
  set.seed(set_seed)
  start_date = NULL
  
  # data ----------
  if (forecast == "India") {
    start_date = as.Date('03/15/2020', format = "%m/%d/%y")
    data = get_nat_counts() %>%
      mutate(cases = daily_cases,
                recovered = daily_recovered,
                deaths = daily_deaths) %>% 
      select(place, date, cases, recovered, deaths) %>% 
      mutate(date = as.Date(date, "%m/%d/%y"))
    
    data <- read_tsv(paste0(data_repo, "/", today, "/jhu_data_mod.csv"), col_types = cols()) %>%
      clean_names() %>%
      filter(country == "India") %>%
      mutate(date = as.Date(date, "%m/%d/%y")) %>%
      #filter(date >= start_date) %>%
      arrange(cases) %>%
      mutate(
        cases = cases - dplyr::lag(cases)
      ) %>% 
      drop_na(cases)
  } else {
    start_date = as.Date('03/24/2020', format = "%m/%d/%y")
    data = get_state_counts() %>%
      mutate(cases = daily_cases,
             recovered = daily_recovered,
             deaths = daily_deaths) %>% 
      select(place, date, cases, recovered, deaths) %>% 
      mutate(date = as.Date(date, "%m/%d/%y"))
    
  statenames = vroom(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
      group_by(State) %>%
      filter(Date == max(Date)) %>%
      ungroup() %>% 
    select(State, Name)
  
  data = 
    data %>% 
    left_join(statenames, by = c("place" = "Name"))
  
  data = 
    data %>% 
    mutate(state = State) %>% 
    filter(state == forecast) %>%
    filter(cases > 0)
    
  # data <- read_tsv(paste0(data_repo, "/", today, "/covid19india_data.csv"), col_types = cols()) %>%
  #   clean_names() %>%
  #   filter(state == forecast) %>%
  #   filter(cases != 0) %>% #& date >= start_date
  #   group_by(name) %>%
  #   arrange(date) %>%
  #   mutate(cases = cases - dplyr::lag(cases)) %>%
  #   drop_na(cases) %>%
  #   filter(cases > 0) %>%
  #   ungroup()
  # }
  
  t_start <- seq(2, nrow(data) - 4)
  t_end   <- t_start + 4
  
  res <- estimate_R(
    incid = data$cases,
    method = "parametric_si",
    config = make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = set_seed))
  ) 
  
  plt_data <- tibble(
    date_num = res$dates
  ) %>% left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    add_column(date = data$date) %>%
    mutate(
      text = paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                    format(round(r, 2), nsmall = 2), "<br>CI: ",
                    paste0("[", format(round(lower, 2), nsmall = 2), ", ",
                           format(round(upper, 2), nsmall = 2), "]"))
    ) %>% 
    filter(date >= start_date)
  
  if (forecast == "India") {
    plt_data <- plt_data %>% filter(date >= "2020-03-15")
  } else {
    plt_data <- plt_data %>% filter(date >= "2020-03-24")
  }
  
  cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"), sep = ' ')
  axis_title_font <- list(size = 16)
  
  
  p <- plot_ly(plt_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
          line = list(color = "rgb(54, 163, 11)", width = 5),
          hoverinfo = "text",
          text   = ~text) %>%
    add_markers(data = plt_data, x = ~date, y = ~r, mode = "marker", 
                marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%
    add_ribbons(ymin = ~lower,
                ymax = ~upper,
                line = list(color = 'rgba(54, 163, 11, 0.05)'),
                fillcolor = 'rgba(54, 163, 11, 0.2)',
                hoverinfo = "none") %>%
    layout(
      title = list(text = cap, xanchor = "left", x = 0),
      xaxis = list(title = "Date", titlefont = axis_title_font,
                   tickfont = tickfont, zeroline = T),
      yaxis = list(title = "R(t)", titlefont = axis_title_font,
                   tickfont = tickfont, zeroline = T),
      shapes = list(
        type = "line", xref = "paper", yref = "data",
        x0 = 0, x1 = 1, y0 = 1, y1 = 1, 
        line = list(color = "rgba(255, 153, 51, 0.5)")
      ),
      showlegend = FALSE
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  
  #write_csv(plt_data, path = paste0(data_repo, today, "/", forecast, "_tvr_plot.csv"))

  p
  
}

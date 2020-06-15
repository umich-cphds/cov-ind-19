library(janitor)

# calculate dbl_time ----------
dbl_timr <- function(data, end_date, time) {
  
  start <-  data %>% filter(date == as.Date(as.Date(end_date) - time)) %>% pull(cases)
  
  if (length(start) == 0) {
    NA
  } else if (start == 0) {
    NA
  } else {
    end   <- data %>% filter(date == as.Date(end_date)) %>% pull(cases)
    
    r <- ((end - start) / start) * 100
    
    time * (log(2) / log(1 + (r / 100)))
  }
}

# generate plot
plot_fig_dbl <- function(forecast)
{
  tickfont   <- list(size = 16)
  start_date <- NULL
  
  # data ----------
  if (forecast == "India") {
    start_date <- as.Date("03/15/2020", format = "%m/%d/%y")
    data <- read_tsv(paste0(data_repo, today, "/jhu_data_mod.csv"), col_types = cols()) %>%
      clean_names() %>%
      filter(country == "India") %>%
      mutate(date = as.Date(date, "%m/%d/%y")) %>%
      arrange(date)
  } else {
    start_date <- as.Date("03/24/2020", format = "%m/%d/%y")
    data <- read_tsv(paste0(data_repo, today, "/covid19india_data.csv"), col_types = cols()) %>%
      clean_names() %>%
      filter(state == forecast) %>%
      filter(cases != 0) %>%
      arrange(date)
  }

  data$dbl <- NA
  for (i in seq_along(data$date)) {
    data$dbl[i] <- dbl_timr(data = data, end_date = data$date[i], time = 7)
  }
  
  data <- data %>%
    mutate(
      text = paste0("Date: ", format(date, format = "%b %e"), "<br>Doubling time: ",
                    format(round(dbl, 2), nsmall = 2), " days")
    ) %>%
    filter(date >= start_date)

  
  if (forecast == "India") {
    plt_data <- data %>% drop_na(dbl) %>% filter(date >= "2020-03-15") %>% 
  } else {
    plt_data <- data %>% drop_na(dbl) %>% filter(date >= "2020-03-24")
  }
  
  cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"), sep = ' ')
  axis_title_font <- list(size = 16)
  
  
  p <- plot_ly(plt_data, x = ~date, y = ~dbl, type = "scatter", mode = "lines",
               line = list(color = "rgb(54, 163, 11)", width = 5),
               hoverinfo = "text",
               text   = ~text) %>%
    add_markers(data = plt_data, x = ~date, y = ~dbl, mode = "marker", 
                marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%

    layout(
      title = list(text = cap, xanchor = "left", x = 0),
      xaxis = list(title = "Date", titlefont = axis_title_font,
                   tickfont = tickfont, zeroline = T),
      yaxis = list(title = "Doubling time (days)", titlefont = axis_title_font,
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

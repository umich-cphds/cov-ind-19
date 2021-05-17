suppressPackageStartupMessages({
  library(tidyverse)
  library(vroom)
  library(gt)
  library(glue)
  library(plotly)
})

plot_fig_SEIR = function(state = "India") {
  
  #today = Sys.getenv("today")
  #repo_seir = paste0("https://raw.githubusercontent.com/umich-cphds/covind_seirfansy_data/main/", today)
  repo_seir = paste0("~/covind_seirfansy_data/", today)
  
  # abrev <- vroom(paste0(data_repo, today, "/covid19india_data.csv"), col_types = cols()) %>%
  #   group_by(State) %>%
  #   filter(Date == max(Date) & State != "un") %>%
  #   ungroup() %>%
  #   top_n(20, Cases)
  # abrev = abrev$State
  
  if (state == "India") {
    tmp <- read_tsv(paste0(repo_seir, "/prediction_", "tt", ".txt"), col_types = cols()) %>%
      select(
        state, date, section, pred, value = mean
      ) %>%
      pivot_wider(
        names_from = "section",
        values_from = "value",
        id_cols = c("date", "pred", "state")
      ) %>%
      rename(
        case_daily_reported = positive_daily_reported
      ) %>%
      mutate(
        death_daily_unreported = death_unreported - lag(death_unreported),
        case_daily_unreported = unreported_daily 
      ) %>%
      select(
        state, date, pred, case_daily_reported, death_daily_reported
      ) %>%
      filter(pred == 1) %>% 
      filter(date <= min(date) + 30) %>% 
      pivot_longer(cols = c(case_daily_reported, death_daily_reported))
  } else {
    tmp <- read_tsv(paste0(repo_seir, "/prediction_", state, ".txt"), col_types = cols()) %>%
        select(
          state, date, section, pred, value = mean
        ) %>%
        pivot_wider(
          names_from = "section",
          values_from = "value",
          id_cols = c("date", "pred", "state")
        ) %>%
        rename(
          case_daily_reported = positive_daily_reported
        ) %>%
        mutate(
          death_daily_unreported = death_unreported - lag(death_unreported),
          case_daily_unreported = unreported_daily
        ) %>%
        select(
          state, date, pred, case_daily_reported, death_daily_reported
        ) %>%
        filter(pred == 1) %>%
        filter(date <= min(date) + 30) %>%
        pivot_longer(cols = c(case_daily_reported, death_daily_reported))
  }
  
  tmp = 
    tmp %>% 
    mutate(text = paste0(format(round(value, digits = 0), big.mark=","), " daily ", 
                         str_extract(name, "[^_]+"), "s"))
  
  
  s = state
  p.title = paste0("Projected daily number of new cases and deaths\nin ", s)
  
  cases.xaxis = "Date"
  cases.yaxis = "Daily counts"
  
  cases = plot_ly(tmp %>% filter(name == "case_daily_reported"), 
                  x = ~ date, y = ~ value, text = ~ text,
                  hoverinfo = "text", type = "bar",
                  hoverlabel = list(align = "left"), showlegend = F, 
                  marker = list(color = "#FF9933")) %>%
    layout(annotations = list(text = "SEIR daily new cases", xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22)),
           xaxis = list(title = "Date"), yaxis = list(title = "Daily counts"))
  
  deaths = plot_ly(tmp %>% filter(name == "death_daily_reported"), 
                   x = ~ date, y = ~ value, text = ~ text,
                   hoverinfo = "text", type = "bar",
                   hoverlabel = list(align = "left"), showlegend = F, 
                   marker = list(color = "#138808")) %>%
    layout(annotations = list(text = "SEIR daily new deaths", xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22)),
           xaxis = list(title = "Date"), yaxis = list(title = "Daily counts"))
  
  subplot(cases, deaths, titleX = T, titleY = T, margin = .08,
          nrows = 2, shareX = F) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))
  
}


# libraries ----------
library("tidyverse")
library("glue")
library("janitor")
library("lubridate")
library("gt")
library("vroom")

snapshot = function() {
  # functions -----------
  get_snap <- function(t = NULL) {
    nat <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                    col_types = cols()) %>%
      clean_names() %>%
      select(-date) %>% 
      rename(date = date_ymd)
      # mutate(
      #   date = as.Date(date, "%d %b")
      # )
    
    icmr <- read_csv("https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv",
                     col_types = cols()) %>%
      clean_names() %>%
      mutate(
        date = as.Date(update_time_stamp, "%d/%m/%Y") - 1
      ) %>%
      select(date, total_samples_tested, sample_reported_today)
    
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
      ungroup() %>% 
      filter(state == "Total")#%>% 
      #mutate(state = ifelse(state == "Total", "National estimate", state))
    
    if (!is.null(t)) {
      try(if (!is.Date(t)) stop("t needs to be a date (YYYY-MM-DD)"))
      today <- as.Date(t)
    } else {
      today     <- min(max(nat$date, na.rm = TRUE), max(icmr$date, na.rm = TRUE), 
                       max(vax_dat$date, na.rm = TRUE))
    }
    
    yesterday <- today - 1
    week_ago  <- today - 7
    month_ago <- today - 30
    
    get_stats <- function(d) {
      
      tmp_nat <- nat %>% filter(date == d)
      
      tmp_deaths <- tmp_nat %>% pull(daily_deceased)
      tmp_cases  <- tmp_nat %>% pull(daily_confirmed)
      tmp_tests  <- icmr %>% filter(date == d) %>% pull(sample_reported_today)
      
      tmp_vax = vax_dat %>% filter(date == d) %>% pull(daily_vaccines)
      
      tibble(
        "Day"      = ifelse(d == today, "Today",
                       ifelse(d == yesterday, "Yesterday",
                         ifelse(d == week_ago, "One week ago", 
                           ifelse(d == month_ago, "One month ago", NA)))),
        "Date"     = format(d, "%m/%d"),
        "Deaths"   = format(tmp_deaths, big.mark = ","),
        "Cases"    = format(tmp_cases, big.mark = ","),
        "Tests"    = format(tmp_tests, big.mark = ","),
        "TPR"      = paste0( round((tmp_cases / tmp_tests) * 100, 1), "%"),
        "Vaccines" = format(tmp_vax, big.mark = ",")
      )
      
    }
    
    today_stats     <- get_stats(today)
    yesterday_stats <- get_stats(yesterday)
    week_ago_stats  <- get_stats(week_ago)
    month_ago_stats <- get_stats(month_ago)
    
    bind_rows(
      today_stats,
      yesterday_stats,
      week_ago_stats,
      month_ago_stats
    )
    
  }
  
  make_pretty <- function(x) {
    x %>%
      gt() %>%
      # bold column headers
      tab_style(
        style = cell_text(
          font = "arial",
          weight = "bold"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # center column headers
      tab_style(
        style = cell_text(
          align = "center"
        ),
        locations = cells_column_labels(vars(Date, Deaths, Cases, Tests, TPR))
      ) %>%
      # format columns
      tab_style(
        style = list(
          cell_text(
            font = "arial",
            align = "center"
          )
        ),
        locations = list(
          cells_body(columns = vars(Date, Deaths, Cases, Tests, TPR))
        )
      )
  }
  
  # run ----------
  snap <- get_snap()
  snap
  
  today <- snap %>% filter(Day == "Today") %>% pull(Date) %>% as.Date(., "%m/%d")
  
  return(snap %>%
           make_pretty())
}

snapshot()

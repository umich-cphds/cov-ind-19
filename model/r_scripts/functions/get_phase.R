get_phase <- function(start_date = min_date, end_date = max_date) {
  
  tmp_dates <- seq.Date(from = as.Date(start_date),
                        to   = as.Date(end_date),
                        by    = "day")
  
  tmp_phases <- c(1, 
    tmp_dates %>% lubridate::day() %>% which(x = . == 1)) %>%
    unique()
  
  return(tmp_phases)
  
}


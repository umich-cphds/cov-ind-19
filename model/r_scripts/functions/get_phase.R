get_phase <- function(start_date = min_date, end_date = max_date, phase_length = 15) {
  
  tmp_dates <- seq.Date(from = as.Date(start_date),
                        to   = as.Date(end_date),
                        by    = "day")
  
  tmp_phases <- c(1,
    tmp_dates %>% which(x = . %in% rev(rev(tmp_dates)[seq(1, length(tmp_dates), phase_length)]))) %>%
    unique()
  
  tmp_phases <- head(tmp_phases[!tmp_phases %in% 2:phase_length], -1)
  
  return(tmp_phases)
  
}

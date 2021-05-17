get_phase <- function(phases = NULL, start_date = NULL) {
  
  if (is.null(phases)) {
    
    message("using default phases")
    phases <- c(1, 15, 34, 48, 62, 92, 123, 154, 184, 215, 245, 276, 307, 335)
    
  }
  
  if (is.null(start_date)) {
    
    message("using default start date of April 1 2020")
    start_date <- as.Date("2020-04-01") - 1
    
  }
  
  tmp_phase_tib <- tibble(
    phase_date_num = phases,
    phase_date     = as.Date(start_date) - 1 + phases
  )
  
  start_date <- as.Date(start_date)
  
  tmp_phase_tib %>%
    filter(phase_date >= start_date) %>%
    add_row(
      phase_date         = start_date
    ) %>%
    arrange(phase_date) %>%
    distinct(phase_date) %>%
    mutate(
      phase_date_num = as.numeric(phase_date) - as.numeric(start_date) + 1
    )
  
  as.numeric(as.vector(tmp_phase_tib$phase_date_num))
  
}


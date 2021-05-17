get_init <- function(data) {
  
  tmp_data <- data %>%
    filter(date < min_date) %>%
    dplyr::select(-date) %>%
    summarize(
      Confirmed = sum(Confirmed),
      Recovered = sum(Recovered),
      Deceased  = sum(Deceased)
    ) %>%
    as.numeric(as.vector(.))
  
  tmp <- data %>% filter(date >= min_date & date <= max_date)
  
  data_initial <- c(tmp_data,
                    tmp %>%
                      filter(date == min_date) %>%
                      dplyr::select(-date) %>%
                      as.numeric(as.vector(.))
  )
  if (data_initial[1] == 0) {data_initial[1] <- 1}
  if (data_initial[4] == 0) {data_initial[4] <- 1} # check with Ritwik/Ritoban if this is necessary
  
  return(data_initial)
  
}

library(tidyverse)
library(zoo)

# cleanr_esir: clean tvt.eSIR output ----------
cleanr_esir <- function(f_out = NULL, name = NULL, adj = T, adj_len = 2, out_obs = FALSE, obs_dat = NULL, N = NULL) {
  
  if (is.null(N)) {
    
    return("You need to specify the 'N' argument (population size)...")
    
  } else {
    
    things <- list(
      T_prime          = f_out$plot_infection$plot_env$T_prime,
      T_fin            = f_out$plot_infection$plot_env$T_fin,
      chron_ls         = f_out$plot_infection$plot_env$chron_ls,
      data_comp        = f_out$plot_infection$plot_env$data_comp,
      data_comp_R      = f_out$plot_infection$plot_env$data_comp_R
    )
    
    confirm <- round(N * (((things$data_comp[(things$T_prime + 1):(things$T_prime + things$T_fin), "mean"])) +
      (things$data_comp_R[(things$T_prime + 1):(things$T_prime + things$T_fin), "mean"])))
    
    confirm_up <- round(N * (((things$data_comp[(things$T_prime + 1):(things$T_prime + things$T_fin), "upper"])) +
                            (things$data_comp_R[(things$T_prime + 1):(things$T_prime + things$T_fin), "upper"])))
    
    confirm_low <- round(N * (((things$data_comp[(things$T_prime + 1):(things$T_prime + things$T_fin), "lower"])) +
                               (things$data_comp_R[(things$T_prime + 1):(things$T_prime + things$T_fin), "lower"])))
    
    if (adj == TRUE) {
    adj_v <- mean(
      as.vector(NI_complete[(things$T_prime - adj_len):things$T_prime]) / 
        N / 
        (things$data_comp[(things$T_prime - adj_len):things$T_prime, "mean"] +
           things$data_comp_R[(things$T_prime - adj_len):things$T_prime, "mean"])
    )
    
    confirm     <- round(confirm * adj_v)
    confirm_up  <- round(confirm_up * adj_v)
    confirm_low <- round(confirm_low * adj_v)
    }
    
    tib <- tibble(
      date        = as.Date(things$chron_ls, "%m/%d/%y") %>% tail(-things$T_prime) %>% head(-1),
      value       = na.trim(confirm),
      upper_ci    = na.trim(confirm_up),
      lower_ci    = na.trim(confirm_low)
    ) %>%
      mutate(
        incidence = value - dplyr::lag(value)
      )
    
    if (!is.null(name)) {
      tib <- tib %>% add_column(scenario = name)
    }
    
    if (out_obs == TRUE) {
      if (is.null(obs_dat)) {
        return("If you want to output observed data, you need to specify 'obs_dat' with columns 'Date' and 'Cases'...")
      } else {
      
        obs_dat <- obs_dat %>% dplyr::select(date = Date, value = Cases)
        
        if (!is.null(name)) {
          obs_dat <- obs_dat %>% add_column(scenario = "Observed")
        }  
        tib <- obs_dat %>% add_column(forecast = 0) %>% bind_rows(tib %>% add_column(forecast = 1))
      }
    }
    
    data <- tib %>% dplyr::select(date, value, lower_ci, upper_ci, incidence, everything())
    
    posterior <- tibble(
      stat  = c("r0", "beta", "gamma"),
      mean  = c(f_out$out_table$R0_p_mean, f_out$out_table$beta_p_mean, f_out$out_table$gamma_p_mean),
      lower = c(f_out$out_table$R0_p_ci_low, f_out$out_table$beta_p_ci_low, f_out$out_table$gamma_p_ci_low),
      upper = c(f_out$out_table$R0_p_ci_up, f_out$out_table$beta_p_ci_up, f_out$out_table$gamma_p_ci_up)
    )
    
    out_tib <- f_out$out_table %>% t() %>% as_tibble(rownames = "stat") %>% dplyr::rename(value = V1)
    
    list(
      data      = data,
      posterior = posterior,
      out_tib   = out_tib
    )

  }
  
}

# elefante: truncate pi schedules ----------
elefante <- function(dates, pis, anchor = Sys.Date()) {
  
  if (max(as.Date(dates, "%m/%d/%Y")) > anchor) {
    drpr      <- length(dates[dates <= format(anchor, "%m/%d/%Y")]) + 1
    tmp_dates <- c(format(anchor, "%m/%d/%Y"), dates[drpr:length(dates)])
    tmp_pis   <- c(1, pis[drpr:length(pis)])
  }
  
  if (max(as.Date(dates, "%m/%d/%Y")) <= anchor) {
    tmp_dates <- format(anchor, "%m/%d/%Y")
    tmp_pis   <- c(1, tail(pis, 1))
  }
  
  return(
    list(
      dates = tmp_dates,
      pis   = tmp_pis,
      check = ifelse(length(tmp_dates) + 1 == length(tmp_pis), "All good!", "Uh-oh...")
    )
  )
  
}

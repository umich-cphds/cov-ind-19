# tidy eSIR data ----------
pull_eSIR_data <- function(
  dat,
  name          = NULL,
  adj           = TRUE,
  adj_len       = 2,
  start_date    = "2020-03-01",
  last_obs_date = latest_date,
  f_length      = 200,
  N             = 1.34e9) {
  
  load(dat)
  
  other_plot       <- plot_data_ls[[2]]
  T_fin            <- other_plot[[2]]
  T_prime          <- other_plot[[1]]
  chron_ls         <- other_plot[[3]]
  R0_p_mean        <- other_plot[[10]]
  beta_p_mean      <- other_plot[[8]]
  dthetaI_tp1      <- other_plot[[4]]
  dthetaI_tp2      <- other_plot[[5]]
  gamma_p_mean     <- other_plot[[9]]
  dthetaI_tp1_date <- other_plot[[6]]
  dthetaI_tp2_date <- other_plot[[7]]
  
  spaghetti_plot_ls       <- plot_data_ls[[3]]
  spaghetti_ht            <-spaghetti_plot_ls[[1]]
  dthetaI_mean_data       <-spaghetti_plot_ls[[2]]
  sample_dthetaI_mat_long <-spaghetti_plot_ls[[3]]
  second_tp_date_ci       <-spaghetti_plot_ls[[5]]
  first_tp_date_ci        <-spaghetti_plot_ls[[4]]
  
  infection_plot_ls <- plot_data_ls[[4]]
  y_text_ht         <- infection_plot_ls[[1]]
  data_poly         <- infection_plot_ls[[2]]
  data_comp         <- infection_plot_ls[[3]]
  data_pre          <- infection_plot_ls[[4]]
  
  removed_plot_ls <- plot_data_ls[[5]]
  r_text_ht       <- removed_plot_ls[[1]]
  data_poly_R     <- removed_plot_ls[[2]]
  data_comp_R     <- removed_plot_ls[[3]]
  data_pre_R      <- removed_plot_ls[[4]]
  
  india_confirm <- round(N * (data_comp[(T_prime + 1):(T_prime + f_length), "mean"] +
                                data_comp_R[(T_prime + 1):(T_prime + f_length), "mean"]))
  india_confirm_up <- round(N * (data_comp[(T_prime + 1):(T_prime + f_length), "upper"] +
                                 data_comp_R[(T_prime + 1):(T_prime + f_length), "upper"]))
  india_confirm_low <- round(N * (data_comp[(T_prime+1):(T_prime+f_length),"lower"]+
                                     data_comp_R[(T_prime+1):(T_prime+f_length),"lower"]))
  if (adj == TRUE) {
    adj_v <- mean(as.vector(dataf[(T_prime - adj_len):T_prime])/N/(data_comp[(T_prime - adj_len):T_prime, "mean"] +
                                                                     data_comp_R[(T_prime - adj_len):T_prime, "mean"]))
    
    india_confirm_up  <- round(india_confirm_up * adj_v)
    india_confirm_low <- round(india_confirm_low * adj_v)
    india_confirm     <- round(india_confirm * adj_v)
  }
  
  chron <- chron_ls[1:f_length]
  dates <- as.Date(chron[length(as.Date(start_date):(as.Date(last_obs_date)+1)):length(chron)], origin = "1970-01-01")
  
  india_confirm      <- india_confirm[1:length(dates)]
  india_confirm_up  <- india_confirm_up[1:length(dates)]
  india_confirm_low <- india_confirm_low[1:length(dates)]
  
  tib <- tibble(
    date     = dates,
    value    = india_confirm,
    upper_ci = india_confirm_up,
    lower_ci = india_confirm_low
  )
  
  if (!is.null(name)) {
    tib <- tib %>% add_column(Scenario = name)
  }
  
  return(tib)
  
}

# truncates pi schedules for forecasting ----------
elefante <- function(dates, pis, anchor = Sys.Date()) {
  
  if (max(as.Date(dates, "%m/%d/%Y")) > as.Date(anchor)) {
    drpr      <- length(dates[dates <= format(as.Date(anchor), "%m/%d/%Y")]) + 1
    tmp_dates <- c(format(as.Date(anchor), "%m/%d/%Y"), dates[drpr:length(dates)])
    tmp_pis   <- c(1, pis[drpr:length(pis)])
  }
  
  if (max(as.Date(dates, "%m/%d/%Y")) <= as.Date(anchor)) {
    tmp_dates <- format(as.Date(anchor), "%m/%d/%Y")
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

# extract r0 information from summary.csv files ----------
extractr <- function(file) {
  options(warn = -1)
  tib <- read_csv(file, col_types = cols()) %>%
    t() %>%
    as.data.frame() %>%
    mutate(
      name = rownames(.),
      value = as.character(V1)
    ) %>%
    select(name, value) %>%
    as_tibble()
  
  options(warn = 1)
  
  return(
    list(
      r0 = c(
        "mean"  = tib %>% filter(name == "R0_p_mean") %>% pull(value) %>% as.numeric(),
        "lower" = tib %>% filter(name == "R0_p_ci_low") %>% pull(value) %>% as.numeric(),
        "upper" = tib %>% filter(name == "R0_p_ci_up") %>% pull(value) %>% as.numeric()
      ),
      beta = c(
        "mean"  = tib %>% filter(name == "beta_p_mean") %>% pull(value) %>% as.numeric(),
        "lower" = tib %>% filter(name == "beta_p_ci_low") %>% pull(value) %>% as.numeric(),
        "upper" = tib %>% filter(name == "beta_p_ci_up") %>% pull(value) %>% as.numeric()
      ),
      gamma = c(
        "mean"  = tib %>% filter(name == "gamma_p_mean") %>% pull(value) %>% as.numeric(),
        "lower" = tib %>% filter(name == "gamma_p_ci_low") %>% pull(value) %>% as.numeric(),
        "upper" = tib %>% filter(name == "gamma_p_ci_up") %>% pull(value) %>% as.numeric()
      )
    )
  )
}

# extract specfic dates for values -----------
oil_rig <- function(dat, dates, colores) {
  
  tmp <- list()
  
  for (i in seq_along(dates)) {
    tmp[[as.character(dates[i])]] <- dat %>% filter(Dates == dates[i] & color %in% colores) %>% select(Dates, color, value, upper_ci, incidence, value100k, incidence100k)
  }
  
  return(tmp)
  
}

oil_rig_diff <- function(dat, dates, var_1, var_2, col) {
  
  tmp <- list()
  
  for (i in seq_along(dates)) {
    
    val_1 <- dat %>% filter(date == dates[i] & Scenario == var_1) %>% .[[col]]
    val_2 <- dat %>% filter(date == dates[i] & Scenario == var_2) %>% .[[col]]
    
    tmp[[as.character(dates[i])]] <- tibble(
      diff_type = col,
      var_1 = var_1,
      val_1 = val_1,
      var_2 = var_2,
      val_2 = val_2,
      diff  = val_1 - val_2
    )
  }
  
  return(tmp)
  
}

# # extract values for arrows - does not appear to be working !!!
# arro <- function(date) {
#   
#   cau <- NA
#   soc <- NA
#   
#   for (i in seq_along(date)) {
#     cau[i] <- plt_data %>% filter(date == date[i] & Scenario == "Cautious return") %>% .$incidence100k
#     soc[i] <- plt_data %>% filter(date == date[i] & Scenario == "Soc. Dist. + Travel Ban") %>% .$incidence100k
#   }
#   
#   tibble(
#     date     = date,
#     cautious = cau,
#     social   = soc
#   )
# }

# libraries -----------
library(tidyverse)
library(here)
library(ggtext)
library(extrafont)
library(glue)
library(EpiEstim)
library(httr)
library(janitor)
library(scales)
library(gridExtra)
library(grid)
library(vroom)

plot_fig_forest = function() {
  
  # ggplot theme ------------
  covind19_base <- theme_minimal() +
    theme(
      # text               = element_text(family = "Helvetica Neue"),
      plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
      plot.subtitle      = element_text(size = 14, color = "#36454f"),
      plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
      axis.text          = element_text(size = 10, color = "#36454f"),
      axis.title         = element_text(size = 12, face = "italic"),
      legend.position    = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  # cfr calculation function ------------
  CFR <- function(C,D) {
    cfr       <- D / C
    cfr_logit <- log(cfr) - log(1 - cfr)
    sd_logit  <- sqrt(C / (D * (C - D)))
    
    lower_logit <- cfr_logit - qnorm(0.975) * sd_logit
    upper_logit <- cfr_logit + qnorm(0.975) * sd_logit
    
    upper <- exp(upper_logit) / (1 + exp(upper_logit))
    lower <- exp(lower_logit) / (1 + exp(lower_logit))
    
    return(c(cfr, upper, lower))
  }
  
  # calculate doubling time ----------
  dbl_timr <- function(data, end_date, time) {
    
    start <-  data %>% filter(date == as.Date(as.Date(end_date) - time)) %>% pull(total_cases)
    
    if (length(start) == 0) {
      NA
    } else if (start == 0) {
      NA
    } else {
      end   <- data %>% filter(date == as.Date(end_date)) %>% pull(total_cases)
      
      r <- ((end - start) / start) * 100
      
      time * (log(2) / log(1 + (r / 100)))
    }
  }
  
  # est_r for states function -----------
  estR0_out <- function(dat) {
    
    # if (dim(dat)[1] < 7) {
    #   NA
    # } else {
    
    t_start <- seq(2, nrow(dat) - 4)
    t_end   <- t_start + 4
    
    res <- estimate_R(
      incid = dat$daily_cases,
      method = "parametric_si",
      config = make_config(list(
        mean_si             = 7,
        std_si              = 4.5,
        si_parametric_distr = "G",
        t_start             = t_start,
        t_end               = t_end,
        seed                = set_seed))
    ) 
    
    tibble(
      date_num = res$dates
    ) %>% left_join(
      res$R, by = c("date_num" = "t_end")
    ) %>%
      dplyr::select(
        date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
      ) %>%
      add_column(date = dat$date) %>%
      dplyr::select(-date_num) %>%
      dplyr::select(date, tidyselect::everything())
    
    # }
  }
  
  # Case-fatality rate plot -----------
  states_map <- c("Andhra Pradesh" =  "AP", "Arunachal Pradesh" =  "AR",
                  "Assam" =  "AS", "Bihar" =  "BR", "Chhattisgarh" =  "CG", "Goa" =  "GA",
                  "Gujarat" =  "GJ", "Haryana" =  "HR", "Himachal Pradesh" =  "HP",
                  "Jammu and Kashmir" =  "JK", "Jharkhand" =  "JH", "Karnataka" =  "KA",
                  "Kerala" =  "KL", "Madhya Pradesh" =  "MP",  "Maharashtra" =  "MH",
                  "Manipur" =  "MN", "Meghalaya" =  "ML", "Mizoram" =  "MZ", "Nagaland" =  "NL",
                  "Odisha" =  "OR", "Punjab" =  "PB", "Rajasthan" =  "RJ", "Sikkim" =  "SK",
                  "Tamil Nadu" =  "TN", "Tripura" =  "TR", "Uttarakhand" =  "UK",
                  "Uttar Pradesh" =  "UP", "West Bengal" =  "WB", "Tamil Nadu" =  "TN",
                  "Tripura" =  "TR", "Andaman and Nicobar Islands" =  "AN",
                  "Chandigarh" =  "CH", "Dadra and Nagar Haveli" =  "DH",
                  "Daman and Diu" = "DD", "Delhi" =  "DL", "Lakshadweep" =  "LD",
                  "Pondicherry" =  "PY", "Telangana" =  "TG", "Dadra and Nagar Haveli" =  "DN",
                  "Chhattisgarh" =  "CT", "Ladakh" =  "LA", "Uttarakhand" =  "UT"
  )
  
  x        <- names(states_map)
  names(x) <- states_map
  
  request  <- GET("https://api.covid19india.org/states_daily.json")
  json     <- content(request)
  state    <- map_dfr(json[[1]], ~ .x)
  
  state$tt <- NULL                                                                                                      # remove tt column from data (this is the nationwide counts)
  
  state_codes <- setdiff(names(state), c("date", "status"))                                                            # pull state codes from dataset names
  
  xstates <- vroom(paste0(data_repo, today, "/covid19india_data.csv")) %>%
    group_by(State) %>%
    filter(Date == max(Date) & State != "un") %>%
    ungroup() %>%
    top_n(20, Cases)
  
  state <- state %>%                                       
    clean_names() %>%                                        
    pivot_longer(names_to = "state", values_to = "count", -c(date, status)) %>%                                        # formerly 'gather' - transform from wide to long
    mutate(                                        
      count = as.numeric(count),                                                                                       # change count from character to numeric
      date = as.Date(date, format = "%d-%b-%y")                                                                        # change date from character to date
    ) %>%
    pivot_wider(names_from = status, values_from = count, id_cols = c(date, state), values_fill = list(count = 0)) %>% # formerly 'spread' - create counts for cases, deaths, and recovereds
    rename(                                                                                                            
      daily_cases     = Confirmed,                                                                                           # rename 'Confirmed' to 'cases'
      daily_deaths    = Deceased,                                                                                            # rename 'Deceased' to 'deaths'
      daily_recovered = Recovered                                                                                            # rename 'Recovered' to 'recovered'
    ) %>%
    mutate(
      name = recode(str_to_upper(state), !!!x)                                                                         # add column with state name
    ) %>%
    arrange(state, date) %>%                                                                                           # sort by state, then within state by date
    group_by(state) %>%                                                                                                # create groups
    mutate(
      total_cases     = accumulate(daily_cases, `+`),                                                                  # turn cases into cumulative
      total_deaths    = accumulate(daily_deaths, `+`),                                                                 # turn deaths into cumulative
      total_recovered = accumulate(daily_recovered, `+`)                                                               # turn recovered in cumulative
    ) %>% 
    ungroup() %>%                                                                                                      # need to ungroup
    filter(date >= "2020-03-15",
           state %in% xstates$State)                                                                                     # only using data until 5-18 for paper
  
  request <- GET("https://api.covid19india.org/data.json")
  json    <- content(request)
  
  national <- map_dfr(json[[1]], ~ .x) %>%
    clean_names() %>%
    dplyr::rename(
      daily_cases     = dailyconfirmed,
      daily_deaths    = dailydeceased,
      daily_recovered = dailyrecovered,
      total_cases     = totalconfirmed,
      total_deaths    = totaldeceased,
      total_recovered = totalrecovered
    ) %>%
    mutate(
      date = as.Date(date, "%d %B")
    ) %>%
    mutate_if(
      is.character,
      as.numeric
    ) %>%
    filter(date >= "2020-03-01")
  
  cfr_state <- state %>% group_by(state) %>% filter(date == max(date)) %>% ungroup()
  cfr_nat   <- national %>% filter(date == max(date))
  
  # c <- cfr_state %>%
  #   mutate(
  #     total_cases = case_when(
  #       state == "mp" ~ 5236,
  #       state != "mp" ~ total_cases
  #     )
  #   )
  
  # Compute CFR and CIs
  
  data <- tibble(
    name = c(cfr_state$name,"National estimate"),
    C    = c(cfr_state$total_cases, cfr_nat$total_cases),
    D    = c(cfr_state$total_deaths, cfr_nat$total_deaths)
  )
    
    # structure(list(name = c(cfr_state$name,"National estimate"),
    #                      C    = c(cfr_state$total_cases, cfr_nat$total_cases),
    #                      D    = c(cfr_state$total_deaths, cfr_nat$total_deaths)),
    #                 class = c("tbl_df", "tbl", "data.frame"),
    #                 row.names = c(NA, -39L))
    # 
  test_data <- tibble(
    name  = data$name,
    cfr   = rep(0,nrow(data)),
    upper = rep(0,nrow(data)),
    lower = rep(0,nrow(data))
  )
    
    # structure(list(name  = data$name,
    #                           cfr   = rep(0,nrow(data)),
    #                           upper = rep(0,nrow(data)),
    #                           lower = rep(0,nrow(data))),
    #                      class = c("tbl_df", "tbl", "data.frame"),
    #                      row.names = c(NA, -nrow(data)))
  
  for (i in 1:nrow(test_data)) {
    C <- data$C[i]
    D <- data$D[i]
    
    result <- CFR(C,D)
    
    test_data$cfr[i]   <- result[1]
    test_data$upper[i] <- result[2]
    test_data$lower[i] <- result[3]
  }
  
  # Forest plot
  
  fplot_colors <- c(
    "alarm" = "#eb4034",
    "eh"    = "gray40",
    "good"  = "#138808",
    "india" = "black"
  )
  
  cfr_danger <- 0.06
  cfr_safe   <- 0.03
  
  cfr_title    <- "Case fatality rate for COVID-19 in India by state/union territory"
  cfr_subtitle <- glue("as of {format(as.Date(today), '%B %e')}")
  cfr_x_lab    <- "State/Union territory"
  cfr_y_lab    <- "CFR"
  cfr_caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",
                       "**Source:** covid19india.org<br>",
                       "**Note:**<br>",
                       " - Estimate and 95% confidence interval are provided in each plot by state.<br>",
                       " - Colored red if estimate is above 0.03 and green if below 0.01.<br>",
                       " - Estimation is based on all cases confirmed till May 18.<br>",
                       " - CFR stands for case-fatality rate.")
  
  cfr1_for <- test_data %>%
      mutate(
        fplot = ifelse(name == "National estimate", "india", ifelse(cfr > cfr_danger, "alarm", ifelse(cfr < cfr_safe, "good", "eh"))) # change 
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(name, cfr), "National estimate"), y = cfr, shape = shape)) +
      geom_hline(yintercept = cfr_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = cfr_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = cfr_title,
        subtitle = cfr_subtitle,
        x        = cfr_x_lab,
        y        = cfr_y_lab,
        caption  = cfr_caption
      ) +
      coord_flip(
        ylim = c(0, 0.1)
      ) +
      covind19_base
  
  # dbl time -----------
  national$dbl <- NA
  for (i in seq_along(national$date)) {
    national$dbl[i] <- dbl_timr(data = national, end_date = national$date[i], time = 7)
  }
  
  dbl_nat_t7_avg <- national %>% pull(dbl) %>% tail(7) %>% mean()
  dbl_nat_t7 <- glue("{format(round(dbl_nat_t7_avg, 2), nsmall = 2)}")
  
  for (j in seq_along(unique(state$state))) {
    
    tmp_state <- unique(state$state)[j]
    
    tmp_dat <- state %>% filter(state == tmp_state)
    
    tmp_dat$dbl <-  NA
    
    for (i in seq_along(tmp_dat$date)) {
      
      tmp_dat$dbl[i] <- dbl_timr(data = tmp_dat, end_date = tmp_dat$date[i], time = 7)
      
    }
    
    txt <- glue("{unique(state$state)[j]} <- tmp_dat")
    eval(parse(text = txt))
    
  }
  
  new_dat <- eval(parse(text = glue("bind_rows({paste(unique(state$state), collapse = ', ')})")))
  new_dat$dbl[is.infinite(new_dat$dbl)] <- NA
  
  state_t7_avg <- new_dat %>%
    group_by(name) %>%
    # drop_na(dbl) %>%
    slice((n()-6):n()) %>%
    summarise(
      lower = min(dbl, na.rm = T),
      upper = max(dbl, na.rm = T),
      dbl   = mean(dbl, na.rm = T)
    ) %>%
    drop_na(dbl) %>%
    ungroup()
  
  dbl_nat_t7_avg <- national %>% pull(dbl) %>% tail(7) %>% mean()
  dbl_nat_t7_lci <- national %>% pull(dbl) %>% tail(7) %>% min()
  dbl_nat_t7_uci <- national %>% pull(dbl) %>% tail(7) %>% max()
  
  dbl_danger <- 21
  dbl_safe   <- 28
  
  dbl_for <- state_t7_avg %>%
      mutate(
        fplot = ifelse(dbl < dbl_danger, "alarm", ifelse(dbl > dbl_safe, "good", "eh"))
      ) %>%
      add_row(name = "National estimate", dbl = dbl_nat_t7_avg, lower = dbl_nat_t7_lci, upper = dbl_nat_t7_uci, fplot = "india") %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(name, desc(dbl)), "National estimate"), y = dbl, shape = shape)) +
      geom_hline(yintercept = dbl_danger, color = "gray40", linetype = 2) +
      geom_hline(yintercept = dbl_safe, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "Doubling time for COVID-19 in India<br>by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "Doubling time (days)",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                        "**Source:** covid19india.org<br>",
                        "**Note:** <br>",
                        " - Colored red if estimate is below {dbl_danger} and green if above {dbl_safe}.<br>",
                        " - Intervals represent the range of doubling times over the last 7 days.")
      ) +
      coord_flip() +
      covind19_base
  
  
  # r est -----------
  set_seed <- 46342
  set.seed(set_seed)
  
  r_nat_dat <- national %>%
    drop_na(daily_cases)
  
  t_start <- seq(2, nrow(r_nat_dat) - 4)
  t_end   <- t_start + 4
  
  res <- estimate_R(
    incid     = r_nat_dat$daily_cases,
    method    = "parametric_si",
    config    = make_config(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = set_seed
    )
  )
  
  r_nat_data <- tibble(
    date_num = res$dates
  ) %>% left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    add_column(date = r_nat_dat$date)
  
  r_state_dat <- state %>%
    filter(daily_cases > 0 & total_cases >= 50) %>%
    group_by(state) %>%
    mutate(
      ns = n()
    ) %>%
    ungroup() %>%
    filter(ns >=7)
  
  options(warn = -1)
  state_est <- r_state_dat %>%
    dplyr::select(date, daily_cases, name) %>%
    nest(data = c(-name)) %>%
    mutate(
      estR0 = map(data, ~estR0_out(dat = .x))
    ) %>%
    unnest(estR0) %>%
    dplyr::select(-data) %>%
    filter(date >= "2020-03-23") %>%
    drop_na()
  options(warn = 1)
  
  state_last <- state_est %>%
    group_by(name) %>%
    filter(date == max(date)) %>% 
    ungroup()
  
  state_t7_avg <- state_est %>%
    group_by(name) %>%
    slice((n()-6):n()) %>%
    summarise(
      r = mean(r),
      lower = mean(lower),
      upper = mean(upper)
    ) %>%
    ungroup()
  
  r_t7_avg <- r_nat_data %>% pull(r) %>% tail(7) %>% mean()
  r_t7_lci <- r_nat_data %>% pull(lower) %>% tail(7) %>% mean()
  r_t7_uci <- r_nat_data %>% pull(upper) %>% tail(7) %>% mean()
  
  r_safe   <- 1
  r_danger <- 1.5
  
  r_est_for <- state_t7_avg %>%
      mutate(
        fplot = ifelse(r > r_danger, "alarm", ifelse(r < r_safe, "good", "eh"))
      ) %>%
      add_row(name = "National estimate", r = r_t7_avg, lower = r_t7_lci, upper = r_t7_uci, fplot = "india") %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(name, r), "National estimate"), y = r, shape = shape)) +
      geom_hline(yintercept = r_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = r_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "R for COVID-19 in India by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "R",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                        "**Source:** covid19india.org<br>",
                        "**Note:**<br>",
                        " - Average estimate and 95% confidence interval for last 7 days are provided in each plot by state.<br>",
                        " - Colored red if estimate is above {r_danger} and green if below {r_safe}.")
      ) +
      coord_flip(ylim = c(0, 3.5)) +
      covind19_base
  
  
  # test positive rate ------------
  nat_test <- read_csv(url("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv"), col_types = cols()) %>%
    clean_names() %>%
    mutate(date = as.Date(updated_on, format = '%d/%m/%Y')) %>%
    dplyr::select(date, positive, total_tested, state) %>%
    group_by(state) %>%
    mutate(
      daily_tested   = total_tested - dplyr::lag(total_tested),
      daily_positive = positive - dplyr::lag(positive)
    ) %>%
    ungroup() %>%
    drop_na() %>%
    dplyr::select(-state) %>%
    group_by(date) %>%
    summarise(
      positive     = sum(positive),
      total_tested = sum(total_tested),
      daily_test   = sum(daily_tested),
      daily_pos    = sum(daily_positive)
    ) %>%
    mutate(
      test_pos       = positive / total_tested,
      daily_test_pos = daily_pos / daily_test
    ) %>%
    ungroup()
  
  state_test <- read_csv(url("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv"), col_types = cols()) %>%
    clean_names() %>%
    mutate(date = as.Date(updated_on, format = '%d/%m/%Y')) %>%
    dplyr::select(date, positive, total_tested, state) %>%
    mutate(test_pos = positive / total_tested) %>%
    drop_na() %>%
    left_join(tibble(
      state = names(states_map),
      abbrev = tolower(states_map)
    ), by = c("state"))
  # filter(abbrev %in% use_abbrev)
  
  state_test_plt_dat <- state_test %>%
    filter(state != "Telangana", abbrev %in% xstates$State) %>%
    group_by(state) %>%
    mutate(
      test_pos = positive / total_tested
    ) %>%
    slice((n()-6):n()) %>%
    summarise(
      lower    = min(test_pos, na.rm = T),
      upper    = max(test_pos, na.rm = T),
      test_pos = mean(test_pos, na.rm = T)
    ) %>%
    drop_na(test_pos) %>%
    ungroup()
  
  tpr_nat_t7_avg <- nat_test %>% pull(test_pos) %>% tail(7) %>% mean()
  tpr_nat_t7_lci <- nat_test %>% pull(test_pos) %>% tail(7) %>% min()
  tpr_nat_t7_uci <- nat_test %>% pull(test_pos) %>% tail(7) %>% max()
  
  tpr_safe   <- 0.02
  tpr_danger <- 0.06
  
  tp_for <- state_test_plt_dat %>%
      # bind_rows(tg_est) %>%
      mutate(
        fplot = ifelse(test_pos > tpr_danger, "alarm", ifelse(test_pos < tpr_safe, "good", "eh"))
      ) %>%
      add_row(state = "National estimate", test_pos = tpr_nat_t7_avg, lower = tpr_nat_t7_lci, upper = tpr_nat_t7_uci, fplot = "india") %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(state, test_pos), "National estimate"), y = test_pos, shape = shape)) +
      geom_hline(yintercept = tpr_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = tpr_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "Test-positive rate for COVID-19 in India<br>by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "Test-positive rate",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>**Source:** covid19india.org<br>",
                        "**Note:**<br>",
                        " - Colored red if estimate is above {tpr_danger} and green if below {tpr_safe}.<br>",
                        " - Telangana is based on only 5 days of data.")
      ) +
      coord_flip() +
      covind19_base
  
  
  test_max_date <- max(state_test$date)
  
  # dashboard --------------
  # dashboard
  remove <- labs(
    subtitle = NULL,
    caption  = NULL,
    title    = NULL
  )
  
  # cairo_pdf(here("figs", "dashboard_v1.pdf"), width = 12, height = 12)
  
  ga_for = arrangeGrob(
    cfr1_for + 
      # remove + 
      theme(axis.title.y = element_blank()) + 
      labs(
        title = "a. Case-fatality rate",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with 95% confidence interval shown.<br>",
                       " - Colored red if estimate is above {cfr_danger} and green if below {cfr_safe}.")),
    dbl_for + 
      # remove + 
      theme(axis.title.y = element_blank()) +
      labs(
        title = "b. Doubling time",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with range shown.<br>",
                       " - Colored red if estimate is below {dbl_danger} and green if above {dbl_safe}.")),
    r_est_for + 
      # remove + 
      theme(axis.title.y = element_blank()) + 
      labs(
        title = "c. Effective reproduction number",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with 95% confidence interval shown.<br>",
                       " - Colored red if estimate is above {r_danger} and green if below {r_safe}.")
      ),
    tp_for +
      # remove +
      labs(
        title   = "d. Test-positive rate",
        subtitle = NULL,
        caption = glue("**Notes:**<br>", 
                       " - 7-day average estimate with range shown.<br>",
                       " - Colored red if estimate is above {tpr_danger} and green if below {tpr_safe}.")
      ) +
      theme(axis.title.y = element_blank()),
    ncol   = 2,
    nrow   = 2,
    top    = textGrob("COVID-19 in India Dashboard", hjust = 0, x = 0.1, gp = gpar(fontsize = 27, fontface = "bold")),
    bottom = textGrob(glue("\uA9 COV-IND-19 Study Group\n",
                           "Source: covid19india.org (data through {format(test_max_date, '%B %e')})"),
                      hjust = 0, x = 0.1, gp = gpar(fontsize = 12))
  )
  # dev.off()
  return(list(
    cfr1_for = cfr1_for, dbl_for = dbl_for, 
    r_est_for = r_est_for, tp_for = tp_for, 
    ga_for = ga_for 
  ))
}


# get count data ----------
get_count_data <- function(d, abbrevs = NULL) {
    
    # national-level count data
    nat_dat <- readr::read_tsv(paste0(data_repo, today, '/jhu_data_mod.csv'), col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::filter(country == "India") %>%
        dplyr::rename(place = country) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_cases     = cases - dplyr::lag(cases),
            daily_deaths    = deaths - dplyr::lag(deaths),
            daily_recovered = recovered - dplyr::lag(recovered)
        ) %>%
        tibble::add_column(abbrev = "India")
    
    # state-level count data
    state_dat <- readr::read_tsv(paste0(data_repo, today, '/covid19india_data.csv'),
                                 col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::rename(
            abbrev = state,
            place  = name
        ) %>%
        dplyr::filter(abbrev != "un") %>%
        dplyr::group_by(place) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_cases     = cases - dplyr::lag(cases),
            daily_deaths    = deaths - dplyr::lag(deaths),
            daily_recovered = recovered - dplyr::lag(recovered)
        ) %>%
        dplyr::ungroup()
    
    if (is.null(abbrevs)) {
        abbrevs <- state_dat %>%
            dplyr::group_by(place) %>%
            dplyr::filter(cases == max(cases)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(desc(cases)) %>%
            #utils::head(20) %>%
            dplyr::pull(place)
    }
    
    state_dat <- state_dat %>% dplyr::filter(place %in% abbrevs)
    
    dplyr::bind_rows(nat_dat, state_dat)
    
}

# get testing data ----------
get_testing_data <- function(d) {
    
    # population data
    pops <- readr::read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19/IRIS/model/r_scripts/one_off/state_pop.csv", col_types = cols()) %>%
        dplyr::rename(place = state)
    
    # national-level testing data
    nat_test <- readr::read_csv(paste0(data_repo, today, '/testing.csv'),
                                col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::select(date, place = country, total_tests = tests) %>%
        dplyr::group_by(date) %>%
        dplyr::filter(total_tests == max(total_tests)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_tests = total_tests - dplyr::lag(total_tests),
            ppt         = total_tests / (pops %>% dplyr::filter(place == "India") %>% dplyr::pull(population))
        )
    
    # state-level testing data
    state_test <- readr::read_csv(paste0(data_repo, today, '/statewise_tested_numbers_data.csv'),
                                  col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
            date = as.Date(updated_on, "%d/%m/%Y"),
            tests_per_million = total_tested / 1332830000 * 1e6,
            ppt  = tests_per_million / 1e6
        ) %>%
        dplyr::select(date, place = state, total_tests = total_tested, ppt) %>%
        dplyr::left_join(pops, by = "place") %>%
        dplyr::group_by(place) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_tests = total_tests - dplyr::lag(total_tests),
            ppt         = total_tests / population) %>%
        dplyr::ungroup() %>%
        dplyr::select(-population) %>%
        dplyr::filter(date <= as.Date(d) - 1)
    
    # combine into single table
    dplyr::bind_rows(nat_test, state_test)
    
}

merge_data <- function(count, test, tpr_d = 0.02) {
    
    dplyr::left_join(
        count,
        test,
        by = c("place", "date")
    ) %>%
        dplyr::mutate(
            tpr       = cases / total_tests,
            shortfall = ((tpr / tpr_d) - 1) * total_tests
        ) %>%
        dplyr::mutate(
            shortfall = case_when(
                shortfall < 0 ~ 0,
                TRUE ~ shortfall
            )
        )
    
}

# do it all ----------
do_it_all <- function(d) {
    
    count_dat <- get_count_data(d = d)
    test_dat  <- get_testing_data(d = d)
    merge_dat <- merge_data(count = count_dat, test = test_dat) %>%
        get_dbl()
    r0_dat <- get_r0(merge_dat) %>%
        dplyr::select(
            place,
            date,
            r_est = r,
            r_lower = lower,
            r_upper = upper
        )
    
    merge_dat %>%
        left_join(r0_dat, by = c("place", "date"))
    
    
}

# forecast data
get_abbrevs <- function(d) {
    
    d %>% pull(abbrev) %>% unique()
    
}

get_forecast_data <- function(d, ab) {
    
    for (i in seq_along(abbrevs)) {
        
        super_tmp_dat <- readr::read_tsv(glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{d}/1wk/{ab[i]}_plot_data.txt"),
                                         col_types = cols()) %>%
            add_column(abbrev = ab[i])
        
        if (i == 1) {
            
            tmp <- super_tmp_dat
            
        } else {
            tmp <- bind_rows(tmp, super_tmp_dat)
        }
        
    }
    
    return(tmp)
    
}

# ggplot theme ----------
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
dbl_timr <- function(data, end_date, time = 7) {
    
    start <-  data %>% 
        dplyr::filter(date == as.Date(as.Date(end_date) - time)) %>% 
        dplyr::pull(cases)
    
    if (length(start) == 0) {
        NA
    } else if (start == 0) {
        NA
    } else {
        end   <- data %>%
            dplyr::filter(date == as.Date(end_date)) %>%
            dplyr::pull(cases)
        
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
    
    res <- EpiEstim::estimate_R(
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
    
    tibble::tibble(
        date_num = res$dates
    ) %>% dplyr::left_join(
        res$R, by = c("date_num" = "t_end")
    ) %>%
        dplyr::select(
            date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
        ) %>%
        tibble::add_column(date = dat$date) %>%
        dplyr::select(-date_num) %>%
        dplyr::select(date, tidyselect::everything())
    
    # }
}


# get cfr ----------
get_cfr <- function(dat) {
    
    tmp <- dat %>%
        dplyr::group_by(place) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::ungroup()
    
    tmp <- tmp %>%
        dplyr::select(
            place = place,
            C     = cases,
            D     = deaths
        )
    
    tmp_out <- tibble(
        place  = tmp$place,
        cfr   = rep(0,nrow(tmp)),
        upper = rep(0,nrow(tmp)),
        lower = rep(0,nrow(tmp))
    )
    
    for (i in 1:nrow(tmp_out)) {
        C <- tmp$C[i]
        D <- tmp$D[i]
        
        result <- CFR(C,D)
        
        tmp_out$cfr[i]   <- result[1]
        tmp_out$upper[i] <- result[2]
        tmp_out$lower[i] <- result[3]
        
    }
    
    tmp_out %>%
        dplyr::mutate(
            place = case_when(
                place == "India" ~ "National estimate",
                TRUE ~ place
            )
        )

}

# get_dbl -----------

get_dbl <- function(dat) {
    
    places <- unique(dat$place)
    
    for (i in seq_along(places)) {
        
        tmp_dat <- dat %>%
            dplyr::filter(place == places[i]) %>%
            tibble::add_column(dbl = NA) %>%
            dplyr::arrange(date)
        
        for (j in seq_along(tmp_dat$date)) {
            
            tmp_dat$dbl[j] <- dbl_timr(data = tmp_dat, end_date = tmp_dat$date[j])
            
        }
        
        tmp_dat$dbl[is.infinite(tmp_dat$dbl)] <- NA
        
        txt <- glue("tmp_{unique(tmp_dat$abbrev)} <- tmp_dat")
        eval(parse(text = txt))
        
    }
    
    eval(parse(text = glue("bind_rows({paste(paste0('tmp_', unique(dat$abbrev)), collapse = ', ')})")))
    
}

# get_r0 -----------
get_r0 <- function(dat) {
    
    tmp_dat <- dat %>%
        dplyr::filter(daily_cases > 0 & cases >= 50) %>%
        dplyr::group_by(place) %>%
        dplyr::mutate(
            ns = n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(ns >=7)
    
    options(warn = -1)
    tmp_est <- tmp_dat %>%
        dplyr::select(date, daily_cases, place) %>%
        tidyr::nest(data = c(-place)) %>%
        dplyr::mutate(
            estR0 = purrr::map(data, ~estR0_out(dat = .x))
        ) %>%
        tidyr::unnest(estR0) %>%
        dplyr::select(-data) %>%
        dplyr::filter(date >= "2020-03-23") %>%
        tidyr::drop_na()
    options(warn = 1)
    
    return(tmp_est)
}

# get_r_est ----------
get_r_est <- function(dat) {
    
    dat %>%
        dplyr::group_by(place) %>%
        dplyr::slice((n()-6):n()) %>%
        dplyr::summarize(
            r       = mean(r_est, na.rm = TRUE),
            lower   = mean(r_lower, na.rm = TRUE),
            upper   = mean(r_upper, na.rm = TRUE),
            .groups = "drop_last"
        ) %>%
        dplyr::ungroup()
    
}

# get_tpr_est -----------
get_tpr_est <- function(dat) {
    
    dat %>%
        group_by(place) %>%
        slice((n()-6):n()) %>%
        summarize(
            tpr_min = min(tpr, na.rm = T),
            tpr_max = max(tpr, na.rm = T),
            tpr     = mean(tpr, na.rm = T),
            .groups = "drop_last"
        ) %>%
        drop_na(tpr) %>%
        ungroup()
    
}

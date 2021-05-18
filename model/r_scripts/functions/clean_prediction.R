clean_prediction <- function(x, state, obs_days, t_pred) {
  
  vals <- c(
    "susceptible",
    "exposed",
    "infectious_unreported",
    "positive_reported",
    "false_negative",
    "recovered_unreported",
    "recovered_reported",
    "death_unreported",
    "death_reported",
    "positive_daily_reported",
    "recovered_daily_reported",
    "death_daily_reported",
    "false_negative_daily",
    "unreported_daily"
  )
  
  as_tibble(x, .name_repair = "unique") %>%
    mutate(
      state   = state,
      section = rep(vals, each = obs_days + t_pred),
      date    = rep(as.Date(min_date:(min_date + (obs_days + t_pred - 1)), origin = "1970-01-01"), 14),
      pred    = rep(c(rep(0, obs_days), rep(1, t_pred)), 14)
    ) %>%
    dplyr::select(state, section, date, pred, everything()) %>%
    rowwise(state, section, date, pred) %>%
    summarize(
      min    = min(c_across()),
      p2.5   = quantile(c_across(), 0.025),
      median = median(c_across()),
      mean   = mean(c_across()),
      p97.5  = quantile(c_across(), 0.975),
      max    = max(c_across()),
      sd     = sd(c_across()),
      .groups = "drop"
    )
  
}

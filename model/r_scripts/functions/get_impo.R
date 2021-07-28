get_impo <- function(x, res, obs_days = obs_days, t_pred = t_pred) {
  
  p_pred <- x %>%
    filter(section == "positive_reported") %>%
    dplyr::select(-(1:4)) %>%
    rowMeans()
  
  r_pred <- x %>%
    filter(section == "recovered_reported") %>%
    dplyr::select(-(1:4)) %>%
    rowMeans()
  
  d_pred <- x %>%
    filter(section == "death_reported") %>%
    dplyr::select(-(1:4)) %>%
    rowMeans()
  
  t_d <- r_pred + d_pred + p_pred
  total_pred <- rowSums(matrix(rowMeans(res[["prediction"]]), nrow = obs_days + t_pred)[, 3:9])
  UF_p <- total_pred / t_d
  
  d_u <- x %>%
    filter(section == "death_unreported") %>%
    dplyr::select(-(1:4)) %>%
    rowMeans()
  total_death <- d_u + d_pred
  UF_d <- total_death / d_pred
  ifr <- total_death / total_pred
  
  tibble(
    "underreporting_cases"  = UF_p[obs_days + 1],
    "underreporting_deaths" = UF_d[obs_days + 1],
    "ifr"                   = ifr[obs_days + 1]
  )
  
}
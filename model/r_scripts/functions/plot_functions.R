# various plot related functions ----------
my_diff <- function(x) {
  c(2*diff(x)[1]-diff(x)[2], diff(x))
}

average_pred <- function(data, sec, n_date) {
  
  data %>%
    dplyr::filter(section == sec & pred == 1) %>%
    arrange(date) %>%
    slice_head(n = n_date) %>%
    mutate(
      value = rowMeans(.[4:ncol(data)])
    ) %>%
    dplyr::select(state, section, date, pred, value)
  
}

final_theme <-   theme_minimal() +
  theme(
    plot.title         = element_text(size = 20, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = element_text(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 18, color = "#36454f"),
    axis.title         = element_text(size = 20, face = "italic"),
    legend.title       = element_blank() ,
    legend.text        = element_text(size = 16) ,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position    = "bottom",
    axis.ticks         = element_line(),
    axis.ticks.length  = unit(5, "pt"),
    axis.line          = element_line()
  )





addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}
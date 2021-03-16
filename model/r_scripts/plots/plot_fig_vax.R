
suppressPackageStartupMessages({
library(vroom)
library(tidyverse)
library(ggtext)
library(scales)
})

plot_fig_vax = function() {
  vax_dat <- suppressMessages(vroom("http://api.covid19india.org/csv/latest/vaccine_doses_statewise.csv")) %>%
    pivot_longer(
      names_to = "date",
      values_to = "vaccines",
      -State
    ) %>%
    mutate(
      date = as.Date(date, format = "%d/%m/%Y")
    ) %>%
    dplyr::rename(
      state = State
    ) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(
      daily_vaccines = vaccines - dplyr::lag(vaccines)
    ) %>%
    ungroup()
  
  vax_dat %>%
    filter(state == "Total") %>%
    ggplot(aes(x = date, y = vaccines)) +
    geom_line() +
    labs(
      title = "Cumulative COVID-19 vaccines delivered in India",
      x     = "Date",
      y     = "Number of vaccines",
      caption = "**Source:** covid19india.org<br>**\uA9 COVIND-19 Study Group**"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal()+
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_markdown(hjust = 0)
    )
  
  vax_india = vax_dat %>% filter(state == "Total") %>% 
    rename(Day = date, Vaccines = vaccines) %>% 
    mutate(text = paste0("India", "<br>", Day, ": ", format(Vaccines, big.mark = ","),
                         " total vaccines<br>")) %>% 
    filter(Day >= as.Date("2021-01-15"))
  
  india_color <- "#138808"
  names(india_color) <- "India"
  
  vax.title <- "Cumulative COVID-19 vaccines delivered in India"
  
  axis.title.font <- list(size = 16)
  
  vax.xaxis <- list(title = "Date",
                    titlefont = axis.title.font, showticklabels = TRUE,
                    tickangle = 0, showline = T, zeroline = F)
  
  vax.yaxis <- list(title = "Number of vaccines", titlefont =
                      axis.title.font, zeroline = F, showline = F)
  
  case_plot <- plot_ly(vax_india, x = ~ Day, y = ~ Vaccines, text = ~ text, color = I("#138808"),
                       hoverinfo = "text", mode = "markers+lines", hoverlabel = list(align = "left"),
                       showlegend = F, line = list(width = 3)
  ) %>%
    layout(xaxis = vax.xaxis, yaxis = vax.yaxis,
           annotations = list(text = vax.title, xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22))
    )
  
  case_plot
}

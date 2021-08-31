
suppressPackageStartupMessages({
library(vroom)
library(tidyverse)
library(ggtext)
library(scales)
library(tidyr)
library(covid19india)
})

plot_fig_vax = function() {
  
  vax_dat = get_state_vax() %>% 
    filter(place == "India") %>% 
    mutate(date = as.Date(date, "%e/%m/%Y"))
  
  vax_india = vax_dat %>% 
    rename(Day = date) %>% 
    mutate(text = paste0("India", "<br>", Day, ": ", format(daily_doses, big.mark = ","),
                         " daily vaccines<br>")) %>% 
    filter(Day >= as.Date("2021-03-15"))
  
  india_color <- "#138808"
  names(india_color) <- "India"
  
  vax.title <- "Daily COVID-19 vaccines delivered in India"
  
  axis.title.font <- list(size = 16)
  
  vax.xaxis <- list(title = "Date",
                    titlefont = axis.title.font, showticklabels = TRUE,
                    tickangle = 0, showline = T, zeroline = F)
  
  vax.yaxis <- list(title = "Number of vaccines", titlefont =
                      axis.title.font, zeroline = F, showline = F)
  
  case_plot <- plot_ly(vax_india, x = ~ Day, y = ~ daily_doses, text = ~ text, color = I("#138808"),
                       hoverinfo = "text", type = "bar", hoverlabel = list(align = "left"),
                       showlegend = F, line = list(width = 3)
  ) %>%
    layout(xaxis = vax.xaxis, yaxis = vax.yaxis,
           annotations = list(text = vax.title, xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22))
    )
  
  case_plot
}

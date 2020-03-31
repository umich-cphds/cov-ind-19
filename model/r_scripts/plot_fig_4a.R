library(tidyverse)
library(vroom)
library(plotly)

start.date <- as.Date("2020-03-01")
latest <- Sys.Date()

data <- vroom(paste0("~/cov-ind-19-data/", latest, "/1wk/figure_4_data.csv")) %>%
mutate(variable = factor(variable, levels = c("True", "mod_3",
       "mod_2", "mod_4", "mod_4_up")
)) %>%
mutate(variable = recode(variable,
    "True" = "Observed",
    "mod_3" = "No intervention",
    "mod_2" = "Social distancing",
    "mod_4" = "Lockdown with moderate release",
    "mod_4_up" = "Lockdown upper credible interval")
) %>%

mutate(text = paste0(format(Dates, format("%b %d")), ": ", value,
                    ifelse(variable == "Observed", " observed cases",
                                                   " projected cases")),
       i = variable != "Lockdown upper credible interval"
)

title <- paste("Cumulative number of COVID-19 cases in India compared",
               "to other countries affected by the pandemic")

cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
              format(latest, format = "%b %d"), sep = ' ')

axis.title.font <- list(size = 16)
tickfont        <- list(size = 16)

xaxis <- list(title = "Date",
              titlefont = axis.title.font, showticklabels = TRUE,
              tickangle = -30, showline = T)

yaxis <- list(title = "Cumulative number of cases", type = "log",
              dtick = 1, titlefont = axis.title.font, zeroline = T)


colors <- c("#979799", "#ED553B", "#f2c82e", "#173F5F", "#173F5F")
p <- plot_ly(data %>% filter(i),
        x = ~ Dates, y = ~ value, text = ~text, color = ~variable,
        colors = colors, type = "bar", hoverinfo = "text"
) %>%
add_trace(data = data %>% filter(!i), x = ~Dates, y = ~value,
          type = "scatter", mode = "line"
) %>%
layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
       title = list(text = cap, xanchor = "left", x = 0)
)

saveRDS(p, paste0("~/cov-ind-19-data/", latest, "/plot4a.RDS"))

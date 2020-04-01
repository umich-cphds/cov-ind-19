library(tidyverse)
library(vroom)
library(plotly)

start.date <- as.Date("2020-03-01")
latest <- Sys.Date()

data <- read_csv(paste0("~/cov-ind-19-data/", latest, "/1wk/figure_4_data.csv")) %>%
select(-color) %>%
group_by(Dates) %>%
nest() %>%
mutate(data = map(data,
    function(data)
    {
        data$ref <-
        if (nrow(data) == 1)
            NA
        else
            data$value[c(2, 1, 4, 3, 6, 4)]
        data
    }
)) %>% unnest() %>%
mutate(variable = factor(variable, levels = c("True", "mod_3",
       "mod_2", "mod_4", "mod_3_up", "mod_2_up", "mod_4_up")
)) %>%
mutate(variable = recode(variable,
    "True" = "Observed",
    "mod_3" = "No intervention",
    "mod_2" = "Social distancing",
    "mod_4" = "Lockdown with moderate release",
    "mod_3_up" = "No intervention upper credible interval",
    "mod_2_up" = "Social distancing upper credible interval",
    "mod_4_up" = "Lockdown upper credible interval")
) %>%
mutate(
    Date.fmt = format(Dates, format("%b %e")),
    Val.fmt = format(value, big.mark = ",", scientific = F, trim = T),
    Ref.fmt = format(ref, big.mark = ",", scientific = F, trim = T)
) %>%
mutate(text = case_when(
    variable == "Observed" ~
        paste0(Date.fmt,": ", Val.fmt, " observed cases"),
    variable == "No intervention" ~
        paste0(paste0(Date.fmt, ": ", Val.fmt, " projected cases"),
               paste0("<br>Projection upper CI: ", Ref.fmt, " cases<br>")),
    variable == "No intervention upper credible interval" ~
        paste0(paste0(Date.fmt, ": ", Ref.fmt, " projected cases"),
        paste0("<br>Projection upper CI: ", Val.fmt, " cases<br>")),
    variable == "Social distancing" ~
        paste0(paste0(Date.fmt, ": ", Val.fmt, " projected cases"),
               paste0("<br>Projection upper CI: ", Ref.fmt, " cases<br>")),
    variable == "Social distancing upper credible interval" ~
        paste0(paste0(Date.fmt, ": ", Ref.fmt, " projected cases"),
        paste0("<br>Projection upper CI: ", Val.fmt, " cases<br>")),
    variable == "Lockdown with moderate release" ~
        paste0(paste0(Date.fmt, ": ", Val.fmt, " projected cases"),
               paste0("<br>Projection upper CI: ", Ref.fmt, " cases<br>")),
    variable == "Lockdown upper credible interval" ~
        paste0(paste0(Date.fmt, ": ", Ref.fmt, " projected cases"),
        paste0("<br>Projection upper CI: ", Val.fmt, " cases<br>"))
)) %>%
mutate(
    i = variable %in% c("Observed", "No intervention", "Social distancing", "Lockdown with moderate release")
) %>%
filter(Dates <= "2020-04-30")

title <- paste("Cumulative number of COVID-19 cases in India compared",
               "to other countries affected by the pandemic")

cap <- paste0("© COV-IND-19 Study Group. Last updated: ",
              format(latest, format = "%b %e"), sep = ' ')

axis.title.font <- list(size = 16)
tickfont        <- list(size = 16)

xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
              tickangle = -30, showline = T, zeroline = T,
              x0 = format(latest, format = "%b %e"))

yaxis <- list(title = "Cumulative number of cases", type = "log",
              dtick = 1, titlefont = axis.title.font, zeroline = T, showline =T)


colors <- c("#979799", "#ED553B", "#f2c82e", "#173F5F", "#ED553B", "#f2c82e", "#173F5F")
p <- plot_ly(data %>% filter(i), x = ~ Dates, y = ~ value, text = ~text,
             color = ~variable, colors = colors, type = "bar",
             hoverinfo = "text", hoverlabel = list(align = "left")
) %>%
layout(barmode = "overlay", xaxis = xaxis, yaxis = yaxis,
       title = list(text = cap, xanchor = "left", x = 0),
       legend = list(orientation = "h", font = list(size = 16))
) %>% add_trace(data = data %>% filter(!i), x = ~Dates, y = ~value,
          type = "scatter", mode = "line", line =
          list(width = 3, dash = "dash"), visible = "legendonly"
)

saveRDS(p, paste0("~/cov-ind-19-data/", latest, "/plot4a.RDS"))

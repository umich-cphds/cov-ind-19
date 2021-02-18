plot_fig_9 <- function(start.date = as.Date("2020-04-01"))
{
    data <- vroom(paste0(data_repo, today, "/global_testing.csv"), col_types = cols()) %>%
    select(location, date, total_cases_per_million, total_tests_per_thousand) %>%
    group_by(location) %>%
    filter(!is.na(total_tests_per_thousand)) %>%
    filter(date == max(date)) %>%
    drop_na() %>%
    mutate(Text = paste0(location, "<br>Total cases per million: ",
        format(round(total_cases_per_million), big.mark = ",", sci = F, trim = T),
        "<br>Total tests per thousand: ",
        format(round(total_tests_per_thousand), big.mark = ",",
        sci = F, trim = T))
    )

    lmpred = predict(lm(total_tests_per_thousand~total_cases_per_million, data = data),
                        newdata = data.frame(total_cases_per_million = data$total_cases_per_million))
    data$lmpred = lmpred  #= data.frame(lmfit = lmfit, x = data$total_cases_per_million)
    cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(today, format = "%b %e"))

    title <- paste("Testing pattern across countries")

    axis.title.font <- list(size = 16)
    tickfont        <- list(size = 16)

    yaxis <- list(title = "Total positive tests per million", titlefont = axis.title.font,
                  tickfont = tickfont, zeroline = T)

    xaxis <- list(title = "Total tests per thousand", titlefont = axis.title.font,
                  showticklabels = TRUE)

    line_fmt <- list(dash = "solid", width = 1.5, color = "black")

    p = plot_ly(data, type = "scatter", color = ~location, y = ~total_cases_per_million, x = ~total_tests_per_thousand,
                mode = "line", hoverinfo = "text", hoverlabel = list(align = "left"), text = ~Text
        ) %>%
        add_fun(
        function(p) {
          p %>%
            add_segments(data = (data %>% arrange(desc(lmpred)))[c(1,nrow(data)),],
                         y = ~total_cases_per_million[1], yend = ~total_cases_per_million[2],
                         x = ~lmpred[1],  xend = ~lmpred[2],
                         type = "scatter",
                         showlegend = F, line = list(width = 1, color = 'gray'))
        }
      ) %>% layout(xaxis = xaxis, yaxis = yaxis, title = list(text = cap, xanchor = "left", x = 0)) %>%
      plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

  p
}

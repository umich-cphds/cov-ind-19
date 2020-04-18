testing <- tabPanel("Testing",
                     fluidRow(column(width = 12, top_matter)),
                     fluidRow(column(width = 1),
                              column(width = 10,
                                     h4("(Please wait a few seconds for the figures to load)"),
                                     h4(textOutput("latest")),
                                     h2(""),
                                     h2("Daily number of new COVID-19 cases, fatalities and recovered in India"),
                                     p("This figure provides the number of COVID-19 new cases (yellow),
        fatalities (red), and recovered cases (green) in India. You can
        hover your cursor over the bar to see the exact numerical counts."
                                     ),
                                     plotlyOutput("India_p8", height = "600px"),
                                     hr(),
                                     h2("Total number of COVID-19 cases and deaths"),
                                     p("The first figure represents COVID-19 case counts where the x-axis
        starts on the day when each country passed 100 cases. The second
        figure represents COVID-19 fatalities where the x-axis starts on the
        day when each country exceeded 3 fatalities. These axes allow
        comparison of counts at similar stages of the outbreak. You can click
        on countries in the legend to add or remove them and you can hover
        your cursor over the lines to see the exact numerical counts."
                                     ),
                                     #plotlyOutput("India_p2", height = "800px"),
                                     hr()
                              ),
                              column(width = 1)
                     )
)
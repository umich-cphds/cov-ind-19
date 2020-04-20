testing <- tabPanel("Testing",
                     fluidRow(column(width = 12, top_matter)),
                     fluidRow(column(width = 1),
                              column(width = 10,
                                     h4("(Please wait a few seconds for the figures to load)"),
                                     h4(textOutput("latest")),
                                     h2(""),
                                     h2("Daily confirmed positive cases and total tests in India"),
                                     p("This figure provides the number of COVID-19 cases (yellow) and positive tests (red)."),
                                     plotlyOutput("India_p8", height = "600px"),
                                     hr(),
                                     h2(""),
                                     h2("Testing pattern across countries"),
                                     p("This figure provides the pattern of testing across countries using cumulative tests
                                       per thousand versus cumulative confirmed cases per million."),
                                     plotlyOutput("India_p9", height = "600px"),
                                     hr()
                              ),
                              column(width = 1)
                     )
)
forecast <- tabPanel("National Forecast",
  fluidRow(
    column(width = 1),
    column(width = 10,
    h2("Short-term forecast"),
    p("In the following Figures we consider various scenarios of intervention
      effects to assess the effect of the lockdown.", "These figures should
      not be overinterpreted as in reality we do not know how the lockdown
      will actually reduce the transmission probability in India and to what
      extent.", "We use the eSIR model (", a("Wang et al. 2020", .noWS =
      "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"),
      ") for all our projections and can create hypothetical reductions in
      transmission probabilities capturing interventions like social
      distancing and lockdown. This in turn reduces the basic reproduction
      number over the period.", "The bar plot below shows 
      the predicted cumulative short-term case counts under the assumption that there is no intervention.
      All models assume a basic reproduction number of 2 under no intervention."),
      HTML(paste0("You can hover of the bars for dates and counts. Also, please note the dotted line represents the upper confidence interval.",
        "Our codes are available on ", a("GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19") ," and so users can change the nature of interventions.")
    ),
    h2("Short-term forecast"),
    h3("Figure 4. eSIR (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
    plotlyOutput("India_p4", height = "600px"),
#    h3("SEIR"),
#    plotlyOutput("India_pSEIR", height = "800px"),
    # hr(),
    # h2("Longer term forecasts post-lockdown"),
    # p("We again present predict case counts under the assumption that there is no intervention."),
    # h3("Figure 5a"),
    # plotlyOutput("India_p5a", height = "600px"),
    #h3("Figure 5b"),
    #plotlyOutput("India_p5b", height = "600px")
    hr(),
    #h3('Figure 12a'),
    #h4('Long-term projected daily number of cases by state in India until September 15'),
    #plotOutput("India_p12a", height = "800px"),
    #downloadLink('downloadFacet_inc_projection', 'Download'),
    #h3('Figure 12b'),
    #h4('Long-term projected cumulative number of cases by state in India until September 15'),
    #plotOutput("India_p12b", height = "800px"),
    #downloadLink('downloadFacet_cumul_projection', 'Download'),
    #hr()
    h3("Time-varying R"),
    h4("Effective basic reproduction number"),
    plotlyOutput("India_ptvr", height = '600px'),
    hr(),
    #h3("Doubling time"),
    #h4("Each point represents the estimated time for cases to double based on relative growth observed in the trailing 7 days."),
    #plotlyOutput("India_pdbl", height = '600px'),
    #hr()
    h2("Temporal testing pattern in India"),
    plotlyOutput("India_p15", height = "500px"),
  )),
  column(width = 1)
)

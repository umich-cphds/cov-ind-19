forecast <- tabPanel("National Forecast",
  fluidRow(
    column(width = 1),
    column(width = 10,
    h2("Short-term impact of social distancing, travel ban, and lockdown"),
    p("In the following Figures we consider various scenarios of intervention
      effects to assess the effect of the lockdown.", "These figures should
      not be overinterpreted as in reality we do not know how the lockdown
      will actually reduce the transmission probability in India and to what
      extent.", "We use the eSIR model (", a("Wang et al. 2020", .noWS =
      "outside", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1.full.pdf"),
      ") for all our projections and create hypothetical reductions in
      transmission probabilities capturing interventions like social
      distancing and lockdown. This in turn reduces the basic reproduction
      number over the period.", "The bar plots below
      the predicted cumulative short-term case counts represent three
      hypothetical scenarios: ",
      tags$ol(
        tags$li("No intervention"),
        tags$li("Social distancing and travel ban (without lockdown)"),
        tags$li("Lockdown with a gradual, moderate resumption of daily activities")
      ),
      "Because we are using SIR models to generate the forecast, we
      explicitly delay changes in the basic reproduction number one week to capture delayed onset of
      cases due to incubation period.",
      HTML(paste0("All models assume a basic reproduction number of 2 under
        no intervention. The implied R", tags$sub("0"), " is 1.5 under Scenario 2.")),
      HTML(paste0("We further assume the R", tags$sub("0"), " drops to 0.8 under lockdown and then gradually rises back up to 1.5 after the lockdown ends over a three week period ('lockdown with moderate return').")),
        "You can hover of the bars for dates and counts. Also, please note the dotted line represents the upper confidence interval for the lockdown scenario (3), which is closest to the current intervention.",
        "Our codes are available on ", a("GitHub", .noWS = "outside", href = "https://github.com/umich-cphds/cov-ind-19") ," and so users can change the nature of interventions."
    ),
    h2("Short-term forecast"),
    h3("Figure 4 (please note that the y-axis is in log base-10 scale; hover over the bars for count estimates and upper credible limits)"),
    plotlyOutput("India_p4", height = "600px"),
    hr(),
    h2("Longer term forecasts post-lockdown"),
    p("We present four hypothetical scenarios:",
      tags$ul(
        tags$li(HTML(paste0("Perpetual social distancing and travel ban (no lockdown; represented in yellow): R", tags$sub("0"), " remains 1.5 over the entire interval."))),
        tags$li(HTML(paste0("Post-lockdown activities return to normal activities prior to any intervention ('normal (pre-intervention)'; light blue): R", tags$sub("0"), " returns to 2 three weeks after the lockdown ends."))),
        tags$li(HTML(paste0("Post-lockdown activities gradually return to a moderate level ('moderate return'; blue): R", tags$sub("0"), " returns to 1.5 three weeks after the lockdown ends."))),
        tags$li(HTML(paste0("Post-lockdown activities return to a subdued level ('cautious return'; dark blue): R", tags$sub("0"), " returns to 1.2 three weeks after the lockdown ends.")))
      ),
    h3("Figure 5a"),
    plotlyOutput("India_p5a", height = "600px"),
    h3("Figure 5b"),
    plotlyOutput("India_p5b", height = "600px")
    ),
    hr(),
    h3('Figure 12a'),
    h4('Long-term projected daily number of cases by state in India until September 15'),
    plotOutput("India_p12a", height = "800px"),
    downloadLink('downloadFacet_inc_projection', 'Download'),
    h3('Figure 12b'),
    h4('Long-term projected cumulative number of cases by state in India until September 15'),
    plotOutput("India_p12b", height = "800px"),
    downloadLink('downloadFacet_cumul_projection', 'Download'),
    hr()
  ),
  column(width = 1)
))
